#lang racket/base

(require racket/tcp
         racket/math)

(require "defs.rkt"
         "physics.rkt")

(provide start-server)

(define server-listener #f)
(define client-in-ports '())
(define client-out-ports '())

(define SERVER_LOOP_DELAY .03)  ; don't loop more often than X secs
(define SERVER_SEND_DELAY .25)  ; don't send updates more often than X secs
(define ownspace #f)
(define new-client #f)


(define (receive-command cmd)
  (printf "receive-command\n")
  (cond
    ((player? cmd)
     (new-client ownspace cmd))
    ((plasma? cmd)  ; add a plasma
     (set-space-objects! ownspace (append (space-objects ownspace) (list cmd))))
    ((role? cmd)
     ; find the ship that this role is on
     (define stack (find-player ownspace (player-id (role-player cmd))))
     (define ownship (caddr (reverse stack)))
     (cond ((helm? cmd)
            (when (helm-aft cmd)
              (set-helm-aft! cmd #f)
              (define rx (cos (thing-r ownship)))
              (define ry (sin (thing-r ownship)))
              (define x (+ (thing-x ownship) (* 20 rx)))
              (define y (+ (thing-y ownship) (* 20 ry)))
              (define p (plasma x y 0
                                (+ (thing-dx ownship) (* 30 rx))
                                (+ (thing-dy ownship) (* 30 ry))
                                0 "blue" 10.0 (ship-id ownship) '()))
              (set-space-objects! ownspace (list* p (space-objects ownspace)))
              )
            (set-ship-helm! ownship cmd))))))


(define previous-loop-time (current-inexact-milliseconds))
(define previous-send-time (current-inexact-milliseconds))

(define (server-loop)
  (define current-time (current-inexact-milliseconds))
  (define dt (/ (- current-time previous-loop-time) 1000))
  (set! previous-loop-time current-time)
  (define need-update #f)
  
  ; physics
  ;(printf "server physics:\n")
  ;(vector-set! (shield-sections (car (ship-shields (car (space-objects ownspace))))) 0 100)
  (update-physics! ownspace dt)
  (update-effects! ownspace)
  
  ; process new clients
  (when (tcp-accept-ready? server-listener)
    (printf "server accept-ready\n")
    (define-values (in out) (tcp-accept server-listener))
    (set! client-in-ports (cons in client-in-ports))
    (set! client-out-ports (cons out client-out-ports))
    (set! need-update #t))
  
  ; process commands
  (for ((p client-in-ports))
    (when (byte-ready? p)
      (define x (read p))
      (receive-command x)
      (set! need-update #t)))
  
  ; send out updated world
  (when (or need-update
            (> (/ (- current-time previous-send-time) 1000) SERVER_SEND_DELAY))
    (set! previous-send-time current-time)
    (for ((p client-out-ports))
      ;(printf "server sending ~v\n" ownspace)
      (write ownspace p)
      (flush-output p)))
  
  ; sleep so we don't hog the whole racket vm
  (define loop-time (/ (- (current-inexact-milliseconds) current-time) 1000))
  (if (loop-time . < . SERVER_LOOP_DELAY)
      (sleep (- SERVER_LOOP_DELAY loop-time))
      (begin (printf "SERVER LOOP TOO LONG: ~a\n" loop-time)))
  
  (server-loop))

(define (start-server port new-space new-client-arg)
  (set! ownspace new-space)
  (set! new-client new-client-arg)
  (set! server-listener (tcp-listen port))
  (server-loop))

(module+ main
  (define (big-ship x y)
    (ship x y (* 0.5 pi) 0 0 0
          (random-id)
          (helm #f (* 0.5 pi) #f #f #f #f)
          100 1
          (list 
           (shield 57 "blue" 100 (make-vector 16 50))
           (shield 50 "red" 100 (make-vector 16 50)))))
  
  (define ownspace (space (list (big-ship 0 0) (big-ship 100 0))))
  
  (define (new-client ownspace player)
    (printf "new-client ~a\n" (player-id player))
    (define ship (car (space-objects ownspace)))
    (set-role-player! (ship-helm ship) player))
  
  (start-server PORT ownspace new-client))
