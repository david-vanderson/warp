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
(define SERVER_SEND_DELAY 1.0)  ; don't send auto updates more often than X secs
(define ownspace #f)


;; Server Utilities

(define (copy-role r)
  (cond
    ((observer? r) (struct-copy observer r))
    ((crewer? r) (struct-copy crewer r))
    (else (error "copy-role hit ELSE clause, role:\n" r))))

(define (join-role! r p)
  ;(printf "player ~v joining role ~v\n" p r)
  (cond
    ((and (role? r) (not (role-player r)))
     (set-role-player! r p)
     #t)
    ((multirole? r)
     (define new-role (copy-role (multirole-role r)))
     (set-role-player! new-role p)
     (set-multirole-roles!
      r (cons new-role (multirole-roles r)))
     #t)
    ((not r)
     #t)  ; can always leave space
    (else #f)))

(define (leave-role! r p)
  ;(printf "player ~v leaving role ~v\n" p r)
  (cond
    ((role? r) (set-role-player! r #f))
    ((multirole? r) (set-multirole-roles!
                     r (filter (lambda (xr) (not (equal? (role-player xr) p)))
                               (multirole-roles r))))))


(define (receive-command cmd)
  ;(printf "receive-command ~v\n" cmd)
  (cond
    ((role-change? cmd)
     (define p (role-change-player cmd))
     (define from (find-id ownspace (role-change-from cmd)))
     (define to (find-id ownspace (role-change-to cmd)))
     (when (join-role! to p)
       (leave-role! from p)))
    ((role? cmd)
     ; find our role
     (define stack (find-stack ownspace (obj-id cmd)))
     (define ownrole (car stack))
     (cond
       ((and (role-player ownrole)
             (role-player cmd)
             (= (obj-id (role-player ownrole))
                (obj-id (role-player cmd))))
        ; we got a command from the correct player
        
        ; make instant changes
        (when (helm? cmd)
          (when (helm-aft cmd)
            (set-helm-aft! cmd #f)
            
            (define ownship (cadr stack))
            (define pv (obj-posvel ownship))
            (define rx (cos (posvel-r pv)))
            (define ry (sin (posvel-r pv)))
            (define x (+ (posvel-x pv) (* 20 rx)))
            (define y (+ (posvel-y pv) (* 20 ry)))
            (define p (plasma (next-id) (space-time ownspace)
                              (posvel x y 0
                                      (+ (posvel-dx pv) (* 30 rx))
                                      (+ (posvel-dy pv) (* 30 ry))
                                      0)
                              "blue" 10.0 (obj-id ownship) '()))
            (set-space-objects! ownspace (list* p (space-objects ownspace)))))
        
        ; make state changes
        ;(define-values (name, _ struct-type _) (struct-type-info ownrole))
        (when (helm? cmd)
          (set-helm-course! ownrole (helm-course cmd))
          (set-helm-fore! ownrole (helm-fore cmd))
          (set-helm-aft! ownrole (helm-aft cmd))
          (set-helm-left! ownrole (helm-left cmd))
          (set-helm-right! ownrole (helm-right cmd))))
       
       ((and (not (role-player ownrole))
             (role-player cmd))
        ; player is entering an empty role
        (printf "player ~a took role ~v\n" (role-player cmd) ownrole)
        (set-role-player! ownrole (role-player cmd)))))))




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
  ;(printf "~v\n" (car (space-objects ownspace)))
  (update-physics! ownspace dt)
  (set-space-time! ownspace (+ (space-time ownspace) dt))
  (update-effects! ownspace)
  
  ; process new clients
  (when (tcp-accept-ready? server-listener)
    (printf "server accept-ready\n")
    (define-values (in out) (tcp-accept server-listener))
    
    ; need to assign an id to the new player
    (write (player (next-id) #f #f "New Player" #f) out)
    
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
      ;(printf "server sending ~a\n" (space-time ownspace))
      (write ownspace p)
      (flush-output p)))
  
  ; sleep so we don't hog the whole racket vm
  (define loop-time (/ (- (current-inexact-milliseconds) current-time) 1000))
  (if (loop-time . < . SERVER_LOOP_DELAY)
      (sleep (- SERVER_LOOP_DELAY loop-time))
      (begin (printf "SERVER LOOP TOO LONG: ~a\n" loop-time)))
  
  (server-loop))

(define (start-server port new-space)
  (set! ownspace new-space)
  (set! server-listener (tcp-listen port))
  (server-loop))

(module+ main
  
  (define ownspace (space 0 2000 2000 (list (big-ship 0 0 "Ship1") (big-ship 100 0 "Ship2"))))
  
  (start-server PORT ownspace))
