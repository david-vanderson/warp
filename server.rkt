#lang racket/base

(require racket/math
         racket/async-channel
         racket/serialize)

(require "defs.rkt")

(provide command-channel
         update-physics
         start-server)

(define command-channel (make-async-channel))
(define client-channels '())

(define SERVER_LOOP_DELAY .1)  ; don't loop more often than X secs
(define SERVER_SEND_DELAY .25)  ; don't send updates more often than X secs

(define (add-angle r theta)
  (define z (+ r theta))
  (cond ((z . >= . 2pi) (- z 2pi))
        ((z . < . 0) (+ z 2pi))
        (else z)))

(define (angle-diff from to)
  (define diff (- to from))
  (cond (((abs diff) . <= . pi) diff)
        ((diff . > . pi) (- diff 2pi))
        (else (+ 2pi diff))))

(define (opposite-sign? a b)
  (if (positive? a)
      (negative? b)
      (positive? b)))

(define (update-physics ownship dt)
  
  ; steer the ship
  (define course (helm-course (ship-helm ownship)))
  (define r (object-r ownship))
  (define acc
    (cond ((and
            ((abs (angle-diff r course)) . < . (/ 2pi 180))  ; within 2 degrees
            ((abs (object-dr ownship)) . < . (/ 2pi 120)))  ; moving less than 3 degrees / sec
           ;(printf "STOPPING\n")
           (set-object-r! ownship course)
           (set-object-dr! ownship 0)
           0)  ; don't need to accelerate if we're already there
          (else
           (define future-r (add-angle r (object-dr ownship)))  ; we'll be there in 1 sec
           (define future-diff (angle-diff future-r course))
           (define diff (angle-diff r course))
           (define fullRACC (if (positive? diff) RACC (- RACC)))
           
           (cond ((opposite-sign? diff (object-dr ownship)) fullRACC)  ; going the wrong way, full acc
                 ((opposite-sign? diff future-diff)
                  ; getting close, accelerate future-diff less
                  ;(printf "getting close, ~a ~a\n" diff future-diff)
                  (- (* 0.5 fullRACC) (* -3 future-diff)))
                 (else fullRACC)))))  ; not close, keep at it
  
  ;(printf "r ~a, course ~a, acc ~a\n" r course acc)
  (set-object-dr! ownship (+ (object-dr ownship) (* acc dt)))
  
  ; physics
  (set-object-x! ownship (+ (object-x ownship) (* dt (object-dx ownship))))
  (set-object-y! ownship (+ (object-y ownship) (* dt (object-dy ownship))))
  (set-object-r! ownship (add-angle (object-r ownship) (* dt (object-dr ownship))))
  (set-object-dx! ownship (drag (object-dx ownship) dt DRAG_COEF .1))
  (set-object-dy! ownship (drag (object-dy ownship) dt DRAG_COEF .1))
  (set-object-dr! ownship (drag (object-dr ownship) dt R_DRAG_COEF (* (/ 2pi 360) (abs acc)))))


(define ownship (ship 0 0 (* 0.5 3 pi) 0 0 0
                      (helm #f (* 0.5 3 pi) #f #f #f #f)
                      (list (shield 100 "red" 100
                                    '(100 50 25 0 20 20 20 20 20 20 20 20 20 20 20 20))
                            (shield 107 "blue" 100
                                    '(100 50 25 0 20 20 20 20 20 20 20 20 20 20 20 20)))))

(define (receive-command cmd)
  (printf "receive-command\n")
  (cond
    ((async-channel? cmd)  ; new client
     (set! client-channels (list* cmd client-channels)))
    ((helm? cmd)
     ; find the ship that this role is on
     (set-ship-helm! ownship cmd)
     )))

(define (drag dv dt coef epsilon)
  (define newv (* dv (expt (1 . - . coef) dt)))
  (if ((abs newv) . < . epsilon) 0 newv))

(define previous-loop-time (current-inexact-milliseconds))
(define previous-send-time (current-inexact-milliseconds))

(define (start-server)
  (define current-time (current-inexact-milliseconds))
  (define dt (/ (- current-time previous-loop-time) 1000))
  (set! previous-loop-time current-time)
  (define need-update #f)
  
  ; physics
  ;(printf "server physics: ")
  (update-physics ownship dt)
  
  ; process commands
  (let loop ()
    (define command (async-channel-try-get command-channel))
    (when command
      (set! need-update #t)
      (receive-command command)
      (loop)))
  
  ; do collision detection and make all decisions
  
  
  ; send out updated world
  (when (or need-update
            (> (/ (- current-time previous-send-time) 1000) SERVER_SEND_DELAY))
    (set! previous-send-time current-time)
    ;    (printf "server sending\n")
    (for ((c client-channels))
      (async-channel-put c (serialize ownship))))
  
  ; sleep so we don't hog the whole racket vm
  (when (dt . < . SERVER_LOOP_DELAY)
    (sleep (- SERVER_LOOP_DELAY dt)))
  
  (start-server))
