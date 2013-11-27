#lang racket/base

(require racket/math)

(require "defs.rkt")

(provide (all-defined-out))


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

(define (drag dv dt coef epsilon)
  (define newv (* dv (expt (1 . - . coef) dt)))
  (if ((abs newv) . < . epsilon) 0 newv))


; This is used on both server and client (for prediction)
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