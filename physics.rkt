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


(define (steer! ownship dt)
  (define course (helm-course (ship-helm ownship)))
  (define r (thing-r ownship))
  (define acc
    (cond ((and
            ((abs (angle-diff r course)) . < . (/ 2pi 180))  ; within 2 degrees
            ((abs (thing-dr ownship)) . < . (/ 2pi 120)))  ; moving less than 3 degrees / sec
           ;(printf "STOPPING\n")
           (set-thing-r! ownship course)
           (set-thing-dr! ownship 0)
           0)  ; don't need to accelerate if we're already there
          (else
           (define future-r (add-angle r (thing-dr ownship)))  ; we'll be there in 1 sec
           (define future-diff (angle-diff future-r course))
           (define diff (angle-diff r course))
           (define fullRACC (if (positive? diff) RACC (- RACC)))
           
           (cond ((opposite-sign? diff (thing-dr ownship)) fullRACC)  ; going the wrong way, full acc
                 ((opposite-sign? diff future-diff)
                  ; getting close, accelerate future-diff less
                  ;(printf "getting close, ~a ~a\n" diff future-diff)
                  (- (* 0.5 fullRACC) (* -3 future-diff)))
                 (else fullRACC)))))  ; not close, keep at it
  
  ;(printf "r ~a, course ~a, acc ~a\n" r course acc)
  (set-thing-dr! ownship (+ (thing-dr ownship) (* acc dt)))
  (values 0 0 acc))

(define (physics! thing dt drag? ddx ddy ddr)
  (set-thing-x! thing (+ (thing-x thing) (* dt (thing-dx thing))))
  (set-thing-y! thing (+ (thing-y thing) (* dt (thing-dy thing))))
  (set-thing-r! thing (add-angle (thing-r thing) (* dt (thing-dr thing))))
  (when drag?
    (set-thing-dx! thing (drag (thing-dx thing) dt DRAG_COEF (if (zero? ddx) .1 0)))
    (set-thing-dy! thing (drag (thing-dy thing) dt DRAG_COEF (if (zero? ddy) .1 0)))
    (set-thing-dr! thing (drag (thing-dr thing) dt R_DRAG_COEF (if (zero? ddr) (/ 2pi 360) 0)))))

; This is used on both server and client (for prediction)
(define (update-physics! ownspace dt)
  (for ((o (space-objects ownspace)))
    (cond
      ((ship? o)
       (define-values (ddx ddy ddr) (steer! o dt))
       (physics! o dt #t ddx ddy ddr))
      ((plasma? o)
       (physics! o dt #f 0 0 0)))))