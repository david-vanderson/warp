#lang racket/base

(require racket/math)

(require "defs.rkt"
         "plasma.rkt"
         "shield.rkt")

(provide (all-defined-out))

; gives angular distance and direction (-pi to pi)
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
  (define posvel (obj-posvel ownship))
  (define r (posvel-r posvel))
  (define ddr
    (cond ((and
            ((abs (angle-diff r course)) . < . (/ 2pi 180))  ; within 2 degrees
            ((abs (posvel-dr posvel)) . < . (/ 2pi 120)))  ; moving less than 3 degrees / sec
           ;(printf "STOPPING\n")
           (set-posvel-r! posvel course)
           (set-posvel-dr! posvel 0)
           0)  ; don't need to accelerate if we're already there
          (else
           (define future-r (angle-add r (posvel-dr posvel)))  ; we'll be there in 1 sec
           (define future-diff (angle-diff future-r course))
           (define diff (angle-diff r course))
           (define fullRACC (if (positive? diff) RACC (- RACC)))
           
           (cond ((opposite-sign? diff (posvel-dr posvel)) fullRACC)  ; going the wrong way, full acc
                 ((opposite-sign? diff future-diff)
                  ; getting close, accelerate future-diff less
                  ;(printf "getting close, ~a ~a\n" diff future-diff)
                  (- (* 0.5 fullRACC) (* -3 future-diff)))
                 (else fullRACC)))))  ; not close, keep at it
  
  ;(printf "r ~a, course ~a, acc ~a\n" r course acc)
  (set-posvel-dr! posvel (+ (posvel-dr posvel) (* ddr dt)))
  
  (define ddx 0)
  (define ddy 0)
  (when (helm-fore (ship-helm ownship))
    (set! ddx (* 20 (cos (posvel-r posvel))))
    (set! ddy (* 20 (sin (posvel-r posvel)))))
  
  (set-posvel-dx! posvel (+ (posvel-dx posvel) (* ddx dt)))
  (set-posvel-dy! posvel (+ (posvel-dy posvel) (* ddy dt)))
  
  (values ddx ddy ddr))


(define (physics! posvel dt drag? ddx ddy ddr)
  (set-posvel-x! posvel (+ (posvel-x posvel) (* dt (posvel-dx posvel))))
  (set-posvel-y! posvel (+ (posvel-y posvel) (* dt (posvel-dy posvel))))
  (set-posvel-r! posvel (angle-add (posvel-r posvel) (* dt (posvel-dr posvel))))
  (when drag?
    (set-posvel-dx! posvel (drag (posvel-dx posvel) dt DRAG_COEF (if (zero? ddx) .1 0)))
    (set-posvel-dy! posvel (drag (posvel-dy posvel) dt DRAG_COEF (if (zero? ddy) .1 0)))
    (set-posvel-dr! posvel (drag (posvel-dr posvel) dt R_DRAG_COEF (if (zero? ddr) (/ 2pi 360) 0)))))


; This is used on both server and client (for prediction)
(define (update-physics! ownspace dt)
  (for ((o (space-objects ownspace)))
    (cond
      ((ship? o)
       (define-values (ddx ddy ddr) (steer! o dt))
       (physics! (obj-posvel o) dt #t ddx ddy ddr)
       )
      ((plasma? o)
       (physics! (obj-posvel o) dt #f 0 0 0)
       (when ((- (space-time ownspace) (obj-start-time o)) . > . 5000)
         (reduce-plasma! ownspace o (* dt 10/5))))
      ((shield? o)
       (physics! (obj-posvel o) dt #t 0 0 0)
       (when ((- (space-time ownspace) (obj-start-time o)) . > . 10000)
         (reduce-shield! ownspace o (* dt 20/10)))))))


(define (distance o1 o2)
  (define dx (- (posvel-x (obj-posvel o1)) (posvel-x (obj-posvel o2))))
  (define dy (- (posvel-y (obj-posvel o1)) (posvel-y (obj-posvel o2))))
  (sqrt (+ (* dx dx) (* dy dy))))


(define (theta from to)
  (define dx (- (posvel-x (obj-posvel to)) (posvel-x (obj-posvel from))))
  (define dy (- (posvel-y (obj-posvel to)) (posvel-y (obj-posvel from))))
  ;(printf "dx ~a, dy ~a\n" dx dy)
  (atan dy dx))


(define (plasma-hit-ship! space ship p)
  (when (and (not (equal? (plasma-ownship-id p) (obj-id ship)))
             ((distance ship p) . < . (+ 10 (plasma-radius p))))
    (define damage (plasma-energy p))
    (reduce-plasma! space p damage)
    (reduce-reactor! space ship damage)))


(define (plasma-hit-shield! space shield p)
  (define r (posvel-r (obj-posvel shield)))
  (define-values (px py) (recenter shield p))
  (define x (+ (* px (cos r)) (* py (sin r))))
  (define y (+ (* -1 px (sin r)) (* py (cos r))))
  ; x,y are now the position of the plasma in the coord space of the shield
  ; shield is along the y axis
  (define rad (plasma-radius p))
  (define l (shield-length shield))
  (when (and (< (- rad) x rad)
             (< (- (- (/ l 2)) rad) y (+ (/ l 2) rad)))
    (define damage (plasma-energy p))
    (reduce-plasma! space p damage)
    (reduce-shield! space shield damage)))


(define (reduce-reactor! space ship damage)
  (set-ship-containment! ship (- (ship-containment ship) damage))
  (when ((ship-containment ship) . <= . 0)
    (set-space-objects! space (remove ship (space-objects space)))))


(define (update-effects! space)
  (define objects (space-objects space))
  (define ships (filter ship? objects))
  (define plasmas (filter plasma? objects))
  (define shields (filter shield? objects))
  (for ((p plasmas))
    (for ((shield shields))
      (plasma-hit-shield! space shield p))
    (when (not (plasma-dead? p))
      (for ((ship ships))
        (plasma-hit-ship! space ship p)))))
