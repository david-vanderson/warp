#lang racket/base

(require "defs.rkt"
         "utils.rkt"
         "plasma.rkt"
         "shield.rkt"
         "effect.rkt")

(provide (all-defined-out))


;; server and client

(define (opposite-sign? a b)
  (if (positive? a)
      (negative? b)
      (positive? b)))


(define (steer! ownship dt)
  (define course (pilot-course (ship-pilot ownship)))
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
  (define acc? #f)
  (when (pilot-fore (ship-pilot ownship))
    (set! acc? #t)
    (set! ddx (* 20 (cos (posvel-r posvel))))
    (set! ddy (* 20 (sin (posvel-r posvel)))))
  
  (set-posvel-dx! posvel (+ (posvel-dx posvel) (* ddx dt)))
  (set-posvel-dy! posvel (+ (posvel-dy posvel) (* ddy dt)))
  
  (values acc? (not (= 0 ddr))))


(define (drag dv dt coef epsilon)
  (define newv (* dv (expt (1 . - . coef) dt)))
  (if ((abs newv) . < . epsilon) 0 newv))


(define (physics! pv dt drag? (acc? #f) (accr? #f))
  (set-posvel-x! pv (+ (posvel-x pv) (* dt (posvel-dx pv))))
  (set-posvel-y! pv (+ (posvel-y pv) (* dt (posvel-dy pv))))
  (set-posvel-r! pv (angle-add (posvel-r pv) (* dt (posvel-dr pv))))
  (when drag?
    (when (not (= 0 (posvel-dx pv)))
      (define dtheta (atan (posvel-dy pv) (posvel-dx pv)))
      (define drad (sqrt (+ (* (posvel-dy pv) (posvel-dy pv))
                            (* (posvel-dx pv) (posvel-dx pv)))))
      
      ; angle of motion with respect to course
      (define thetadiff (angle-sub (posvel-r pv) dtheta))
      (define d-along (drag (* drad (cos thetadiff)) dt .3 (if acc? 0 .1)))
      (define d-across (drag (* drad (sin thetadiff)) dt .9 .1))
      
      (cond ((= 0 d-along d-across)
             (set-posvel-dx! pv 0)
             (set-posvel-dy! pv 0))
            (else
             (define newtheta (angle-add (atan d-across d-along) (posvel-r pv)))
             (define newrad (sqrt (+ (* d-along d-along) (* d-across d-across))))
             
             (set-posvel-dx! pv (* newrad (cos newtheta)))
             (set-posvel-dy! pv (* newrad (sin newtheta))))))
    (set-posvel-dr! pv (drag (posvel-dr pv) dt R_DRAG_COEF (if accr? 0 (/ 2pi 360))))))


(define (update-physics! space o dt)
  (cond
    ((ship? o)
     (define-values (acc? accr?) (steer! o dt))
     (physics! (obj-posvel o) dt #t acc? accr?))
    ((plasma? o)
     (physics! (obj-posvel o) dt #f)
     (when (plasma-dead? space o)
       (set-space-objects! space (remove o (space-objects space)))))
    ((shield? o)
     (physics! (obj-posvel o) dt #t)
     (when (shield-dead? space o)
       (set-space-objects! space (remove o (space-objects space)))))
    ((effect? o)
     (physics! (obj-posvel o) dt #f)
     (when (effect-dead? space o)
       (set-space-objects! space (remove o (space-objects space)))))))


; return list of changes
(define (damage-object! space o damage)
  (define changes '())
  (cond ((plasma? o) (reduce-plasma! space o damage))
        ((shield? o) (reduce-shield! space o damage))
        ((ship? o)
         (set! changes (append changes (reduce-reactor! space o damage)))))
  (append changes (list (chdam (ob-id o) damage))))


; return list of changes
(define (reduce-reactor! space ship damage)
  (define changes '())
  (set-ship-containment! ship (- (ship-containment ship) damage))
  (when ((ship-containment ship) . <= . 0)
    (set-space-objects! space (remove ship (space-objects space)))
    (define pv (obj-posvel ship))
    (define e (effect (next-id) (space-time space) (struct-copy posvel pv) 45 1000))
    (set! changes (cons (chadd e) changes))
    (for ((i 21))
      (define t (random-between 0 2pi))
      (define s (random-between 80 120))
      (define p (plasma (next-id) (space-time space)
                        (posvel (space-time space) (posvel-x pv) (posvel-y pv) 0
                                (+ (* s (cos t)) (posvel-dx pv))
                                (+ (* s (sin t)) (posvel-dy pv))
                                0)
                        10.0 #f))
      (set-space-objects! space (cons p (space-objects space)))
      (set! changes (cons (chadd p) changes))))
  changes)


;; server only

; return a list of changes
(define (plasma-hit-ship! space ship p)
  (define changes '())
  (when (and (not ((ship-containment ship) . <= . 0))
             (not (plasma-dead? space p)))
    (when (and (not (equal? (plasma-ownship-id p) (ob-id ship)))
               ((distance ship p) . < . (+ 10 (plasma-radius space p))))
      ;(printf "plasma hit ship ~a (~a ~a)\n" (ship-name ship) (plasma-ownship-id p) (obj-id ship))
      (define damage (plasma-energy space p))
      (set! changes (append changes (damage-object! space p damage)))
      (set! changes (append changes (damage-object! space ship damage)))
      (define e (effect (next-id) (space-time space)
                        (struct-copy posvel (obj-posvel p)
                                     (dx (posvel-dx (obj-posvel ship)))
                                     (dy (posvel-dy (obj-posvel ship))))
                        6 300))
      (set! changes (cons (chadd e) changes))))
  changes)


; return a list of changes
(define (plasma-hit-shield! space shield p)
  (define changes '())
  (when (and (not (shield-dead? space shield))
             (not (plasma-dead? space p)))
    (define r (posvel-r (obj-posvel shield)))
    (define-values (px py) (recenter shield p))
    (define x (+ (* px (cos r)) (* py (sin r))))
    (define y (+ (* -1 px (sin r)) (* py (cos r))))
    ; x,y are now the position of the plasma in the coord space of the shield
    ; shield is along the y axis
    (define rad (plasma-radius space p))
    (define l (shield-length shield))
    (when (and (< (- rad) x rad)
               (< (- (- (/ l 2)) rad) y (+ (/ l 2) rad)))
      (define damage (plasma-energy space p))
      (set! changes (append changes (damage-object! space p damage)))
      (set! changes (append changes (damage-object! space shield damage)))))
  changes)


; return a list of changes
(define (update-effects! space)
  (define changes '())
  (define objects (space-objects space))
  (define ships (filter ship? objects))
  (define plasmas (filter plasma? objects))
  (define shields (filter shield? objects))
  (for ((p plasmas))
    (for ((shield shields))
      (set! changes (append changes (plasma-hit-shield! space shield p))))
    (for ((ship ships))
      (set! changes (append changes (plasma-hit-ship! space ship p)))))
  changes)
