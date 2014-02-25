#lang racket/base

(require racket/math)

(require "defs.rkt"
         "utils.rkt"
         "plasma.rkt"
         "shield.rkt"
         "effect.rkt")

(provide (all-defined-out))


(define (opposite-sign? a b)
  (if (positive? a)
      (negative? b)
      (positive? b)))


(define (steer! ownship dt)
  (define h (ship-helm ownship))
  (define p (pod-role h))
  (define course (pilot-course p))
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
           (define racc (/ (stats-rthrust (ship-stats ownship))
                        (stats-mass (ship-stats ownship))))
           (define full_racc (if (positive? diff) racc (- racc)))
           
           (cond ((opposite-sign? diff (posvel-dr posvel)) full_racc)  ; going the wrong way, full acc
                 ((opposite-sign? diff future-diff)
                  ; getting close, accelerate future-diff less
                  ;(printf "getting close, ~a ~a\n" diff future-diff)
                  (- (* 0.5 full_racc) (* -3 future-diff)))
                 (else full_racc)))))  ; not close, keep at it
   
  (define racc? #f)
  (define acc? #f)
  
  (when (not (= 0 ddr))
    (set! racc? #t)
    (when ((pod-energy h) . > . 0)
      (set-posvel-dr! posvel (+ (posvel-dr posvel) (* ddr dt)))))
    
  (when (pilot-fore (ship-pilot ownship))
    (set! acc? #t)
    (when ((pod-energy h) . > . 0)
      (define xy_acc (/ (stats-thrust (ship-stats ownship))
                        (stats-mass (ship-stats ownship))))
      (define ddx (* xy_acc (cos (posvel-r posvel))))
      (define ddy (* xy_acc (sin (posvel-r posvel))))
      (set-posvel-dx! posvel (+ (posvel-dx posvel) (* ddx dt)))
      (set-posvel-dy! posvel (+ (posvel-dy posvel) (* ddy dt)))))
  
  (values acc? racc?))


(define (drag dv dt coef epsilon)
  (define newv (* dv (expt (1 . - . coef) dt)))
  (if ((abs newv) . < . epsilon) 0 newv))


(define (physics! pv dt (drag_xy #f) (drag_r #f) (acc? #f) (racc? #f))
  (set-posvel-x! pv (+ (posvel-x pv) (* dt (posvel-dx pv))))
  (set-posvel-y! pv (+ (posvel-y pv) (* dt (posvel-dy pv))))
  (set-posvel-r! pv (angle-add (posvel-r pv) (* dt (posvel-dr pv))))
  (when drag_xy
    (set-posvel-dx! pv (drag (posvel-dx pv) dt drag_xy (if acc? 0 .1)))
    (set-posvel-dy! pv (drag (posvel-dy pv) dt drag_xy (if acc? 0 .1))))
  (when drag_r
    (set-posvel-dr! pv (drag (posvel-dr pv) dt drag_r (if racc? 0 (/ 2pi 360))))))


(define (update-physics! space o dt)
  (cond
    ((ship? o)
     (define-values (acc? racc?) (steer! o dt))
     (physics! (obj-posvel o) dt 0.5 0.7 acc? racc?))
    ((plasma? o)
     (physics! (obj-posvel o) dt)
     (when (plasma-dead? space o)
       (set-space-objects! space (remove o (space-objects space)))))
    ((shield? o)
     (physics! (obj-posvel o) dt 0.5)
     (when (shield-dead? space o)
       (set-space-objects! space (remove o (space-objects space)))))
    ((effect? o)
     (physics! (obj-posvel o) dt)
     (when (effect-dead? space o)
       (set-space-objects! space (remove o (space-objects space)))))))


; return list of additional changes
(define (damage-object! space o damage)
  (cond ((plasma? o) (reduce-plasma! space o damage) '())
        ((shield? o) (reduce-shield! space o damage) '())
        ((ship? o) (reduce-ship! space o damage))))


; return list of additional changes
(define (reduce-ship! space ship damage)
  (define changes '())
  (set-stats-containment! (ship-stats ship) (- (ship-containment ship) damage))
  (when ((ship-containment ship) . <= . 0)
    (set-space-objects! space (remove ship (space-objects space)))
    (define pv (obj-posvel ship))
    (define e (effect (next-id) (space-time space) (struct-copy posvel pv) 45 1000))
    (set! changes (append changes (list (chadd e))))
    (for ((i 41))
      (define t (random-between 0 2pi))
      (define s (random-between 80 120))
      (define p (plasma (next-id) (space-time space)
                        (posvel (space-time space) (posvel-x pv) (posvel-y pv) 0
                                (+ (* s (cos t)) (posvel-dx pv))
                                (+ (* s (sin t)) (posvel-dy pv))
                                0)
                        10.0 #f))
      (set! changes (append changes (list (chadd p))))))
  changes)


(define (update-energy! dt ship extra)
  
  ; remove energy for stateful things
  (define h (ship-helm ship))
  (when ((pod-energy h) . > . 0)
    (when (pilot-fore (pod-role h))
      (set-pod-energy! h (- (pod-energy h) (* 3.0 dt))))
    (when (not (= (pilot-course (pod-role h))
                  (posvel-r (obj-posvel ship))))
      (set-pod-energy! h (- (pod-energy h) (* 2.0 dt)))))
  
  ; distribute produced and extra energy
  (define pods (filter (lambda (p) (not (multipod? p))) (ship-pods ship)))
  (set! pods (sort pods < #:key (lambda (p) (max 0.0 (- MAX_POD_ENERGY (pod-energy p))))))
  (define e (+ 0.0 (* dt (stats-power (ship-stats ship))) extra))
  (while (not (null? pods))
    (define ef (/ e (length pods)))
    (define p (car pods))
    (define pod-empty (max 0.0 (- MAX_POD_ENERGY (max 0.0 (pod-energy p)))))
    (cond
      ((ef . <= . pod-empty)
       (set-pod-energy! p (+ (pod-energy p) ef))
       (set! e (- e ef)))
      (else
       (set! e (- e pod-empty))
       (set-pod-energy! p (+ (pod-energy p) pod-empty))))
    (set! pods (cdr pods)))
  
  (min extra e))

