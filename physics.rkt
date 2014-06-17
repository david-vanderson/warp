#lang racket/base

(require racket/math)

(require "defs.rkt"
         "utils.rkt"
         "plasma.rkt"
         "shield.rkt"
         "effect.rkt"
         "ships.rkt")

(provide (all-defined-out))


(define (opposite-sign? a b)
  (if (positive? a)
      (negative? b)
      (positive? b)))


(define (steer! ownship dt)
  (define posvel (obj-posvel ownship))
  (define h (ship-helm ownship))
  
  (define p (pod-role h))
  (define course (pilot-course p))
  (define r (posvel-r posvel))
  
  (define racc (stats-rthrust (ship-stats ownship)))
  (cond
    (((abs (angle-diff r course)) . < . (* racc dt))
     ;(printf "STOPPING\n")
     (set-posvel-r! posvel course)
     (set-posvel-dr! posvel 0))
    (((pod-energy h) . > . 0)
     (set-posvel-dr! posvel (if ((angle-diff r course) . > . 0) racc (- racc)))))
  
  (define acc? #f)
  (when (pilot-fore (ship-pilot ownship))
    (set! acc? #t)
    (when ((pod-energy h) . > . 0)
      (define xy_acc (stats-thrust (ship-stats ownship)))
      (define ddx (* xy_acc (cos (posvel-r posvel))))
      (define ddy (* xy_acc (sin (posvel-r posvel))))
      (set-posvel-dx! posvel (+ (posvel-dx posvel) (* ddx dt)))
      (set-posvel-dy! posvel (+ (posvel-dy posvel) (* ddy dt)))))
  
  acc?)


(define (drag dv dt coef epsilon)
  (define newv (* dv (expt (1 . - . coef) dt)))
  (if ((abs newv) . < . epsilon) 0 newv))


(define (physics! pv dt (drag_xy #f) (acc? #f))
  (set-posvel-x! pv (+ (posvel-x pv) (* dt (posvel-dx pv))))
  (set-posvel-y! pv (+ (posvel-y pv) (* dt (posvel-dy pv))))
  (set-posvel-r! pv (angle-add (posvel-r pv) (* dt (posvel-dr pv))))
  (when drag_xy
    (set-posvel-dx! pv (drag (posvel-dx pv) dt drag_xy (if acc? 0 dt)))
    (set-posvel-dy! pv (drag (posvel-dy pv) dt drag_xy (if acc? 0 dt)))))


(define (update-physics! space o dt)
  (cond
    ((ship? o)
     (define acc? (if (ship-helm o) (steer! o dt) #f))
     (physics! (obj-posvel o) dt 0.4 acc?))
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
       (set-space-objects! space (remove o (space-objects space)))))
    ((message? o)
     (when ((obj-age space o) . > . MSG_FADE_TIME)
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
    
    (for ((ps (search ship player? #t)))
      (define p (car ps))
      (define ss (make-ship "space-suit"
                            (player-name p)
                            (ship-faction ship)
                            #:x (posvel-x pv) #:y (posvel-y pv)
                            #:dx (+ (posvel-dx pv) (random-between -50 50))
                            #:dy (+ (posvel-dx pv) (random-between -50 50))))
      (define rc (role-change p #f (ob-id (car (ship-pods ss))) (next-id)))
      (set! changes (append changes (list (chadd ss) rc))))
    
    (for ((i 21))
      (define t (random-between 0 2pi))
      (define s (random-between 10 50))
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
  (when (and h ((pod-energy h) . > . 0))
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

