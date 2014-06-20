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
  (set-stats-con! (ship-stats ship) (- (ship-con ship) damage))
  (when ((ship-con ship) . <= . 0)
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
      (set! changes (append changes (list (chadd p)))))
    
    (define msg (message (next-id) (space-time space) #f
                         (format "~a Destroyed" (ship-name ship))))
    (set! changes (append changes (list msg))))
  changes)


(define (update-energy! dt ship extra)
  
  ; remove energy for stateful things
  (define h (ship-helm ship))
  (when (and h (ship-flying? ship) ((pod-energy h) . > . 0))
    (when (pilot-fore (pod-role h))
      (set-pod-energy! h (- (pod-energy h) (* 3.0 dt))))
    (when (not (= (pilot-course (pod-role h))
                  (posvel-r (obj-posvel ship))))
      (set-pod-energy! h (- (pod-energy h) (* 2.0 dt)))))
  
  ; distribute produced and extra energy
  (define pods (filter (lambda (p) (not (multipod? p))) (ship-pods ship)))
  (define suckers (append pods (ship-ships ship)))
  (define (pod-need s) (max 0.0 (- MAX_POD_ENERGY (max 0.0 (pod-energy s)))))
  (define (ship-need s) (min (* 10.0 dt) (max 0.0 (- (ship-maxcon s) (ship-con s)))))
  (set! suckers (sort suckers <
                      #:key (lambda (s)
                              (cond ((pod? s) (pod-need s))
                                    ((ship? s) (ship-need s))))))
  
  ; take out battery energy
  (define batpow (* 5.0 dt))
  (define bate (min batpow (ship-bat ship)))
  (set-stats-bat! (ship-stats ship) (- (ship-bat ship) bate))
  
  (define e (+ 0.0 (* dt (ship-power ship)) bate extra))
  (while (not (null? suckers))
    (define ef (/ e (length suckers)))
    (define s (car suckers))
    (cond ((pod? s)
           (define need (pod-need s))
           (cond ((ef . <= . need)
                  (set-pod-energy! s (+ (pod-energy s) ef))
                  (set! e (- e ef)))
                 (else
                  (set! e (- e need))
                  (set-pod-energy! s (+ (pod-energy s) need)))))
          ((ship? s)
           (define need (ship-need s))
           (define stats (ship-stats s))
           (cond ((ef . <= . need)
                  (set-stats-con! stats (+ (stats-con stats) (* 0.1 ef)))
                  (set! e (- e ef)))
                 (else
                  (set! e (- e need))
                  (set-stats-con! stats (+ (stats-con stats) (* 0.1 need)))))))
    
    (set! suckers (cdr suckers)))
  
  (for ((s (ship-ships ship)))
    (set! e (update-energy! dt s e)))
  
  ; put back in battery energy we didn't use
  (define batback (min e (+ bate (* 5.0 dt))))
  (set! e (- e batback))
  (set-stats-bat! (ship-stats ship) (+ (ship-bat ship) batback))
  
  ; if a ship doesn't use all it's own energy, it still can't give any to its parent
  (min extra e))

