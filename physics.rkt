#lang racket/base

(require data/heap)

(require "defs.rkt"
         "utils.rkt"
         "plasma.rkt"
         "shield.rkt"
         "effect.rkt"
         "ships.rkt"
         "upgrade.rkt")

(provide (all-defined-out))


(define (steer! ownship dt)
  (define cstack (find-stack ownship steer? #f))
  (cond
    ((not cstack) #f)
    (else
     (define course (steer-course (car cstack)))
     (define posvel (obj-posvel ownship))
     (define r (posvel-r posvel))
     
     (define racc (stats-rthrust (ship-stats ownship)))
     (cond
       (((abs (angle-diff r course)) . < . (* racc dt))
        ;(printf "STOPPING\n")
        (set-posvel-r! posvel course)
        (set-posvel-dr! posvel 0.0))
       (((pod-energy (get-pod cstack)) . > . 0.0)
        (set-posvel-dr! posvel (if ((angle-diff r course) . > . 0.0) racc (- racc)))))

     (define ftstack (find-stack ownship fthrust? #f))
     
     (cond
       ((and ftstack (fthrust-on (car ftstack)))
        (when ((pod-energy (get-pod ftstack)) . > . 0.0)
          (define xy_acc (stats-thrust (ship-stats ownship)))
          (define ddx (* xy_acc (cos (posvel-r posvel))))
          (define ddy (* xy_acc (sin (posvel-r posvel))))
          (set-posvel-dx! posvel (+ (posvel-dx posvel) (* ddx dt)))
          (set-posvel-dy! posvel (+ (posvel-dy posvel) (* ddy dt))))
        #t)
       (else #f)))))


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


(define EDGE_THRUST 100.0)

(define (update-physics! space o dt)
  (define pv (obj-posvel o))
  (cond
    ((ship? o)
     (physics! pv dt 0.4 (steer! o dt))
     (when ((posvel-x pv) . > . (/ (space-width space) 2))
       (set-posvel-dx! pv (- (posvel-dx pv) (* EDGE_THRUST dt))))
     (when ((posvel-x pv) . < . (- (/ (space-width space) 2)))
       (set-posvel-dx! pv (+ (posvel-dx pv) (* EDGE_THRUST dt))))
     (when ((posvel-y pv) . > . (/ (space-height space) 2))
       (set-posvel-dy! pv (- (posvel-dy pv) (* EDGE_THRUST dt))))
     (when ((posvel-y pv) . < . (- (/ (space-height space) 2)))
       (set-posvel-dy! pv (+ (posvel-dy pv) (* EDGE_THRUST dt)))))
    ((plasma? o)
     (physics! pv dt)
     (when (plasma-dead? space o)
       (set-space-objects! space (remove o (space-objects space)))))
    ((shield? o)
     (physics! pv dt 0.4)
     (when (shield-dead? space o)
       (set-space-objects! space (remove o (space-objects space)))))
    ((effect? o)
     (physics! pv dt)
     (when (effect-dead? space o)
       (set-space-objects! space (remove o (space-objects space)))))
    ((message? o)
     (when ((obj-age space o) . > . MSG_FADE_TIME)
       (set-space-objects! space (remove o (space-objects space)))))
    ((upgrade? o)
     (physics! pv dt 0.4)
     (when (upgrade-dead? space o)
       (set-space-objects! space (remove o (space-objects space)))))))


; return list of additional changes
(define (damage-object! space o damage)
  (cond ((plasma? o) (reduce-plasma! space o damage) '())
        ((shield? o) (reduce-shield! space o damage) '())
        ((ship? o) (reduce-ship! space o damage))))


; return list of additional changes
; when a damage is added to a tool, it might turn the tool off?
; when a damage is added, should the pod lose any energy directly?
(define (add-dmg! space t dmg)
  (define changes '())
  (printf "add-dmg! ~v\n" dmg)
  (cond
    ((ormap (lambda (d) (equal? (dmg-type dmg) (dmg-type d)))
            (tool-dmgs t))
     (printf "add-dmg! already had dmg of type ~a\n" (dmg-type dmg)))
    (else
     (set-tool-dmgs! t (append (tool-dmgs t) (list dmg)))))
  changes)


; return list of additional changes
(define (reduce-ship! space ship damage)
  (define changes '())
  (set-stats-con! (ship-stats ship) (- (ship-con ship) damage))
  
  (when (damage . > . 0)
    (define types '("translation"
                    ;"shear"
                    ;"rotation"
                    ;"fade"
                    ;"flicker"
                    ))
    (define type (list-ref types (random (length types))))
    (define d (dmgfx (next-id) (space-time space) #f type damage))
    (set-ship-dmgfx! ship (append (ship-dmgfx ship) (list d))))
  
  (when ((ship-con ship) . <= . 0)
    (set-space-objects! space (remove ship (space-objects space)))
    (when (server?)
      (define pv (obj-posvel ship))
      (define energy (ship-mass ship))
      (define e (effect (next-id) (space-time space) (struct-copy posvel pv) (sqrt energy) 1000))
      (set! changes (append changes (list (chadd e #f))))
      
      (for ((ps (in-list (search ship player? #t))))
        (define p (car ps))
        (define ss (make-spacesuit (player-name p) ship))
        (define rc (chrole p (ob-id (ship-lounge ss))))
        (set! changes (append changes (list (chadd ss #f) rc))))
      
      (for ((u (in-list (ship-cargo ship))))
        (define newpv (posvel (space-time space) (posvel-x pv) (posvel-y pv) 0
                              (+ (posvel-dx pv) (random-between -50 50))
                              (+ (posvel-dy pv) (random-between -50 50)) 0))
        (set-obj-posvel! u newpv)
        (set-obj-start-time! u (space-time space))
        (set! changes (append changes (list (chadd u #f)))))
      
      (define energy-left energy)
      (while (energy-left . > . 1)
        (define e (random-between 5 (sqrt energy)))
        (set! energy-left (- energy-left e))
        (define t (random-between 0 2pi))
        (define s (random-between 10 50))
        (define p (plasma (next-id) (space-time space)
                          (posvel (space-time space) (posvel-x pv) (posvel-y pv) 0
                                  (+ (* s (cos t)) (posvel-dx pv))
                                  (+ (* s (sin t)) (posvel-dy pv))
                                  0)
                          e #f))
        (set! changes (append changes (list (chadd p #f)))))
      
      (define msg (message (next-id) (space-time space) #f
                           (format "~a Destroyed" (ship-name ship))))
      (set! changes (append changes (list msg) ((destroy-callback) ship)))))
  changes)


(define (pod-need p) (max 0.0 (- (pod-maxe p) (max 0.0 (pod-energy p)))))
(define (ship-need s) (max 0.0 (- (ship-maxcon s) (ship-con s))))


(define (update-energy! dt ship extra)
  ;(printf "update-energy! ship ~a extra: ~a bat: ~a\n" (ship-name ship) extra (ship-bat ship))

  (define podpow (* 10.0 dt))  ; how much e can go to each pod
  (define batpow (* 20.0 dt))  ; how much e can flow out of reserve
  (define repairpow (* 10.0 dt))  ; how much e can go to repair each ship
  (define repairratio 0.5)  ; how many hp you get for each e
  (define fixpow (* 1.0 dt))  ; how much e can go to each fixing dmg
  
  ; remove energy for stateful things
  (when (ship-flying? ship)
    (define ftstack (find-stack ship fthrust? #f))
    (define p (if ftstack (get-pod ftstack) #f))
    (when (and p ((pod-energy p) . > . 0.0) (fthrust-on (car ftstack)))
      (set-pod-energy! p (- (pod-energy p) (* 3.0 dt))))
    
    (define sstack (find-stack ship steer? #f))
    (define sp (if sstack (get-pod sstack) #f))
    (when (and sp ((pod-energy sp) . > . 0.0) (not (= (steer-course (car sstack))
                                                      (posvel-r (obj-posvel ship)))))
      (set-pod-energy! sp (- (pod-energy sp) (* 2.0 dt)))))

  ; remove energy for fixing stuff
  (define dstacks (search ship dmg? #t #f))
  (for ((ds (in-list dstacks))
        #:when (dmg-fixing? (car ds)))
    (define d (car ds))
    (define tool (cadr ds))
    (define p (get-pod ds))
    (when ((pod-energy p) . > . 0.0)
      (set-pod-energy! p (- (pod-energy p) fixpow))
      (set-dmg-energy! d (+ (dmg-energy d) fixpow))
      (when ((dmg-energy d) . >= . (dmg-size d))
        (set-tool-dmgs! tool (remove-id (ob-id d) (tool-dmgs tool))))))
  
  ; take out battery energy
  (define bate (min batpow (ship-bat ship)))
  (set-stats-bat! (ship-stats ship) (- (ship-bat ship) bate))
  
  ; total energy we have to give
  (define e (+ (* dt (ship-power ship)) bate extra))
  
  ; try to give half to docked ships
  (define e-to-ships (/ e 2))
  (set! e (- e e-to-ships))
  (for ((s (in-list (ship-ships ship))))
    (set! e-to-ships (update-energy! dt s e-to-ships)))
  (set! e (+ e e-to-ships))
  
  ; split remaining half (plus whatever docked ships didn't use) between pods and repairing ships
  ; figure out how much total power is requested (limited by e)
  ; distribute power proportionally
  (define maxreq 0.0001)
  (define espent 0.0)
  (for ((p (in-list (ship-pods ship))))
    (set! maxreq (+ maxreq (min e podpow (pod-need p)))))
  (for ((s (in-list (ship-ships ship))))
    (set! maxreq (+ maxreq (min e repairpow (ship-need s)))))
  
  (for ((p (in-list (ship-pods ship))))
    (define xfer (min podpow (pod-need p) (* e (/ (min e podpow (pod-need p)) maxreq))))
    ;(printf "xfer: ~a, ~a, ~a\n" (pod-need p) maxreq xfer)
    (set-pod-energy! p (+ (pod-energy p) xfer))
    (set! espent (+ espent xfer)))
  
  (for ((s (in-list (ship-ships ship))))
    (define xfer (min repairpow (ship-need s) (* e (/ (min e repairpow (ship-need s)) maxreq))))
    (define stats (ship-stats s))
    (set-stats-con! stats (+ (stats-con stats) (* repairratio xfer)))
    (set! espent (+ espent xfer)))
  
  ;(printf "update-energy: ~a, ~a\n" e espent)
  (set! e (- e espent))
  
  ; put back in battery energy we didn't use
  ; and also possibly more from reactor
  (define batback (min e (- (ship-maxbat ship) (ship-bat ship))))
  (set! e (- e batback))
  (set-stats-bat! (ship-stats ship) (+ (ship-bat ship) batback))
  
  ; if a ship doesn't use all it's own energy, it still can't give any to its parent
  (min extra e))

