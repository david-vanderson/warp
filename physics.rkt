#lang racket/base

(require "defs.rkt"
         "utils.rkt"
         "quadtree.rkt"
         "plasma.rkt"
         "explosion.rkt"
         "shield.rkt"
         "effect.rkt"
         "ships.rkt"
         "warp.rkt"
         "upgrade.rkt")

(provide (all-defined-out))

; what is put into the quadtree and used for collisions
; - collisions determine obj-neb (how far inside nebula)
; also used for fog of war (view? #t)
(define (obj-radius ownspace o [view? #f])
  (cond
    ((nebula? o) (nebula-radius o))
    ((mine? o) (if view? (ship-radius o) (ship-radar o)))
    ((ship? o) (ship-radius o))
    ((plasma? o) (plasma-radius ownspace o))
    ((explosion? o) (explosion-radius o))
    ((upgrade? o) (upgrade-radius ownspace o))
    ((effect? o) (effect-size o))
    (else #f)))


; return a list of changes
(define (plasma-hit-ship! space ship p)
  (define changes '())
  ;(printf "plasma hit ship ~a (~a)\n" (ship-name ship) (ob-id ship))
    
  (define damage (plasma-damage space p))
  (append! changes (chdam (ob-id p) damage #f)
           (chdam (ob-id ship) damage #f))
  changes)


; return a list of changes
(define (missile-hit-ship! space ship m)
  (define changes '())
  ;(printf "missile hit ship ~a\n" (ship-name ship))
  
  (define damage (ship-maxcon m))
  (append! changes (list (chdam (ob-id ship) damage #t)
                         (chdam (ob-id m) damage #f)))
  changes)


; return a list of changes
(define (mine-hit-ship! space m s)
  (define changes '())
  ;(printf "mine hit ship ~a\n" (ship-name s))
  
  (define damage (ship-maxcon m))
  (append! changes (list (chdam (ob-id s) damage #t)
                         (chdam (ob-id m) damage #f)))
  changes)


(define (missile-hit-missile! space m1 m2)
  (define changes '())
  (define d (+ (ship-maxcon m1) (ship-maxcon m2)))
  (append! changes
           (chdam (ob-id m1) d #f)
           (chdam (ob-id m2) d #f))
  changes)


(define (cb-hit-cb! space cb1 cb2)
  (define changes '())
  (define d (+ (ship-maxcon cb1) (ship-maxcon cb2)))
  (append! changes
           (chdam (ob-id cb1) d #f)
           (chdam (ob-id cb2) d #f))
  changes)


(define (cb-hit-ship! space cb ship)
  (define changes '())
  ;(printf "cannon hit ship ~a\n" (ship-name ship))

  (define d (ship-maxcon cb))
  (append! changes (list (chdam (ob-id ship) d #t)
                         (chdam (ob-id cb) d #f)))
  changes)


; return a list of changes
(define (plasma-hit-shield! space s p)
  (define changes '())
  (when (and (not (shield-dead? space s))
             (not (plasma-dead? space p)))
    (define r (obj-r s))
    (define px (- (obj-x p) (obj-x s)))
    (define py (- (obj-y p) (obj-y s)))
    (define x (+ (* px (cos r)) (* py (sin r))))
    (define y (+ (* -1 px (sin r)) (* py (cos r))))
    ; x,y are now the position of the plasma in the coord space of the shield
    ; shield is along the y axis
    (define rad (plasma-radius space p))
    (define l (shield-length space s))
    (when (and (< (- rad) x rad)
               (< (- (- (/ l 2)) rad) y (+ (/ l 2) rad)))
      (define damage (min (plasma-damage space p) (shield-energy space s)))
      (append! changes (list (chdam (ob-id p) damage #f)
                             (chdam (ob-id s) damage #f)))))
  changes)


(define (perpv o1 m1 o2 m2)
  (define phi (theta o1 o2))
  (/ (+ (* (dmag o1) (cos (- (dtheta o1) phi)) (- m1 m2))
        (* 2 m2 (dmag o2) (cos (- (dtheta o2) phi))))
     (+ m1 m2)))


(define (ship-collide! s1 s2)
  (define changes '())
  (define s1dx (obj-dx s1))
  (define s1dy (obj-dy s1))
  (define s2dx (obj-dx s2))
  (define s2dy (obj-dy s2))

  (when (and (= 0.0 (dmag s1)) (= 0.0 (dmag s2)))
    ; ships aren't moving, but somehow collided, maybe launched at the same time?
    ; fake as if the less massive one was moving towards the other
    (let ((s1 s1) (s2 s2))
      (when ((ship-mass s2) . < . (ship-mass s1))
        (define t s1)
        (set! s1 s2)
        (set! s2 t))
      (define t (theta s1 s2))
      (define pv1 (obj-posvel s1))
      (define d (- (+ 1.0 (hit-distance s1 s2))
                   (distance s1 s2)))
      (set-posvel-dx! pv1 (* d (cos t)))
      (set-posvel-dy! pv1 (* d (sin t)))))
     
  (define m1 (ship-mass s1))
  (define m2 (ship-mass s2))
  (define phi (theta s1 s2))
  (define perpv1 (perpv s1 m1 s2 m2))
  (define perpv2 (- (perpv s2 m2 s1 m1)))
  ;(printf "perpv1 ~a perpv2 ~a\n" perpv1 perpv2)
  
  (set-posvel-dx! (obj-posvel s1)
                  (* 0.9 (+ (* perpv1 (cos phi))
                            (* (dmag s1) (sin (- (dtheta s1) phi)) (cos (+ phi pi/2))))))
  (set-posvel-dy! (obj-posvel s1)
                  (* 0.9 (+ (* perpv1 (sin phi))
                            (* (dmag s1) (sin (- (dtheta s1) phi)) (sin (+ phi pi/2))))))
  
  
  (set-posvel-dx! (obj-posvel s2)
                  (* 0.9 (+ (* perpv2 (cos phi))
                            (* (dmag s2) (sin (- (dtheta s2) phi)) (cos (+ phi pi/2))))))
  (set-posvel-dy! (obj-posvel s2)
                  (* 0.9 (+ (* perpv2 (sin phi))
                            (* (dmag s2) (sin (- (dtheta s2) phi)) (sin (+ phi pi/2))))))
  
  ; make sure we send the new posvels right away
  (set-posvel-t! (obj-posvel s1) 0)
  (set-posvel-t! (obj-posvel s2) 0)

  ; damage ships by how much their velocities changed
  (define s1xch (- s1dx (obj-dx s1)))
  (define s1ych (- s1dy (obj-dy s1)))
  (define s1dam (/ (sqrt (+ (* s1xch s1xch) (* s1ych s1ych))) 4.0))
  (append! changes (chdam (ob-id s1) s1dam #t))

  (define s2xch (- s2dx (obj-dx s2)))
  (define s2ych (- s2dy (obj-dy s2)))
  (define s2dam (/ (sqrt (+ (* s2xch s2xch) (* s2ych s2ych))) 4.0))
  (append! changes (chdam (ob-id s2) s2dam #t))

  changes)


; return a list of changes
(define (dock! s1 s2)  
  (define changes '())
  (append! changes (list (chmov (ob-id s1) (ob-id s2) #f)))
  changes)


(define (ship-hit-ship! space ship s)
  (define changes '())
  ;(printf "ship ~a hit ship ~a\n" (ship-name ship) (ship-name s))
  (cond
    ((and (spacesuit? ship) (spacesuit? s))
     #f)
    ((or (spacesuit? ship) (spacesuit? s))
     (when (spacesuit? ship)
       (define temp ship)
       (set! ship s)
       (set! s temp))
     (when ((faction-check (ship-faction ship) (ship-faction s)) . > . 0)
       ; don't need to explicitly remove the spacesuit because chmov does that
       (append! changes (chmov (car (ship-playerids s))
                               (ob-id ship) #f))))
    ((will-dock? ship s)       
     (append! changes (dock! ship s)))
    ((will-dock? s ship)
     (append! changes (dock! s ship)))
    (else
     (when (warping? ship)
       (append! changes (command (ob-id ship) #f 'warp 'stop)))
     (when (warping? s)
       (append! changes (command (ob-id s) #f 'warp 'stop)))
     ; get relative velocity of s with respect to ship
     (define vx (- (obj-dx s) (obj-dx ship)))
     (define vy (- (obj-dy s) (obj-dy ship)))
     ; get position vector from ship to s
     (define sx (- (obj-x s) (obj-x ship)))
     (define sy (- (obj-y s) (obj-y ship)))
     ; dot velocity to position
     (define dot (+ (* vx sx) (* vy sy)))
     (when (dot . <= . 0.0)
       ; only collide if the ships are moving towards each other
       (append! changes (ship-collide! ship s)))))
  changes)


; numeric priority that controls the order that objects are seen by collide!
(define (priority o)
  (cond ((nebula? o) 0)
        ((explosion? o) 1)
        ((plasma? o) 2)
        ((cannonball? o) 3)
        ((missile? o) 4)
        ((mine? o) 5)
        ((spaceship? o) 6)
        ((probe? o) 6)
        ((spacesuit? o) 6)
        ((effect? o) 7)
        ((upgrade? o) 8)
        (else (printf "priority unknown for ~v\n" o) 9)))


(define (collide-common! a b dt)
  (cond
    ((and (nebula? a)
          (not (nebula? b)))
     (define d (distance a b))
     (define n (- 1.0 (linear-fade d (* (nebula-radius a) 0.7) (nebula-radius a))))
     (set-obj-neb! b (min (obj-neb b) n)))
    ((and (mine? a) (ship-tool a 'engine))
     (cond ((spaceship? b)
            (define d (distance a b))
            (when (d . < . (+ (ship-radar a) (ship-radius b)))
              (define xy_acc (tool-val (ship-tool a 'engine)))
              (define r (theta a b))
              (define ddx (* xy_acc (cos r)))
              (define ddy (* xy_acc (sin r)))
              (set-posvel-dx! (obj-posvel a) (+ (obj-dx a) (* ddx dt)))
              (set-posvel-dy! (obj-posvel a) (+ (obj-dy a) (* ddy dt)))))))))

; called on every pair of objects that might be colliding
; called only once for each pair
; (priority a) <= (priority b)
; return a list of changes
(define (collide! space a b dt)
  (collide-common! a b dt)
  (when (server?)
    (cond
      ((explosion? a)
       (cond ((plasma? b)
              (when ((distance a b) . < . (+ (plasma-radius space b) (explosion-radius a)))
                (list (chdam (ob-id b) (explosion-damage a dt) #t))))
             ((or (cannonball? b)
                  (mine? b)
                  (missile? b)
                  (spaceship? b)
                  (probe? b))
              (when ((distance a b) . < . (+ (ship-radius b) (explosion-radius a)))
                (list (chdam (ob-id b) (explosion-damage a dt) #t))))))
      ((plasma? a)
       (cond ((or (spaceship? b)
                  (probe? b)
                  (missile? b)
                  (mine? b)
                  (cannonball? b))
              (when ((distance a b) . < . (+ (ship-radius b) (plasma-radius space a)))
                (plasma-hit-ship! space b a)))))
      ((cannonball? a)
       (cond ((cannonball? b)
              (when ((distance a b) . < . (hit-distance a b))
                (cb-hit-cb! space a b)))
             ((or (spaceship? b)
                  (missile? b)
                  (mine? b)
                  (probe? b))
              (when ((distance a b) . < . (hit-distance a b))
                (cb-hit-ship! space a b)))))
      ((missile? a)
       (cond ((missile? b)
              (when ((distance a b) . < . (hit-distance a b))
                (missile-hit-missile! space a b)))
             ((or (spaceship? b)
                  (mine? b)
                  (probe? b))
              (when ((distance a b) . < . (hit-distance a b))
                (missile-hit-ship! space b a)))))
      ((mine? a)
       (cond ((or (spaceship? b) (mine? b))
              (define d (distance a b))
              (when (d . < . (hit-distance a b))
                (mine-hit-ship! space a b)))
             ((probe? b)
              (define d (distance a b))
              (when (d . < . (hit-distance a b))
                (ship-hit-ship! space a b)))))
      ((or (spaceship? a)
           (probe? a)
           (spacesuit? a))
       (cond
         ((or (spaceship? b)
              (probe? b)
              (spacesuit? b))
          (when ((distance a b) . < . (hit-distance a b))
            (ship-hit-ship! space a b))))))))


(define (add-to-qt! ownspace qt o)
  (define rad (obj-radius ownspace o))
  (when rad
    (qt-add! qt o (obj-x o) (obj-y o) rad)))
  

(define (tick-space! ownspace apply-all-changes!)
  (define updates '())
  (set-space-time! ownspace (+ (space-time ownspace) TICK))
  (define qt (qt-new 0 0 (space-width ownspace) (space-height ownspace)))
  (for ((o (in-list (space-objects ownspace)))
        #:when (obj-alive? o))
    ; everything starts outside the nebula, collide will update
    (set-obj-neb! o 1.0)
    (update-physics! ownspace o (/ TICK 1000.0))
    (update-stats! ownspace o (/ TICK 1000.0))
    (add-to-qt! ownspace qt o)
    (when (client?)
      (define be (make-backeffect! ownspace o))
      (when (backeffect? be)
        (set-space-objects! ownspace (cons be (space-objects ownspace)))
        (add-to-qt! ownspace qt be))))

  ; need to delay adding new things to quadtree during qt-collide!
  (define objs-added '())
  (define (addf-delay o)
    (set! objs-added (cons o objs-added)))

  (define (coll! a b)
    (when (and (obj-alive? a)
               (obj-alive? b))
      (define precs (if ((priority a) . <= . (priority b))
                        (collide! ownspace a b (/ TICK 1000.0))
                        (collide! ownspace b a (/ TICK 1000.0))))
      (when (not (void? precs))
        (define cs (apply-all-changes! ownspace precs "server" #:addf addf-delay))
        (append! updates cs))))
  
  (qt-collide! qt coll!)

  ; add all new things to the quadtree
  (for ((o (in-list objs-added)))
    (add-to-qt! ownspace qt o))

  (when (and (client?) (not (null? updates)))
    (error "client got updates from tick-space!"))

  (values qt updates))


(define (steer! space ownship dt)
  (define pv (obj-posvel ownship))
  ;(printf "steer! for ~a\n" (ship-name ownship))
  
  (cond
    ((warping? ownship)
     (define w (ship-tool ownship 'warp))
     (set-posvel-dx! pv (* (warp-speed w) (cos (posvel-r pv))))
     (set-posvel-dy! pv (* (warp-speed w) (sin (posvel-r pv))))
     (set-posvel-dr! pv 0.0))
    (else
     (define st (ship-tool ownship 'steer))
     (define tl (ship-tool ownship 'turnleft))
     (define tr (ship-tool ownship 'turnright))
     (when (or st tl tr)
       (set-posvel-dr! pv 0.0))  ; ignore whatever we had before

     (define cl (tool-count space tl ownship))
     (define cr (tool-count space tr ownship))
     
     (cond
       ((or (cl . > . 0)
            (cr . > . 0))
        (define racc (- (* (geom-sum 1.0 PLAYER_RATIO cl) (if tl (tool-val tl) 0.0))
                        (* (geom-sum 1.0 PLAYER_RATIO cr) (if tr (tool-val tr) 0.0))))
        (set-posvel-dr! pv racc))
       ((and st (ai-ship? ownship) (tool-rc st))
        (define course (tool-rc st))
        (define r (posvel-r pv))
        (define racc (tool-val st))
        (cond
          (((abs (angle-frto r course)) . < . (* racc dt))
           ;(printf "STOPPING\n")
           (set-posvel-r! pv course)
           (set-posvel-dr! pv 0.0))
          (else
           (set-posvel-dr! pv (if ((angle-frto r course) . > . 0.0) racc (- racc)))))))
     
     (define eng (ship-tool ownship 'engine))
     (when eng
       (define c (tool-count space eng ownship))
       (when (c . > . 0)
         (define xy_acc (* (geom-sum 1.0 PLAYER_RATIO c) (tool-val eng)))
         (define ddx (* xy_acc (cos (posvel-r pv))))
         (define ddy (* xy_acc (sin (posvel-r pv))))
         (set-posvel-dx! pv (+ (posvel-dx pv) (* ddx dt)))
         (set-posvel-dy! pv (+ (posvel-dy pv) (* ddy dt))))))))


(define (drag dv dt coef)
  (define newv (* dv (expt (1.0 . - . coef) dt)))
  (if ((abs newv) . < . .01) 0 newv))


(define (physics! pv dt (drag_xy 0.0))
  (set-posvel-x! pv (+ (posvel-x pv) (* dt (posvel-dx pv))))
  (set-posvel-y! pv (+ (posvel-y pv) (* dt (posvel-dy pv))))
  (set-posvel-r! pv (angle-add (posvel-r pv) (* dt (posvel-dr pv))))
  (when (not (= drag_xy 0.0))
    (set-posvel-dx! pv (drag (posvel-dx pv) dt drag_xy))
    (set-posvel-dy! pv (drag (posvel-dy pv) dt drag_xy))))


(define EDGE_THRUST 10.0)

(define (outside? space o)
  (or ((obj-x o) . > . (/ (space-width space) 2))
      ((obj-x o) . < . (/ (- (space-width space)) 2))
      ((obj-y o) . > . (/ (space-height space) 2))
      ((obj-y o) . < . (/ (- (space-height space)) 2))))

(define (push-back! space o dt)
  (define pv (obj-posvel o))
  (define dx ((posvel-x pv) . - . (/ (space-width space) 2)))
  (when (dx . > . 0)
    (set-posvel-dx! pv (- (posvel-dx pv) (* EDGE_THRUST dt dx))))
  (when (dx . < . (- (space-width space)))
    (set-posvel-dx! pv (- (posvel-dx pv) (* EDGE_THRUST dt (+ dx (space-width space))))))

  (define dy ((posvel-y pv) . - . (/ (space-height space) 2)))
  (when (dy . > . 0)
    (set-posvel-dy! pv (- (posvel-dy pv) (* EDGE_THRUST dt dy))))
  (when (dy . < . (- (space-height space)))
    (set-posvel-dy! pv (- (posvel-dy pv) (* EDGE_THRUST dt (+ dy (space-height space)))))))

(define (update-physics! space o dt)
  (define pv (obj-posvel o))
  (cond
    ((ship? o)
     (physics! pv dt (ship-drag o))
     (steer! space o dt)
     (push-back! space o dt))
    ((plasma? o)
     (physics! pv dt)
     (push-back! space o dt)
     (when (plasma-dead? space o)
       (set-obj-alive?! o #f)))
    ((explosion? o)
     (physics! pv dt)
     (when (explosion-dead? o)
       (set-obj-alive?! o #f)))
    ((shield? o)
     (physics! pv dt 0.4)
     (push-back! space o dt)
     (when (shield-dead? space o)
       (set-obj-alive?! o #f)))
    ((effect? o)
     (physics! pv dt)
     (when (effect-dead? space o)
       (set-obj-alive?! o #f)))
    ((message? o)
     (when ((obj-age space o) . > . MSG_FADE_TIME)
       (set-obj-alive?! o #f)))
    ((and (ann-text? o) (ann-text-life o))
     (when ((obj-age space o) . > . (+ (ann-text-life o) MSG_FADE_TIME))
       (set-obj-alive?! o #f)))
    ((upgrade? o)
     (physics! pv dt 0.4)
     (when (not (upgrade-alive? space o))
       (set-obj-alive?! o #f)))))


; return list of additional changes
(define (reduce-ship! space ship damage)
  (define changes '())

  (when (not (ship-invincible? ship))
    (set-ship-con! ship (- (ship-con ship) damage)))
  
  (when ((ship-con ship) . <= . 0)
    (set-obj-alive?! ship #f)
    
    (define pv (obj-posvel ship))
    (define energy (ship-maxcon ship))
    
    (when (client?)
      (define e (effect (next-id) (space-time space) #t 1.0 (struct-copy posvel pv) (sqrt energy) 1000))
      (append! changes (chadd e #f)))
    
    (when (server?)
      (for ((ps (in-list (search space ship player? #t))))
        (define p (car ps))
        (define ss (make-spacesuit (player-name p) ship))
        (append! changes (chadd ss #f) (chmov (ob-id p) (ob-id ss) #f)))
      
      (for ((u (in-list (ship-cargo ship))))
        (define newpv (posvel (space-time space) (posvel-x pv) (posvel-y pv) 0
                              (+ (posvel-dx pv) (random-between -50 50))
                              (+ (posvel-dy pv) (random-between -50 50)) 0))
        (set-obj-posvel! u newpv)
        (set-obj-start-time! u (space-time space))
        (append! changes (chadd u #f)))
      
      (define energy-left energy)
      (while (energy-left . > . 1)
        (define e (random-between 5 (sqrt energy)))
        (set! energy-left (- energy-left e))
        (define t (random-between 0 2pi))
        (define s (random-between 10 50))
        (define p (plasma (next-id) (space-time space) #t 1.0
                          (posvel (space-time space) (posvel-x pv) (posvel-y pv) 0
                                  (+ (* s (cos t)) (posvel-dx pv))
                                  (+ (* s (sin t)) (posvel-dy pv))
                                  0)
                          e))
        (append! changes (chadd p #f)))

      (when (spaceship? ship)
        (define msg (make-message space
                                  (format "~a Destroyed" (ship-name ship))))
        (append! changes msg))))
  changes)



(define (update-stats! space o dt)
  (cond
    ((ship? o) (update-ship! space o (/ TICK 1000.0)))
    ((explosion? o) (update-explosion! space o (/ TICK 1000.0)))))


(define (update-ship! space ship dt)
  (define w (ship-tool ship 'warp))
  (when w
    (cond
      ((warping? ship)
       ; nothing changes
       )
      ((warp-charging? space ship)
       (define wc (tool-count space w ship))
       (set-tool-val! w (list (warp-speed w)
                              (warp-threshold w)
                              (min (warp-threshold w)
                                   (+ (warp-energy w)
                                      (* 10.0 dt (geom-sum 1.0 PLAYER_RATIO wc)))))))
      (else
       ; drain it
       (set-tool-val! w (list (warp-speed w)
                              (warp-threshold w)
                              (max 0.0 (- (warp-energy w) (* 10.0 dt))))))))

  (set-ship-dmgfx! ship (max 0.0 (- (ship-dmgfx ship) (* 20.0 dt))))

  (define r (ship-tool ship 'regen))
  (when r
    (set-ship-con! ship (min (ship-maxcon ship)
                             (+ (ship-con ship) (* (tool-val r) dt)))))
  
  (for ((s (in-list (ship-ships ship))))
    (set-ship-con! s (min (ship-maxcon s)
                          (+ (ship-con s) (* 10.0 dt))))
    (update-ship! space s dt)))

