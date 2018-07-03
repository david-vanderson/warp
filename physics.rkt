#lang racket/base

(require data/heap)

(require "defs.rkt"
         "utils.rkt"
         "plasma.rkt"
         "explosion.rkt"
         "shield.rkt"
         "effect.rkt"
         "ships.rkt"
         "warp.rkt"
         "upgrade.rkt")

(provide (all-defined-out))


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

     (cond
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
           (set-posvel-dr! pv (if ((angle-frto r course) . > . 0.0) racc (- racc))))))
       ((or tl tr)
        (define cl (tool-count space tl ownship))
        (define cr (tool-count space tr ownship))
        (define racc (- (* (geom-sum 1.0 PLAYER_RATIO cl) (if tl (tool-val tl) 0.0))
                        (* (geom-sum 1.0 PLAYER_RATIO cr) (if tr (tool-val tr) 0.0))))
        (set-posvel-dr! pv racc)))
     
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


(define (physics! pv dt (drag_xy 0.0) (acc? #f))
  (set-posvel-x! pv (+ (posvel-x pv) (* dt (posvel-dx pv))))
  (set-posvel-y! pv (+ (posvel-y pv) (* dt (posvel-dy pv))))
  (set-posvel-r! pv (angle-add (posvel-r pv) (* dt (posvel-dr pv))))
  (set-posvel-dx! pv (drag (posvel-dx pv) dt drag_xy))
  (set-posvel-dy! pv (drag (posvel-dy pv) dt drag_xy)))


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
  (set-stats-con! (ship-stats ship) (- (ship-con ship) damage))
  
  (when ((ship-con ship) . <= . 0)
    (set-obj-alive?! ship #f)
    
    (define pv (obj-posvel ship))
    (define energy (ship-maxcon ship))
    
    (when (client?)
      (define e (effect (next-id) (space-time space) #t (struct-copy posvel pv) (sqrt energy) 1000))
      (append! changes (chadd e #f)))
    
    (when (server?)
      (for ((ps (in-list (search space ship player? #t))))
        (define p (car ps))
        (define ss (make-spacesuit (player-name p) ship))
        (append! changes (chadd ss #f) (chmov (ob-id p) (ob-id ss) #f)))
      
      (for ((u (in-list (ship-cargo ship)))
            #:when (upgrade-color u))  ; if no color, then it's not meant to exist outside the ship
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
        (define p (plasma (next-id) (space-time space) #t
                          (posvel (space-time space) (posvel-x pv) (posvel-y pv) 0
                                  (+ (* s (cos t)) (posvel-dx pv))
                                  (+ (* s (sin t)) (posvel-dy pv))
                                  0)
                          e))
        (append! changes (chadd p #f)))
      
      (define msg (message (next-id) (space-time space) #t #f
                           (format "~a Destroyed" (ship-name ship))))
      (append! changes msg)))
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
    (define stats (ship-stats ship))
    (set-stats-con! stats (min (stats-maxcon stats)
                               (+ (stats-con stats) (* (tool-val r) dt)))))
  
  (for ((s (in-list (ship-ships ship))))
    (define stats (ship-stats s))
    (set-stats-con! stats (min (stats-maxcon stats)
                               (+ (stats-con stats) (* 10.0 dt))))
    (update-ship! space s dt)))

