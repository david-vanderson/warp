#lang racket/base

(require data/heap)

(require "defs.rkt"
         "utils.rkt"
         "plasma.rkt"
         "shield.rkt"
         "effect.rkt"
         "ships.rkt"
         "warp.rkt"
         "upgrade.rkt")

(provide (all-defined-out))


(define (steer! ownship dt)
  (define trying? #f)
  (define pv (obj-posvel ownship))
  ;(printf "steer! for ~a\n" (ship-name ownship))
  
  (cond
    ((warping? ownship)
     (define w (ship-tool ownship 'warp))
     (set-tool-val! w (list (car (tool-val w))
                            (cadr (tool-val w))
                            (max 0 (- (caddr (tool-val w)) (* 25.0 dt)))))
     (define xy_acc (car (tool-val w)))
     (define ddx (* xy_acc (cos (posvel-r pv))))
     (define ddy (* xy_acc (sin (posvel-r pv))))
     (set-posvel-dx! pv (+ (posvel-dx pv) (* ddx dt)))
     (set-posvel-dy! pv (+ (posvel-dy pv) (* ddy dt)))
     (set! trying? #t))
    (else
     (when (warp-charging? ownship)
       (define w (ship-tool ownship 'warp))
       (define wc (tool-count w ownship))
       (set-tool-val! w (list (car (tool-val w))
                              (cadr (tool-val w))
                              (min (cadr (tool-val w))
                                   (+ (caddr (tool-val w)) (* 10.0 dt wc))))))

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
           (set-posvel-dr! pv (if ((angle-frto r course) . > . 0.0) racc (- racc)))
           (set! trying? #t))))
       ((or tl tr)
        (define cl (tool-count tl ownship))
        (define cr (tool-count tr ownship))
        (define racc (- (* cl (if tl (tool-val tl) 0.0))
                        (* cr (if tr (tool-val tr) 0.0))))
        (set-posvel-dr! pv racc)))
     
     (define eng (ship-tool ownship 'engine))
     (when eng
       (define c (tool-count eng ownship))
       (when (c . > . 0)
         (define xy_acc (* c (tool-val eng)))
         (define ddx (* xy_acc (cos (posvel-r pv))))
         (define ddy (* xy_acc (sin (posvel-r pv))))
         (set-posvel-dx! pv (+ (posvel-dx pv) (* ddx dt)))
         (set-posvel-dy! pv (+ (posvel-dy pv) (* ddy dt)))
         (set! trying? #t)))))
  
  trying?)


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

(define (push-back! space o dt)
  (define pv (obj-posvel o))
  (when ((posvel-x pv) . > . (/ (space-width space) 2))
    (set-posvel-dx! pv (- (posvel-dx pv) (* EDGE_THRUST dt))))
  (when ((posvel-x pv) . < . (- (/ (space-width space) 2)))
    (set-posvel-dx! pv (+ (posvel-dx pv) (* EDGE_THRUST dt))))
  (when ((posvel-y pv) . > . (/ (space-height space) 2))
    (set-posvel-dy! pv (- (posvel-dy pv) (* EDGE_THRUST dt))))
  (when ((posvel-y pv) . < . (- (/ (space-height space) 2)))
    (set-posvel-dy! pv (+ (posvel-dy pv) (* EDGE_THRUST dt)))))

(define (update-physics! space o dt)
  (define pv (obj-posvel o))
  (cond
    ((ship? o)
     (physics! pv dt (ship-drag o) (steer! o dt))
     (push-back! space o dt))
    ((plasma? o)
     (physics! pv dt)
     (push-back! space o dt)
     (when (plasma-dead? space o)
       (set-space-objects! space (remove o (space-objects space)))))
    ((shield? o)
     (physics! pv dt 0.4)
     (push-back! space o dt)
     (when (shield-dead? space o)
       (set-space-objects! space (remove o (space-objects space)))))
    ((effect? o)
     (physics! pv dt)
     (when (effect-dead? space o)
       (set-space-objects! space (remove o (space-objects space)))))
    ((message? o)
     (when ((obj-age space o) . > . MSG_FADE_TIME)
       (set-space-objects! space (remove o (space-objects space)))))
    ((and (ann-text? o) (ann-text-life o))
     (when ((obj-age space o) . > . (+ (ann-text-life o) MSG_FADE_TIME))
       (set-space-objects! space (remove o (space-objects space)))))
    ((upgrade? o)
     (physics! pv dt 0.4)
     (when (upgrade-dead? space o)
       (set-space-objects! space (remove o (space-objects space)))))))


; return list of additional changes
(define (reduce-ship! space ship damage)
  (define changes '())
  (set-stats-con! (ship-stats ship) (- (ship-con ship) damage))
  
  (when ((ship-con ship) . <= . 0)
    (set-space-objects! space (remove ship (space-objects space)))
    (when (server?)
      (define pv (obj-posvel ship))
      (define energy (ship-mass ship))
      (define e (effect (next-id) (space-time space) (struct-copy posvel pv) (sqrt energy) 1000))
      (append! changes (chadd e #f))
      
      (for ((ps (in-list (search ship player? #t))))
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
        (define p (plasma (next-id) (space-time space)
                          (posvel (space-time space) (posvel-x pv) (posvel-y pv) 0
                                  (+ (* s (cos t)) (posvel-dx pv))
                                  (+ (* s (sin t)) (posvel-dy pv))
                                  0)
                          e #f))
        (append! changes (chadd p #f)))
      
      (define msg (message (next-id) (space-time space) #f
                           (format "~a Destroyed" (ship-name ship))))
      (append! changes msg)))
  changes)


(define (repair-subships! dt ship)
  (for ((s (in-list (ship-ships ship))))
    (define stats (ship-stats s))
    (set-stats-con! stats (min (stats-maxcon stats)
                               (+ (stats-con stats) (* 5.0 dt))))
    (repair-subships! dt s)))

