#lang racket/base

(require racket/class
         mode-lambda
         racket/draw)

(require "defs.rkt"
         "utils.rkt"
         "missile.rkt"
         "draw-utils.rkt")

(provide (all-defined-out))


(define (add-backeffects! space o)
  (when (and (missile? o) (time-for (obj-age space o) 200))
    (define be (backeffect 0 (space-time space)
                           (posvel 0
                                   (- (obj-x o) (* (missile-radius o) (cos (obj-r o))))
                                   (- (obj-y o) (* (missile-radius o) (sin (obj-r o))))
                                   0
                                   (- (* 16.0 (cos (obj-r o))))
                                   (- (* 16.0 (sin (obj-r o))))
                                   0) #f 200))
    (set-space-objects! space (cons be (space-objects space))))
  
  (when (ship? o)
    (define w (find-id o warp? #f))
    (define ftstack (find-stack o fthrust? #f))
    (cond
      ((and w ((warp-e w) . > . 0.0) (equal? "release" (warp-mode w)))
       (when (time-for (obj-age space o) 100)
         (define l (- (ship-radius o)))
         (define t (posvel-r (obj-posvel o)))
         (define be (backeffect 0 (space-time space)
                                (posvel 0
                                        (+ (posvel-x (obj-posvel o)) (* l (cos t))
                                           (* (random-between (- l) l) (sin t)))
                                        (+ (posvel-y (obj-posvel o)) (* l (sin t))
                                           (* (random-between (- l) l) (cos t)))
                                        0
                                        (- (* 1.5 (posvel-dx (obj-posvel o))))
                                        (- (* 1.5 (posvel-dy (obj-posvel o))))
                                        0) #f 1000))
         (set-space-objects! space (cons be (space-objects space)))))
      ((and w ((warp-e w) . < . (warp-maxe w)) (equal? "hold" (warp-mode w)))
       ; charging warp drive
       )
      ((and ftstack (fthrust-on (car ftstack)))
       (when (or (and (time-for (obj-age space o) 800)
                      ((pod-energy (get-pod ftstack)) . > . 0.0))
                 (and (time-for (obj-age space o) 800 400)
                      ((pod-energy (get-pod ftstack)) . > . 1.0)))
         
         (define l (- (ship-radius o)))
         (define t (posvel-r (obj-posvel o)))
         (define be (backeffect 0 (space-time space)
                                (posvel 0
                                        (+ (posvel-x (obj-posvel o)) (* l (cos t)))
                                        (+ (posvel-y (obj-posvel o)) (* l (sin t)))
                                        0
                                        (- (posvel-dx (obj-posvel o)))
                                        (- (posvel-dy (obj-posvel o)))
                                        0) #f 1000))
         (set-space-objects! space (cons be (space-objects space))))))))



(define (effect-dead? space e)
  ((obj-age space e) . > . (effect-duration e)))


(define (draw-effect csd center scale space e fowa layer_effects)
  (cond ((backeffect? e)
         (draw-backeffect csd center scale space e fowa))
        (else
         (define agep (linear-fade (obj-age space e) 0 (effect-duration e)))
         (define col (linear-color "yellow" "yellow" 0 agep))
         (define rad (* (+ 1.0 (- 1.0 agep)) (effect-size e)))
         (obj-sprite e csd center scale layer_effects 'circle (* 2.0 rad) (* fowa agep) 0 col))))


(define (draw-backeffect csd center scale space e fowa)
  (define z (linear-fade (obj-age space e) 0.0 (effect-duration e)))
  (define col (linear-color "white" "red" (- 1.0 z) z))
  (obj-sprite e csd center scale LAYER_MAP 'circle 3.0 (* fowa z) 0 col))

