#lang racket/base

(require racket/class
         racket/draw)

(require "defs.rkt"
         "utils.rkt"
         "draw-utils.rkt")

(provide (all-defined-out))


(define (add-backeffects! space o)
  (when (ship? o)
    (define ftstack (find-stack o fthrust? #f))
    (when (and ftstack
               (fthrust-on (car ftstack))
               (or (time-for (obj-age space o) 800)
                   (and (time-for (obj-age space o) 800 400)
                        ((pod-energy (get-pod ftstack)) . > . 1))))
      ;(printf "~a adding backeffect at ~a ~a\n" (ship-name o) (modulo (obj-age space o) 500) (obj-age space o))
      (define l (- (ship-radius o)))
      (define t (posvel-r (obj-posvel o)))
      (define be (backeffect 0 (space-time space)
                             (posvel 0
                                     (+ (posvel-x (obj-posvel o)) (* l (cos t)))
                                     (+ (posvel-y (obj-posvel o)) (* l (sin t)))
                                     0
                                     (- (posvel-dx (obj-posvel o)))
                                     (- (posvel-dy (obj-posvel o)))
                                     0) #f #f))
      (set-space-objects! space (cons be (space-objects space))))))


(define BACKEFFECT_DEAD 1000)


(define (effect-dead? space e)
  ((obj-age space e) . > . (if (backeffect? e) BACKEFFECT_DEAD (effect-duration e))))


(define (draw-effect dc space e)
  (cond ((backeffect? e)
         (draw-backeffect dc space e))
        (else
         (define agep (linear-fade (obj-age space e) 0 (effect-duration e)))
         (define cc (linear-color "yellow" "yellow" 0 agep))
         (define rad (* (+ 0.5 (- 1.0 agep)) (effect-size e)))
         (send dc set-pen cc rad 'solid)
         (send dc set-brush nocolor 'transparent)
         (send dc draw-ellipse (- (obj-x e) rad) (- (obj-y e) rad) (* 2 rad) (* 2 rad)))))


(define (draw-backeffect dc space e)
  (define z (linear-fade (obj-age space e) 0.0 BACKEFFECT_DEAD))
  (define cc (linear-color "white" "red" (- 1.0 z) z))
  (send dc set-pen cc 1.0 'solid)
  (send dc set-brush cc 'solid)
  (define size 3)
  (send dc draw-ellipse (- (obj-x e) (/ size 2)) (- (obj-y e) (/ size 2)) size size))

