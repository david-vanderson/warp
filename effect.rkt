#lang racket/base

(require racket/class
         racket/draw)

(require "defs.rkt"
         "utils.rkt")

(provide (all-defined-out))


(define (add-backeffects! space o dt)
  (when (and (ship? o) (pilot-fore (ship-pilot o))
             ((modulo (+ dt (obj-age space o)) 500) . < . dt))
    (define l -20)
    (define t (posvel-r (obj-posvel o)))
    (define be (backeffect 0 (space-time space)
                           (posvel 0
                                   (+ (posvel-x (obj-posvel o)) (* l (cos t)))
                                   (+ (posvel-y (obj-posvel o)) (* l (sin t)))
                                   0
                                   (- (posvel-dx (obj-posvel o)))
                                   (- (posvel-dy (obj-posvel o)))
                                   0)))
    (set-space-objects! space (cons be (space-objects space)))))


(define EFFECT_DEAD 300)
(define BACKEFFECT_DEAD 1000)


(define (effect-dead? space e)
  ((obj-age space e) . > . (if (backeffect? e) BACKEFFECT_DEAD EFFECT_DEAD)))


(define (draw-effect dc space center e)
  (cond ((backeffect? e)
         (draw-backeffect dc space center e))
        (else
         (define-values (x y) (recenter center e))
         (define size (+ 6.0 (* (/ (obj-age space e) EFFECT_DEAD) 6.0)))
         (define c (send the-color-database find-color "yellow"))
         (define cc (make-color (send c red) (send c green) (send c blue)
                                (- 1.0 (min 1.0 (/ (obj-age space e) EFFECT_DEAD)))))
         (send dc set-pen cc (/ 18.0 size) 'solid)
         (send dc set-brush nocolor 'transparent)
         (send dc draw-ellipse (- x (/ size 2)) (- y (/ size 2)) size size))))


(define (draw-backeffect dc space center e)
  (define-values (x y) (recenter center e))
  (define a (send the-color-database find-color "white"))
  (define b (send the-color-database find-color "red"))
  (define z (min 1.0 (/ (obj-age space e) BACKEFFECT_DEAD)))
  (define nz (- 1.0 z))
  (define cc (make-color (inexact->exact (floor (+ (* nz (send a red))   (* z (send b red)))))
                         (inexact->exact (floor (+ (* nz (send a green)) (* z (send b green)))))
                         (inexact->exact (floor (+ (* nz (send a blue))  (* z (send b blue)))))
                         nz))
  (send dc set-pen cc 1.0 'solid)
  (send dc set-brush cc 'solid)
  (define size 3)
  (send dc draw-ellipse (- x (/ size 2)) (- y (/ size 2)) size size))

