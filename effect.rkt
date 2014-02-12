#lang racket/base

(require racket/class
         racket/draw)

(require "defs.rkt"
         "utils.rkt")

(provide (all-defined-out))


(define (add-backeffects! space o dt)
  (when (and (ship? o)
             (pilot-fore (ship-pilot o))
             (or (<= 1 (modulo (obj-age space o) 800) dt)
                 (and (<= 401 (modulo (obj-age space o) 800) (+ 400 dt))
                      ((pod-energy (ship-helm o)) . > . 1))))
    ;(printf "~a adding backeffect at ~a ~a\n" (ship-name o) (modulo (obj-age space o) 500) (obj-age space o))
    (define l -20)
    (define t (posvel-r (obj-posvel o)))
    (define be (backeffect 0 (space-time space)
                           (posvel 0
                                   (+ (posvel-x (obj-posvel o)) (* l (cos t)))
                                   (+ (posvel-y (obj-posvel o)) (* l (sin t)))
                                   0
                                   (- (posvel-dx (obj-posvel o)))
                                   (- (posvel-dy (obj-posvel o)))
                                   0) #f #f))
    (set-space-objects! space (cons be (space-objects space)))))


(define BACKEFFECT_DEAD 1000)


(define (effect-dead? space e)
  ((obj-age space e) . > . (if (backeffect? e) BACKEFFECT_DEAD (effect-duration e))))


(define (draw-effect dc space center e)
  (cond ((backeffect? e)
         (draw-backeffect dc space center e))
        (else
         (define-values (x y) (recenter center e))
         (define agep (min 1.0 (/ (obj-age space e) (effect-duration e))))
         (define rad (* 2 agep (effect-size e)))
         (define c (send the-color-database find-color "yellow"))
         (define cc (make-color (send c red) (send c green) (send c blue) (- 1.0 agep)))
         (send dc set-pen cc (* (- 1.0 agep) (* 1 (effect-size e))) 'solid)
         (send dc set-brush nocolor 'transparent)
         (send dc draw-ellipse (- x rad) (- y rad) (* 2 rad) (* 2 rad)))))


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

