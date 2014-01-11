#lang racket/base

(require racket/class
         racket/draw)

(require "defs.rkt"
         "utils.rkt"
         "draw-utils.rkt")

(provide (all-defined-out))


(define SHIELD_LIFE 10000)  ; ms after which shield starts fading
(define SHIELD_DEATH 15000)  ; ms after which shield is dead


(define (shield-energy space s)
  (* (linear-fade (obj-age space s) SHIELD_LIFE SHIELD_DEATH)
     (shield-e s)))


(define (shield-dead? space s)
  (or ((obj-age space s) . > . SHIELD_DEATH)
      ((shield-energy space s) . < . 1)))


(define (reduce-shield! space s damage)
  (set-shield-e! s (- (shield-e s) (/ damage (linear-fade (obj-age space s) SHIELD_LIFE SHIELD_DEATH))))
  (when ((shield-energy space s) . < . 1)
    (set-space-objects! space (remove s (space-objects space)))))


(define (shield-color space s)
  (define b (send the-color-database find-color "blue"))
  (make-color (send b red)
              (send b green) 
              (send b blue)
              (/ (shield-energy space s) 20.0)))


(define (draw-shield dc space center s)
  (define-values (x y) (recenter center s))
  (send dc set-pen (shield-color space s) 3 'solid)
  (define len (shield-length s))
  (keep-transform dc
    (send dc translate x y)
    (send dc rotate (- (posvel-r (obj-posvel s))))
    (send dc draw-line 0 (* -0.5 len) 0 (* 0.5 len))))

