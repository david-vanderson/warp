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

(define (shield-sigmoid space s)
  (sigmoid (shield-energy space s) 12))

(define (shield-length space s)
  (* 30.0 (shield-sigmoid space s)))

(define (shield-dead? space s)
  (or ((obj-age space s) . > . SHIELD_DEATH)
      ((shield-energy space s) . < . 1)))


(define (reduce-shield! space s damage)
  (set-shield-e! s (- (shield-e s) (/ damage (linear-fade (obj-age space s) SHIELD_LIFE SHIELD_DEATH))))
  (when ((shield-energy space s) . < . 1)
    (set-space-objects! space (remove s (space-objects space)))))


(define (draw-shield dc space center s)
  (define-values (x y) (recenter center s))
  (define cc (linear-color "blue" "blue" 1.0 (linear-fade (obj-age space s) SHIELD_LIFE SHIELD_DEATH)))
  (send dc set-pen cc (* 6.0 (shield-sigmoid space s)) 'solid)
  (define len (shield-length space s))
  (keep-transform dc
    (send dc translate x (- y))
    (send dc rotate (posvel-r (obj-posvel s)))
    (send dc draw-line 0 (* -0.5 len) 0 (* 0.5 len))))

