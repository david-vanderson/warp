#lang racket/base

(require "defs.rkt"
         "utils.rkt"
         "draw-utils.rkt")

(provide (all-defined-out))


(define PLASMA_LIFE 3000)  ; ms after which plasma starts fading
(define PLASMA_FADE 2.0)  ; energy loss per second after PLASMA_LIFE

(define (plasma-damage space p)
  (plasma-energy space p))

(define (plasma-energy space p)
  (- (plasma-e p) (* (max 0.0 (- (obj-age space p) PLASMA_LIFE)) (/ PLASMA_FADE 1000.0))))

(define (plasma-energy->radius e)
  (/ e 2.0))

(define (plasma-radius space p)
  (plasma-energy->radius (plasma-energy space p)))

(define (plasma-dead? space p)
  ((plasma-energy space p) . < . 1))


(define (reduce-plasma! space p damage)
  (define changes '())
  (define pr (plasma-radius space p))
  (set-plasma-e! p (- (plasma-e p) damage))
  (when (plasma-dead? space p)
    (set-space-objects! space (remove p (space-objects space)))
    (when (client?)
      (define e (effect (next-id) (space-time space)
                        (posvel (space-time space) (obj-x p) (obj-y p) 0.0 0.0 0.0 0.0)
                        pr 300))
      (append! changes (chadd e #f))))
  changes)


(define (draw-plasma csd center scale p space fowa)
  (define-values (x y) (obj->screen p center scale))
  (define cycle 1000.0)
  (define t (modulo (obj-age space p) cycle))
  (define rot (* 2pi (/ t cycle)))
  (obj-sprite p csd center scale LAYER_SHIPS 'plasma
              (max 2.0 (* 2.0 (plasma-radius space p)))
              fowa rot "black"))

