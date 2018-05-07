#lang racket/base

(require mode-lambda
         mode-lambda/static)

(require "defs.rkt"
         "utils.rkt"
         "draw-utils.rkt")

(provide (all-defined-out))


(define PLASMA_LIFE 3000)  ; ms after which plasma starts fading
(define PLASMA_FADE 2.0)  ; energy loss per second after PLASMA_LIFE

(define (plasma-setup-pre! sd)
  (add-sprite!/file sd 'plasma (string-append "images/plasma.png")))

(define PLASMA_SPRITE_IDX #f)
(define PLASMA_SPRITE_SIZE #f)

(define (plasma-setup-post! csd)
  (set! PLASMA_SPRITE_IDX (sprite-idx csd 'plasma))
  (define w (sprite-width csd PLASMA_SPRITE_IDX))
  (define h (sprite-height csd PLASMA_SPRITE_IDX))
  (set! PLASMA_SPRITE_SIZE (max w h)))

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
    (set-obj-alive?! p #f)
    (when (client?)
      (define e (effect (next-id) (space-time space) #t
                        (posvel (space-time space) (obj-x p) (obj-y p) 0.0 0.0 0.0 0.0)
                        pr 300))
      (append! changes (chadd e #f))))
  changes)


(define (draw-plasma csd center scale p space fowa)
  (define cycle 1000.0)
  (define t (modulo (obj-age space p) cycle))
  (define rot (* 2pi (/ t cycle)))
  (define-values (x y) (obj->screen p center scale))
  (define size (/ (* (max 2.0 (* 2.0 (plasma-radius space p))) scale) PLASMA_SPRITE_SIZE))
  (sprite x y PLASMA_SPRITE_IDX
          #:layer LAYER_SHIPS #:a fowa #:theta (exact->inexact (- rot)) #:m size))
