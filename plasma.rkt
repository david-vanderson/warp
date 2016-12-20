#lang racket/base

(require racket/class
         mode-lambda/static
         mode-lambda
         racket/draw)

(require "defs.rkt"
         "utils.rkt"
         "draw-utils.rkt")

(provide (all-defined-out))


(define PLASMA_LIFE 3000)  ; ms after which plasma starts fading
(define PLASMA_FADE 2.0)  ; energy loss per second after PLASMA_LIFE


(define (plasma-energy space p)
  (- (plasma-e p) (* (max 0.0 (- (obj-age space p) PLASMA_LIFE)) (/ PLASMA_FADE 1000.0))))


(define (plasma-radius space p)
  (/ (plasma-energy space p) 2))


(define (plasma-dead? space p)
  ((plasma-energy space p) . < . 1))


(define (reduce-plasma! space p damage)
  (set-plasma-e! p (- (plasma-e p) damage))
  (when (plasma-dead? space p)
    (set-space-objects! space (remove p (space-objects space)))))


(define (draw-plasma csd center scale p space fowa)
  (define-values (x y) (obj->screen p center scale))
  (define cycle 1000.0)
  (define t (modulo (obj-age space p) cycle))
  (define rot (* 2pi (/ t cycle)))
  (obj-sprite p csd center scale LAYER_SHIPS 'plasma (* 2.0 (plasma-radius space p)) fowa rot "black"))

