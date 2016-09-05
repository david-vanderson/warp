#lang racket/base

(require racket/class
         racket/draw)

(require "defs.rkt"
         "utils.rkt"
         "draw-utils.rkt")

(provide (all-defined-out))


(define PLASMA_LIFE 3000)  ; ms after which plasma starts fading
(define PLASMA_FADE 2.0)  ; energy loss per second after PLASMA_LIFE

(define plasma-bitmap #f)

(define (load-plasma)
  (set! plasma-bitmap (load-bitmap "plasma")))


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


(define (draw-plasma dc p space)
  (keep-transform dc
    (center-on dc p)
    (send dc scale
          (/ (plasma-radius space p) (send plasma-bitmap get-width) 0.5)
          (/ (plasma-radius space p) (send plasma-bitmap get-height) 0.5))
    (define cycle 1000.0)
    (define t (modulo (obj-age space p) cycle))
    (define rot (* 2pi (/ t cycle)))
    (send dc rotate rot)
    (send dc draw-bitmap
          plasma-bitmap
          (- (/ (send plasma-bitmap get-width) 2))
          (- (/ (send plasma-bitmap get-height) 2)))))

