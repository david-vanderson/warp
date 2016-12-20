#lang racket/base

(require racket/class
         racket/draw)

(require "defs.rkt"
         "utils.rkt"
         "draw-utils.rkt")

(provide (all-defined-out))


(define SHIELD_LIFE 5000)  ; ms after which shield starts fading
(define SHIELD_FADE 1.0)  ; energy loss per second after SHIELD_LIFE


(define (shield-energy space s)
  (- (shield-e s) (* (max 0.0 (- (obj-age space s) SHIELD_LIFE)) (/ SHIELD_FADE 1000.0))))

(define (shield-sigmoid space s)
  (sigmoid (shield-energy space s) 12))

(define (shield-length space s)
  (* 30.0 (shield-sigmoid space s)))

(define (shield-dead? space s)
  ((shield-energy space s) . < . 1))


(define (reduce-shield! space s damage)
  (set-shield-e! s (- (shield-e s) damage))
  (when ((shield-energy space s) . < . 1)
    (set-space-objects! space (remove s (space-objects space)))))


(define (draw-shield csd center scale space s fowa)
  (obj-sprite s csd center scale LAYER_SHIPS 'shield (shield-length space s) fowa (obj-r s) "blue"))

