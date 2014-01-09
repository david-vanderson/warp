#lang racket/base

(require racket/class
         racket/draw)

(require "defs.rkt"
         "utils.rkt"
         "draw-utils.rkt")

(provide (all-defined-out))


(define (reduce-shield! space s damage)
  (set-shield-energy! s (- (shield-energy s) damage))
  (when ((shield-energy s) . < . 1)
    (set-space-objects! space (remove s (space-objects space)))))


(define (shield-color s)
  (define b (send the-color-database find-color "blue"))
  (make-color (send b red)
              (send b green) 
              (send b blue)
              (/ (shield-energy s) 20.0)))


(define (draw-shield dc space center s)
  (define-values (x y) (recenter center s))
  (send dc set-pen (shield-color s) 3 'solid)
  (define len (shield-length s))
  (keep-transform dc
    (send dc translate x y)
    (send dc rotate (- (posvel-r (obj-posvel s))))
    (send dc draw-line 0 (* -0.5 len) 0 (* 0.5 len))))

