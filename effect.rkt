#lang racket/base

(require racket/class
         racket/draw)

(require "defs.rkt"
         "utils.rkt")

(provide (all-defined-out))


(define EFFECT_DEAD 300)


(define (effect-dead? space e)
  ((obj-age space e) . > . EFFECT_DEAD))


(define (draw-effect dc space center e)
  (define-values (x y) (recenter center e))
  (define size (+ 6.0 (* (/ (obj-age space e) EFFECT_DEAD) 6.0)))
  (define c (send the-color-database find-color "yellow"))
  (define cc (make-color (send c red) (send c green) (send c blue)
                        (- 1.0 (min 1.0 (/ (obj-age space e) EFFECT_DEAD)))))
  (send dc set-pen cc (/ 18.0 size) 'solid)
  (send dc draw-ellipse (- x (/ size 2)) (- y (/ size 2)) size size))

