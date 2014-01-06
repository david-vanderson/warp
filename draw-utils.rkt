#lang racket/base

(require racket/class)

(require "defs.rkt")

(provide (all-defined-out))

(define-syntax-rule (keep-transform dc e ...)
  (begin
    (define t (send dc get-transformation))
    (define a (let () e ...))
    (send dc set-transformation t)
    a))


(define (canvas-scale canvas)
  (min (/ (send canvas get-width) WIDTH)
       (/ (send canvas get-height) HEIGHT)))


(define (screen->canon canvas x y)
  (define scale (canvas-scale canvas))
  (define cw (send canvas get-width))
  (define ch (send canvas get-height))
  (values (/ (- x (/ cw 2)) scale)
          (/ (- (/ ch 2) y) scale)))


(define (dc->canon canvas dc x y)
  (define m (send dc get-initial-matrix))
  (define sx (+ (* x (vector-ref m 0)) (* y (vector-ref m 1)) (vector-ref m 4)))
  (define sy (+ (* x (vector-ref m 2)) (* y (vector-ref m 3)) (vector-ref m 5)))
  (screen->canon canvas sx sy))

