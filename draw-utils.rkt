#lang racket/base

(require racket/class
         racket/draw)

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


(define (draw-hud-status-text dc linenum str)
  (keep-transform dc
    (send dc translate (- (/ WIDTH 2)) (/ HEIGHT 2))
    (send dc translate 0 (* linenum -20))
    (send dc scale 1 -1)
    (send dc draw-text str 0 0)))


(define (linear-color color1 color2 z alpha)
  (define a (send the-color-database find-color color1))
  (define b (send the-color-database find-color color2))
  (define nz (- 1.0 z))
  (make-color (inexact->exact (floor (+ (* nz (send a red))   (* z (send b red)))))
              (inexact->exact (floor (+ (* nz (send a green)) (* z (send b green)))))
              (inexact->exact (floor (+ (* nz (send a blue))  (* z (send b blue)))))
              alpha))


(define (sigmoid x)
  (- (/ 2.0 (+ 1.0 (exp (- (/ x 20))))) 1.0))
