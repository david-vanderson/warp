#lang racket/base

(require racket/class)

(require "defs.rkt"
         "draw.rkt")

(provide (all-defined-out))


(define (draw-intro dc)
  (define t (send dc get-transformation))

  (send dc scale 1 -1)
  (send dc draw-text "Intro Screen" 0 0)
  
  (send dc set-transformation t))