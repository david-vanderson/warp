#lang racket/base

(require racket/class)

(require "defs.rkt"
         "draw.rkt")

(provide (all-defined-out))


(define (draw-intro dc)
  (define t (send dc get-transformation))

  (send dc scale 1 -1)
  (send dc draw-text "Intro Screen" 0 0)
  
  ; This screen is where you type in your name and the server IP
  
  (send dc set-transformation t))