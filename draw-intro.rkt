#lang racket/base

(require racket/class)

(require "defs.rkt"
         "draw.rkt")

(provide (all-defined-out))


(define (draw-intro dc)
  (keep-transform dc
    
    (send dc scale 1 -1)
    (send dc draw-text "Intro Screen" 0 0)
    
    ; This screen is where you type in your name and the server IP
    
    )
  (list))