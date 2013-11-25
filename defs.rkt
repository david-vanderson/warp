#lang racket/base

(provide (all-defined-out))

(define WIDTH 1024)  ; how many meters wide is the screen view
(define HEIGHT 768)  ; how many meters tall is the screen view
(define EPSILON .00001)  ; things lower than this are zero
(define DRAG_COEF .7)  ; lose X% of your velocity / sec
(define RACC .5)  ; gain X radians / sec / sec
(define R_DRAG_COEF .7)  ; lose X% of your velocity / sec

(define show-framerate? #t)


(struct object (x y r dx dy dr) #:prefab #:mutable)

(struct shield (radius color sections) #:prefab #:mutable)
; sections is a list of integers uniformly going counter clock-wise around
; each integer is how much shields are in that section

(struct ship object (shields) #:prefab #:mutable)