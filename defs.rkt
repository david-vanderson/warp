#lang racket/base

(require racket/serialize
         racket/math)

(provide (all-defined-out))

(define WIDTH 1024)  ; how many meters wide is the screen view
(define HEIGHT 768)  ; how many meters tall is the screen view
(define DRAG_COEF .7)  ; lose X% of your velocity / sec
(define RACC .5)  ; gain X radians / sec / sec
(define R_DRAG_COEF .7)  ; lose X% of your velocity / sec
(define 2pi (* 2 pi))


(serializable-struct object (x y r dx dy dr) #:mutable)

(serializable-struct shield (radius color max sections) #:mutable)
; sections is a list of integers uniformly going counter clock-wise around
; each integer is how much shields are in that section, up to max
; section 0 is centered on r=0

(serializable-struct player (name id) #:mutable)
; id uniquely defines this player
; name is what is shown in UIs

(serializable-struct role (player) #:mutable)
; player is #f if this role is unoccupied

(serializable-struct captain role () #:mutable)

(serializable-struct helm role (course fore aft left right) #:mutable)
; course is angle helm wants to point at
; if fore is #t, main thrusters are firing
; if left is #t, thrusters on the right side are firing pushing the ship left

(serializable-struct ship object (helm shields) #:mutable)

(serializable-struct space (objects) #:mutable)