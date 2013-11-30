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

(define (random-id)
  (random 4294967087))


(serializable-struct thing (x y r dx dy dr) #:mutable)

(serializable-struct plasma thing (color energy ownship-id shields-hit) #:mutable)
; ownship is unique id of the ship that fired it, or #f if it belongs to no ship
;  - plasma will not interact with the shields of ownship
; shields-hit is a list of colors of shields that this plasma has already hit

(serializable-struct shield (radius color max sections) #:mutable)
; sections is a vector of integers uniformly going counter clock-wise around
; each integer is how much shields are in that section, up to max
; section 0 is centered on r=0

(serializable-struct player (id name) #:mutable)
; id uniquely defines this player
; name is what is shown in UIs

(serializable-struct role (player) #:mutable)
; player is #f if this role is unoccupied

(serializable-struct captain role () #:mutable)

(serializable-struct helm role (course fore aft left right) #:mutable)
; course is angle helm wants to point at
; if fore is #t, main thrusters are firing
; if left is #t, thrusters on the right side are firing pushing the ship left

(serializable-struct ship thing (id helm shields) #:mutable)
; shields are in radius order starting with the largest radius

(serializable-struct space (objects) #:mutable)


(define (get-role stack)
  (cadr (reverse stack)))

(define (get-center stack)
  (cadr stack))

; returns a list (stack) of all objects from ownspace to the player (player last)
; if player is not in space, return #f
(define (find-player obj id)
  (define x
    (cond
      ((space? obj) (filter ship? (space-objects obj)))
      ((ship? obj) (list (ship-helm obj)))
      ((role? obj) (list (role-player obj)))
      ((player? obj) obj)))
  
  (cond
    ((player? x)
     (if (equal? id (player-id x))
         (list x)
         #f))
    (else
     (define found (ormap (lambda (o) (find-player o id)) x))
     (and found (cons obj found)))))