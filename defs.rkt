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


(struct thing (x y r dx dy dr) #:mutable #:prefab)

(struct plasma thing (color energy ownship-id shields-hit) #:mutable #:prefab)
; ownship is unique id of the ship that fired it, or #f if it belongs to no ship
;  - plasma will not interact with the shields of ownship
; shields-hit is a list of colors of shields that this plasma has already hit

(struct shield (radius color max sections) #:mutable #:prefab)
; sections is a vector of integers uniformly going counter clock-wise around
; each integer is how much shields are in that section, up to max
; section 0 is centered on r=0

(struct player (id name) #:mutable #:prefab)
; id uniquely defines this player
; name is what is shown in UIs

(struct role (player) #:mutable #:prefab)
; player is #f if this role is unoccupied

(struct captain role () #:mutable #:prefab)

(struct helm role (course fore aft left right) #:mutable #:prefab)
; course is angle helm wants to point at
; if fore is #t, main thrusters are firing
; if left is #t, thrusters on the right side are firing pushing the ship left

(struct ship thing (id helm reactor containment shields) #:mutable #:prefab)
; reactor is the energy produced by the reactor
; containment is the percentage of reactor health left (0-1, starts at 1)
; shields are in radius order starting with the largest radius

(struct space (objects) #:mutable #:prefab)


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
      ((ship? obj) (filter values (list (ship-helm obj))))
      ((role? obj) (filter values (list (role-player obj))))
      ((player? obj) obj)
      (else (printf "find-player error: ~v\n" obj))))
  
  (cond
    ((player? x)
     (if (equal? id (player-id x))
         (list x)
         #f))
    (else
     (define found (ormap (lambda (o) (find-player o id)) x))
     (and found (cons obj found)))))