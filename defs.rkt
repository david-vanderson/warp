#lang racket/base

(require racket/serialize
         racket/math
         racket/function)

(provide (all-defined-out))

(define PORT 22381)
(define WIDTH 1024)  ; how many meters wide is the screen view
(define HEIGHT 768)  ; how many meters tall is the screen view
(define DRAG_COEF .7)  ; lose X% of your velocity / sec
(define RACC .5)  ; gain X radians / sec / sec
(define R_DRAG_COEF .7)  ; lose X% of your velocity / sec
(define 2pi (* 2 pi))

(define next-id
  (let ((id 0))
    (lambda ()
      (set! id (add1 id))
      id)))


(struct posvel (x y r dx dy dr) #:mutable #:prefab)

(struct obj (id posvel) #:mutable #:prefab)
; id is used to uniquely identify any game object
; if posvel is #f, then this obj is inside something else
; if posvel is not #f, then this is a top-level object that is drawn

(struct plasma obj (color energy ownship-id shields-hit) #:mutable #:prefab)
; ownship is unique id of the ship that fired it, or #f if it belongs to no ship
;  - plasma will not interact with the shields of ownship
; shields-hit is a list of colors of shields that this plasma has already hit

(struct shield obj (radius color max sections) #:mutable #:prefab)
; sections is a vector of integers uniformly going counter clock-wise around
; each integer is how much shields are in that section, up to max
; section 0 is centered on r=0

(struct player obj (name npc?) #:mutable #:prefab)
; name is what is shown in UIs
; npc is #t if this player is computer controlled, #f if it's a real person

(struct role obj (player) #:mutable #:prefab)
; player is #f if this role is unoccupied

(struct multirole obj (players) #:mutable #:prefab)
; players is a list of players in this role

(struct observers multirole (new-players?) #:mutable #:prefab)
; special role used to contain players while they are choosing their next role
; new-players? is #t if new players can start in this observer role

(struct helm role (course fore aft left right) #:mutable #:prefab)
; course is angle helm wants to point at
; if fore is #t, main thrusters are firing
; if left is #t, thrusters on the right side are firing pushing the ship left

(struct ship obj (name helm observers reactor containment shields) #:mutable #:prefab)
; reactor is the energy produced by the reactor
; containment is the percentage of reactor health left (0-1, starts at 1)
; shields are in radius order starting with the largest radius

(struct space (time sizex sizey objects) #:mutable #:prefab)
; time is seconds since the scenario started
; sizex and sizey are how big space is


(define (get-role stack)
  (cadr stack))

(define (get-center stack)
  (car (reverse stack)))


(define (get-children o)
  (cond
    ((space? o) (space-objects o))
    ((plasma? o) (list))
    ((ship? o) (append
                (list (ship-helm o) (ship-observers o))
                (ship-shields o)))
    ((multirole? o) (multirole-players o))
    ((role? o) (filter values (list (role-player o))))
    (else
     (printf "no children for: ~v\n" o)
     (list))))

; returns a list (stack) of all objects from ownspace
; to the object with the given id (found object last)
; if object is not in space, return #f
(define (search o id (multiple? #f) (stack '()))
  (let/ec found
    (cond
      ((and (not (space? o)) (integer? id) (= id (obj-id o)))
       (list (cons o stack)))
      ((and (not (space? o)) (procedure? id) (id o))
       (list (cons o stack)))
      (else
       (define results
         (for/list ((c (get-children o)))
           (define r (search c id multiple? (if (space? o) stack (cons o stack))))
           (if (and (not multiple?)
                    (not (null? r)))
               (found r)
               r)))
       (filter (negate null?) (apply append results))))))


(define (find-id o id)
  (define r (search o id))
  (if (null? r) #f (car r)))


(define (find-new-player-roles space)
  (append (map (lambda (o)
                 (cond
                   ((ship? o)
                    (if (observers-new-players? (ship-observers o))
                        (list (ship-observers o))
                        (list)))))
               (space-objects space))))


(define (big-ship x y)
  (ship (next-id) (posvel x y (* 0.5 pi) 0 0 0) "ship"
        (helm (next-id) #f #f (* 0.5 pi) #f #f #f #f)
        (observers (next-id) #f '() #t)
        100 1
        (list)
        #;(list 
         (shield (next-id) #f 57 "blue" 100 (make-vector 16 50))
         (shield (next-id) #f 50 "red" 100 (make-vector 16 50)))))
