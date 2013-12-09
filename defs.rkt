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

;; Game State

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

(struct multirole obj (role start? roles) #:mutable #:prefab)
; role is the role that players joining this multirole get
; roles is a list of roles with players
; start? is #t if new players can start in this role

(struct observer role () #:mutable #:prefab)
; role where you don't do anything but you see the "captain's view"

(struct crewer role () #:mutable #:prefab)
; special role used to contain players while they are choosing their next role

(struct helm role (course fore aft left right) #:mutable #:prefab)
; course is angle helm wants to point at
; if fore is #t, main thrusters are firing
; if left is #t, thrusters on the right side are firing pushing the ship left

(struct ship obj (name helm observers crew reactor containment shields) #:mutable #:prefab)
; reactor is the energy produced by the reactor
; containment is the percentage of reactor health left (0-1, starts at 1)
; shields are in radius order starting with the largest radius

(struct space (time sizex sizey objects) #:mutable #:prefab)
; time is seconds since the scenario started
; sizex and sizey are how big space is


;; Messages

;; Most messages are just role? structs, but here are the exceptions

(struct role-change (player from to) #:mutable #:prefab)
; from and to are a role? id, multirole? id, or #f (no role)


;; UI

(struct button (x y width height name label) #:mutable #:prefab)
; x y width height are 0,0 bottom left corner 1,1 top right
; name is used internally
; label is what is written on the button


;; Utilities

(define (get-role stack)
  (if stack (cadr stack) #f))

(define (get-ship stack)
  (car (memf ship? stack)))

(define (get-center stack)
  (car (reverse stack)))


(define (role-name role)
  (cond ((crewer? role) "Crewer")
        ((helm? role) "Helm")
        ((observer? role) "Observer")
        (else "Unknown")))


(define (get-children o)
  ;(printf "get-children: ~v\n" o)
  (cond
    ((space? o) (space-objects o))
    ((plasma? o) (list))
    ((ship? o) (filter values (append (list (ship-helm o) (ship-observers o) (ship-crew o))
                                      (ship-shields o))))
    ((multirole? o) (multirole-roles o))
    ((role? o) (filter values (list (role-player o))))
    ((shield? o) (list))
    ((player? o) (list))
    (else
     (printf "get-children hit ELSE clause, o ~v\n" o)
     (error)
     (list))))

; returns a list of stacks of all objects from ownspace
; to the object with the given id (found object first)
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

(define (find-all o id)
  (map car (search o id #t)))

(define (find-stack o id)
  (define r (if id (search o id) null))
  (if (null? r) #f (car r)))

(define (find-id o id)
  (define r (find-stack o id))
  (if r (car r) #f))

(define (buttons stack space)
  (define role (if stack (cadr stack) #f))
  (define leave (button 0 0 .1 .05 "leave" "Leave"))
  (cond
    ((not space)
     ; we haven't joined a server yet
     (list))
    ((helm? role)
     (list leave
           (button .2 .1 .1 .05 "fore" (if (helm-fore role) "Stop" "Go"))
           (button .4 .1 .1 .05 "fire" "Fire")))
    ((observer? role)
     (list leave))
    ((crewer? role)
     (define ship-roles
       (find-all (get-ship stack)
                 (lambda (o) (or (multirole? o)
                                 (and (role? o) (not (role-player o)))))))
     (cons 
      leave
      (for/list ((r ship-roles)
                 (i (in-naturals)))
        (cond
          ((role? r)
           (button (* i .15) .05 .14 .05 (obj-id r)
                   (format "~a" (role-name r))))
          ((multirole? r)
           (define role (multirole-role r))
           (button (* i .15) .05 .14 .05 (obj-id r)
                   (format "~a" (role-name role))))))))
    ((not role)
     (define start-stacks
       (search space (lambda (o) (and (multirole? o)
                                      (multirole-start? o))) #t))
     (cons
      leave
      (for/list ((s start-stacks)
                 (i (in-naturals)))
        (define mr (car s))
        (button (* i .15) .05 .14 .05 (obj-id mr)
                (format "~a on ~a" (role-name (multirole-role mr))
                        (ship-name (get-ship s)))))))
    (else
     (error "buttons hit ELSE clause, role\n" role))))


(define (click-button? buttons x y)
  (ormap (lambda (b)
           ;(printf "click-button? ~a ~a\n" x y)
           (and (<= (button-x b) x (+ (button-x b) (button-width b)))
                (<= (button-y b) y (+ (button-y b) (button-height b)))
                (button-name b)))
         buttons))


(define (big-ship x y name)
  (ship (next-id) (posvel x y (* 0.5 pi) 0 0 0) name
        (helm (next-id) #f #f (* 0.5 pi) #f #f #f #f)
        (multirole (next-id) #f
                   (observer (next-id) #f #f) #f '())
        (multirole (next-id) #f
                   (crewer (next-id) #f #f) #t '())
        100 1
        (list 
         (shield (next-id) #f 57 "blue" 100 (make-vector 16 50))
         (shield (next-id) #f 50 "red" 100 (make-vector 16 50)))))
