#lang racket/base

(require racket/math
         racket/function)

(provide (all-defined-out))

(define PORT 22381)
(define WIDTH 1024)  ; how many meters wide is the screen view
(define HEIGHT 768)  ; how many meters tall is the screen view
(define LEFT (/ (- WIDTH) 2))  ; left edge of canonical view
(define BOTTOM (/ (- HEIGHT) 2))  ; bottom edge of canonical view
(define DRAG_COEF .7)  ; lose X% of your velocity / sec
(define RACC .5)  ; gain X radians / sec / sec
(define R_DRAG_COEF .7)  ; lose X% of your velocity / sec
(define 2pi (* 2 pi))
(define bgcolor "black")
(define fgcolor "white")
(define POD_D 20)  ; meters out from center of ship pods go

(define next-id
  (let ((id 0))
    (lambda ()
      (set! id (add1 id))
      id)))

;; Game State

(struct posvel (x y r dx dy dr) #:mutable #:prefab)

(struct obj (id start-time posvel) #:mutable #:prefab)
; id is used to uniquely identify any game object
; start-time is seconds since scenario start that this object was created
;  - used for animations
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

(struct helm role (course fore) #:mutable #:prefab)
; course is angle helm wants to point at
; if fore is #t, main thrusters are firing

(struct pod obj (console deploying? angle desired-angle dist) #:mutable #:prefab)
; console is the thing that is on this pod
; deploying? is #t if going out, #f if going in
; angle is our deploy angle with respect to the ship
; desired-angle is what the person in this pod wants (set by player)
;  - #f means retract and leave role, #t means retract and wait for click
;  - a number means deploy at that angle
; dist is how far from the ship center we are

(struct console (role) #:mutable #:prefab)

(struct weapon console (angle spread) #:mutable #:prefab)
; angle is with respect to the ship/pod we are one
; spread is the angle within which we can shoot (centered on angle)

(struct weapons role (fire) #:mutable #:prefab)
; fire is an angle if we want to shoot a plasma (at that angle)

(struct ship obj (name helm observers crew reactor containment shields pods) #:mutable #:prefab)
; reactor is the energy produced by the reactor
; containment is the percentage of reactor health left (0-1, starts at 1)
; shields are in radius order starting with the largest radius
; pods is a list of all the pods on the ship

(struct space (time sizex sizey objects) #:mutable #:prefab)
; time is seconds since the scenario started
; sizex and sizey are how big space is


;; Messages

;; Most messages are just role? structs, but here are the exceptions

(struct role-change (player from to) #:mutable #:prefab)
; from and to are a role? id, multirole? id, or #f (no role)

(struct pod-cmd (pod angle) #:mutable #:prefab)
; pod is a pod? id, angle is the new pod-desired-angle


;; UI

(struct button (x y width height left-inset top-inset name label) #:mutable #:prefab)
; x y width height are 0,0 bottom left corner 1,1 top right
; name is used internally
; label is what is written on the button




(define leave-button (button (/ (- WIDTH) 2) (/ (- HEIGHT) 2) 60 30 5 5 "leave" "Leave"))



;; Utilities

(define (get-role stack)
  (if stack (cadr stack) #f))

(define (get-pod stack)
  (car (memf pod? stack)))

(define (get-ship stack)
  (car (memf ship? stack)))

(define (get-center stack)
  (define center (cadr (reverse stack)))
  (define spv (obj-posvel center))
  (define p (memf pod? stack))
  (cond ((and p (not (pod-stowed? (car p))))
         (define pod (car p))
         (obj #f #f (posvel (+ (posvel-x spv)
                                (* (pod-dist pod) (cos (+ (posvel-r spv) (pod-angle pod)))))
                             (+ (posvel-y spv)
                                (* (pod-dist pod) (sin (+ (posvel-r spv) (pod-angle pod)))))
                             (posvel-r spv) 0 0 0)))
        (else center)))

(define (get-space stack)
  (car (reverse stack)))


(define (role-name role)
  (cond ((crewer? role) "Crewer")
        ((helm? role) "Helm")
        ((observer? role) "Observer")
        ((weapons? role) "Weapons")
        (else "Unknown")))


(define (get-children o)
  ;(printf "get-children: ~v\n" o)
  (cond
    ((space? o) (space-objects o))
    ((plasma? o) (list))
    ((ship? o) (filter values (append (list (ship-helm o) (ship-observers o) (ship-crew o))
                                      (ship-shields o) (ship-pods o))))
    ((multirole? o) (multirole-roles o))
    ((role? o) (filter values (list (role-player o))))
    ((shield? o) (list))
    ((player? o) (list))
    ((pod? o) (list (console-role (pod-console o))))
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
           (define r (search c id multiple? (cons o stack)))
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

(define (pod-deployed? pod)
  (and (pod-deploying? pod) (= (pod-dist pod) POD_D)))

(define (pod-stowed? pod)
  (and (not (pod-deploying? pod)) (= (pod-dist pod) 0)))


(define (big-ship x y name)
  (ship (next-id) #f (posvel x y (* 0.5 pi) 0 0 0) name
        (helm (next-id) #f #f #f (* 0.5 pi) #f)
        (multirole (next-id) #f #f
                   (observer (next-id) #f #f #f) #f '())
        (multirole (next-id) #f #f
                   (crewer (next-id) #f #f #f) #t '())
        100 1
        (list 
         (shield (next-id) #f #f 57 "blue" 100 (vector 100 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
         (shield (next-id) #f #f 50 "red" 100 (vector 100 0)))
        (list
         (pod (next-id) #f #f 
                     (weapon (weapons (next-id) #f #f #f #f) 0 (/ pi 2))
                     #f 0 #f 0)
         )
        ))
