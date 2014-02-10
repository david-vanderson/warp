#lang racket/base

(require racket/math)

(provide (all-defined-out))

(define PORT 22381)
(define TICK 33)  ; ms time slice for physics, also determines max client frame rate
(define AI_TICK 200)  ; ms time slice for ai
(define WIDTH 1024)  ; how many meters wide is the screen view
(define HEIGHT 768)  ; how many meters tall is the screen view
(define LEFT (/ (- WIDTH) 2))  ; left edge of canonical view
(define BOTTOM (/ (- HEIGHT) 2))  ; bottom edge of canonical view
(define DRAG_COEF .7)  ; lose X% of your velocity / sec
(define RACC .5)  ; gain X radians / sec / sec
(define R_DRAG_COEF .7)  ; lose X% of your velocity / sec
(define 2pi (* 2 pi))
(define pi/2 (* 0.5 pi))
(define bgcolor "black")
(define fgcolor "white")
(define nocolor "whitesmoke")  ; used with a transparent pen/brush

(define PLASMA_SPEED 60)
(define SHIELD_SPEED 40)
(define MAX_POD_ENERGY 100)

(define next-id
  (let ((id 0))
    (lambda ()
      (set! id (add1 id))
      id)))

;; Game State

(struct ob (id) #:mutable #:prefab)
; id is used to uniquely identify any game object

(struct posvel (t x y r dx dy dr) #:mutable #:prefab)
; t is the last spacetime that this posvel was sent to clients

(struct obj ob (start-time posvel) #:mutable #:prefab)
; start-time is seconds since scenario start that this object was created
;  - used for animations
; if posvel is #f, then this obj is inside something else
; if posvel is not #f, then this is a top-level object that is drawn

(struct player ob (name) #:mutable #:prefab)
; name is what is shown in UIs

(struct role ob (player npc?) #:mutable #:prefab)
; the knobs and dials that a player can adjust
; player is #f if this role is unoccupied
; npc? is #t if this role is controlled by the computer when a player is absent

(struct pod ob (role angle dist facing spread energy) #:mutable #:prefab)
; angle/dist is where this pod is with respect to the ship
; facing is where the pod is directed towards (with respect to the ship)
; - if #f, then ignore angle/dist when calculating the center of this pod's screen
; spread is angle within which we can shoot (centered on facing)
; energy is how much is in our batteries

(struct multipod pod (start? roles) #:mutable #:prefab)
; a place for any number of players to sit, roles are created as needed from pod-role
; start? is #t if new players can start in this multipod


(struct crewer role () #:mutable #:prefab)
; special role used to contain players while they are choosing their next role

(struct hangar crewer () #:mutable #:prefab)
; special role used when players are in the hangar of a ship choosing their next role


(struct hangarpod multipod (ships) #:mutable #:prefab)
; ships is a list of the ships inside the hangar

(struct observer role () #:mutable #:prefab)
; role where you don't do anything but you see the "captain's view"

(struct helm pod () #:mutable #:prefab)

(struct pilot role (course fore launch) #:mutable #:prefab)
; course is angle pilot wants to point at
; if fore is #t, main thrusters are firing
; launch is #t if pilot wants to launch from a hangar

(struct weapon pod () #:mutable #:prefab)
; angle is with respect to the ship/pod we are one
; spread is the angle within which we can shoot (centered on angle)

(struct weapons role (fire) #:mutable #:prefab)
; fire is an angle if we want to shoot a plasma (at that angle)

(struct tactical pod () #:mutable #:prefab)
; angle is with respect to the ship/pod we are one

(struct tactics role (shield) #:mutable #:prefab)
; shield is an angle if we want to shoot a shield barrier (at that angle)

(struct ship obj (name faction crew reactor containment pods) #:mutable #:prefab)
; faction is the name that this ship belongs to
; crew is a multipod for players choosing their next role
; reactor is the energy produced by the reactor
; containment is the reactor health left
; pods is a list of all the pods on the ship

(struct plasma obj (e ownship-id) #:mutable #:prefab)
; e is base energy uncorrected for age
; ownship is id of the ship that fired it, or #f if it belongs to no ship

(struct effect obj (size duration) #:mutable #:prefab)

(struct backeffect effect () #:mutable #:prefab)
; effect where we want to render it behind everything else (like engine output)

(struct shield obj (e length) #:mutable #:prefab)
; e is base energy uncorrected for age
; length is the size of the shield

(struct space (time width height objects) #:mutable #:prefab)
; time is msec since the scenario started


;; Changes
;; client sends a single change at a time, server sends a list to clients

;; Most changes are just role? structs, but here are the exceptions

(struct role-change (player from to) #:mutable #:prefab)
; from and to are a role? id, multipod? id, or #f to go to choosing starting multipod


;; Update from server to client

(struct update (time changes pvs) #:mutable #:prefab)
; time is ms since scenario started
; changes is list of above structs
; pvs is a list of pvupdates

(struct chadd (o) #:mutable #:prefab)
; o is the new object to add to space-objects

(struct chmov (id from to pv) #:mutable #:prefab)
; id is of the object to move
; from/to are ids of objects, #f means space-objects
; pv is the new posvel to use for this object

(struct chdam (id damage) #:mutable #:prefab)
; id is of the object that is being damaged
; damage is the amount

(struct cherg (id e) #:mutable #:prefab)
; id is of the pod whose energy is being changed
; e is the additional energy (usually negative)

(struct pvupdate (id pv) #:mutable #:prefab)
; id is the object we want to update
; pv is the new posvel




;; UI

(struct button (x y width height left-inset top-inset name label) #:mutable #:prefab)
; x y width height are 0,0 bottom left corner 1,1 top right
; name is used internally
; label is what is written on the button




(define leave-button (button (/ (- WIDTH) 2) (/ (- HEIGHT) 2) 60 30 5 5 "leave" "Leave"))



;; Utilities


(define (copy-role r)
  (cond
    ((observer? r) (struct-copy observer r))
    ((hangar? r) (struct-copy hangar r))
    ((crewer? r) (struct-copy crewer r))
    ((pilot? r) (struct-copy pilot r))
    ((weapons? r) (struct-copy weapons r))
    ((tactics? r) (struct-copy tactics r))
    (else (error "copy-role hit ELSE clause, role:\n" r))))


(define (role-name role)
  (cond ((hangar? role) "Hangar")
        ((crewer? role) "Crewer")
        ((pilot? role) "Pilot")
        ((observer? role) "Observer")
        ((weapons? role) "Weapons")
        ((tactics? role) "Tactics")
        (else "Unknown")))


(define (get-children o)
  ;(printf "get-children: ~v\n" o)
  (cond
    ((or (player? o)
         (plasma? o)
         (shield? o)
         (effect? o))
     (list))
    ((space? o) (space-objects o))
    ((ship? o) (filter values (cons (ship-crew o) (ship-pods o))))
    ((hangarpod? o) (append (multipod-roles o) (hangarpod-ships o)))
    ((multipod? o) (multipod-roles o))
    ((pod? o) (list (pod-role o)))
    ((role? o) (filter values (list (role-player o))))
    (else
     (printf "get-children hit ELSE clause, o ~v\n" o)
     (error)
     (list))))

;(player (next-id) "Andrea")

(define (big-ship name faction (x 0) (y 0) (r 0)
                  (fore? #f) (hangar? #f) (npc-crew? #f) (npc-helm? #f)
                  (npc-weapons? #f) (npc-tactical? #f))
  (ship (next-id) 0 (if hangar? (posvel 0 x y r 0 0 0) #f) name faction
        (multipod (next-id) (crewer (next-id) #f #f) #f #f #f #f 0 (not npc-crew?) '())
        10 100
        (list
         (helm (next-id) (pilot (next-id) #f npc-helm? r fore? #f) 0 0 #f #f 0)
         (multipod (next-id) (observer (next-id) #f #f) 0 10 #f #f 0 #t '())
         (hangarpod (next-id) (hangar (next-id) #f #f) 0 -10 #f #f 0 #f '()
                    (if hangar?
                        (list (big-ship (string-append name "2") faction)
                              (big-ship (string-append name "3") faction))
                        '()))
         (weapon (next-id) (weapons (next-id) #f npc-weapons? #f) (degrees->radians 21.8) 21.5 0 (* 0.8 pi) 0)
         (tactical (next-id) (tactics (next-id) #f npc-tactical? #f) (degrees->radians -21.8) 21.5 0 (* 0.8 pi) 0)
         )
        ))
