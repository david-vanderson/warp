#lang racket/base

(require racket/math
         racket/function)

(provide (all-defined-out))

(define PORT 22381)
(define TICK 30)  ; ms time slice for physics, also determines max client frame rate
(define SERVER_SEND_DELAY 500)  ; don't send auto updates more often than X ms
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
(define nocolor "whitesmoke")  ; used with a transparent pen/brush

(define next-id
  (let ((id 0))
    (lambda ()
      (set! id (add1 id))
      id)))

;; Game State

(struct posvel (t x y r dx dy dr) #:mutable #:prefab)
; t is the last spacetime that this posvel was sent to clients

(struct obj (id start-time posvel) #:mutable #:prefab)
; id is used to uniquely identify any game object
; start-time is seconds since scenario start that this object was created
;  - used for animations
; if posvel is #f, then this obj is inside something else
; if posvel is not #f, then this is a top-level object that is drawn

(struct plasma obj (e ownship-id) #:mutable #:prefab)
; e is base energy uncorrected for age
; ownship is id of the ship that fired it, or #f if it belongs to no ship

(struct effect obj () #:mutable #:prefab)

(struct backeffect effect () #:mutable #:prefab)
; effect where we want to render it behind everything else (like engine output)

(struct shield obj (e length) #:mutable #:prefab)
; e is base energy uncorrected for age
; length is the size of the shield

(struct player obj (name) #:mutable #:prefab)
; name is what is shown in UIs
; npc is #t if this player is computer controlled, #f if it's a real person

(struct role obj (player) #:mutable #:prefab)
; player is #f if this role is unoccupied

(struct multirole obj (role start? roles) #:mutable #:prefab)
; role is the role that players joining this multirole get
; start? is #t if new players can start in this role
; roles is a list of roles with players

(struct crewer role () #:mutable #:prefab)
; special role used to contain players while they are choosing their next role

(struct hangar crewer () #:mutable #:prefab)
; special role used when players are in the hangar of a ship choosing their next role

(struct pod obj (role angle dist facing spread) #:mutable #:prefab)
; angle/dist is where this pod is with respect to the ship
; facing is where the pod is directed towards (with respect to the ship)
; spread is angle within which we can shoot (centered on facing)

(struct multipod pod (multirole) #:mutable #:prefab)

(struct hangarpod multipod (ships) #:mutable #:prefab)
; ships is a list of the ships inside the hangar

(struct observation multipod () #:mutable #:prefab)

(struct observer role () #:mutable #:prefab)
; role where you don't do anything but you see the "captain's view"

(struct helm pod () #:mutable #:prefab)

(struct pilot role (course fore) #:mutable #:prefab)
; course is angle pilot wants to point at
; if fore is #t, main thrusters are firing

(struct weapon pod () #:mutable #:prefab)
; angle is with respect to the ship/pod we are one
; spread is the angle within which we can shoot (centered on angle)

(struct weapons role (fire) #:mutable #:prefab)
; fire is an angle if we want to shoot a plasma (at that angle)

(struct tactical pod () #:mutable #:prefab)
; angle is with respect to the ship/pod we are one

(struct tactics role (shield) #:mutable #:prefab)
; shield is an angle if we want to shoot a shield barrier (at that angle)

(struct ship obj (name npc? faction crew reactor containment pods) #:mutable #:prefab)
; npc? is #t if this ship is computer controlled
; faction is the name that this ship belongs to
; reactor is the energy produced by the reactor
; containment is the reactor health left
; pods is a list of all the pods on the ship

(struct space (time width height objects) #:mutable #:prefab)
; time is msec since the scenario started


;; Changes
;; client sends a single change at a time, server sends a list to clients

;; Most changes are just role? structs, but here are the exceptions

(struct role-change (player from to) #:mutable #:prefab)
; from and to are a role? id, multirole? id, or #f (no role)


;; Update from server to client

(struct update (time changes pvs) #:mutable #:prefab)
; time is ms since scenario started
; changes is list of above structs
; pvs is a list of pvupdates

(struct chadd (o) #:mutable #:prefab)
; o is the new object to add to space-objects

(struct chdam (id damage) #:mutable #:prefab)
; id is of the object that is being damaged
; damage is the amount

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
    ((or (plasma? o)
         (shield? o)
         (player? o)
         (effect? o))
     (list))
    ((space? o) (space-objects o))
    ((ship? o) (filter values (cons (ship-crew o) (ship-pods o))))
    ((multirole? o) (multirole-roles o))
    ((role? o) (filter values (list (role-player o))))
    ((hangarpod? o) (cons (multipod-multirole o) (hangarpod-ships o)))
    ((multipod? o) (list (multipod-multirole o)))
    ((pod? o) (list (pod-role o)))
    (else
     (printf "get-children hit ELSE clause, o ~v\n" o)
     (error)
     (list))))

;(player (next-id) #f #f "Andrea")

(define (big-ship name npc? faction x y r fore? hangar?)
  (ship (next-id) 0 (posvel 0 x y r 0 0 0) name npc? faction
        (multirole (next-id) #f #f (crewer (next-id) #f #f #f) (not npc?) '())
        110 100
        (list
         (helm (next-id) #f #f
               (pilot (next-id) #f #f #f r fore?)
               0 0 #f #f)
         (observation (next-id) #f #f #f 0 10 #f #f
                      (multirole (next-id) #f #f (observer (next-id) #f #f #f) #f '()))
         (hangarpod (next-id) #f #f #f 0 -10 #f #f
                    (multirole (next-id) #f #f (hangar (next-id) #f #f #f) #f '())
                    (if hangar?
                        (list (big-ship (string-append name "2") npc? faction 0 0 0 #f #f)
                              (big-ship (string-append name "3") npc? faction 0 0 0 #f #f))
                        '()))
         (weapon (next-id) #f #f
                 (weapons (next-id) #f #f #f #f)
                 (/ pi 4) (sqrt 200) (/ pi 4) (/ pi 2))
         (tactical (next-id) #f #f
                   (tactics (next-id) #f #f #f #f)
                   (* 3/4 pi) (sqrt 200) (* 3/4 pi) (/ pi 2))
         )
        ))
