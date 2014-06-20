#lang racket/base

(require racket/math)

(provide (all-defined-out))

(define PORT 22381)
(define TICK 33)  ; ms time slice for physics, also determines max client frame rate
(define AI_TICK 100)  ; ms time slice for ai
(define WIDTH 1024)  ; how many meters wide is the screen view
(define HEIGHT 768)  ; how many meters tall is the screen view
(define LEFT (/ (- WIDTH) 2))  ; left edge of canonical view
(define BOTTOM (/ (- HEIGHT) 2))  ; bottom edge of canonical view
(define 2pi (* 2 pi))
(define pi/2 (* 0.5 pi))
(define AI_GOTO_DIST 50)  ; if you are this close you've hit it
(define bgcolor "black")
(define fgcolor "white")
(define nocolor "hotpink")  ; used with a transparent pen/brush

(define PLASMA_SPEED 60)
(define SHIELD_SPEED 40)
(define MAX_POD_ENERGY 100)
(define MSG_FADE_TIME 10000)

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

(struct pilot role (course fore launch dock) #:mutable #:prefab)
; course is angle pilot wants to point at
; if fore is #t, main thrusters are firing
; launch is #t if pilot wants to launch from a hangar
; dock is #t if the pilot wants to dock with the next ship they hits

(struct weapon pod () #:mutable #:prefab)
; angle is with respect to the ship/pod we are one
; spread is the angle within which we can shoot (centered on angle)

(struct weapons role (fire) #:mutable #:prefab)
; fire is an angle if we want to shoot a plasma (at that angle)

(struct tactical pod () #:mutable #:prefab)
; angle is with respect to the ship/pod we are one

(struct tactics role (shield) #:mutable #:prefab)
; shield is an angle if we want to shoot a shield barrier (at that angle)

(struct stats ob (type name faction power con maxcon radius mass thrust rthrust) #:mutable #:prefab)
; carries all the stats for a ship
; name is the name of the ship
; faction is the name that this ship belongs to
; power is energy per second the engine produces
; con (containment) is how much health you have left
; maxcon is the max containment you can have
; radius is how big your hit area is
; mass controls how you bump into other ships
; thrust is how much force your main engines produce
; rthrust is how much force your turning engines produce

(struct ship obj (stats crew pods ai-strategy) #:mutable #:prefab)
; crew is a multipod for players choosing their next role
; pods is a list of all the pods on the ship
; ai-strategy is a list of strategies, do them in order

(define (ship-name s) (stats-name (ship-stats s)))
(define (ship-type s) (stats-type (ship-stats s)))
(define (ship-faction s) (stats-faction (ship-stats s)))
(define (ship-radius s) (stats-radius (ship-stats s)))
(define (ship-con s) (stats-con (ship-stats s)))
(define (ship-maxcon s) (stats-maxcon (ship-stats s)))
(define (ship-strategy s) (if (null? (ship-ai-strategy s)) #f (car (ship-ai-strategy s))))

(struct spacesuit ship () #:mutable #:prefab)
(struct spaceship ship () #:mutable #:prefab)

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

(struct strategy (t name arg) #:mutable #:prefab)
; name is the state we are in, arg is the parameter(s) for that state


;; Changes
;; Most changes are just role? structs, but here are the exceptions

(struct role-change (player from to newid) #:mutable #:prefab)
; from and to are:
; - role? id or multipod? id
; - #f means we are choosing a starting role
; - newid is set by server, used if changing to a multipod for the id for the new role

(struct update (time changes pvs) #:mutable #:prefab)
; time is ms since scenario started
; changes is list of above structs
; pvs is a list of pvupdates

(struct chadd (o) #:mutable #:prefab)
; o is the new object to add to space-objects

(struct chrm (id) #:mutable #:prefab)
; id is of the object to remove

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

(struct new-strat (ship-id strat) #:mutable #:prefab)
; ship-id is the id of the ship
; strat is the list of new strategies

(struct message obj (msg) #:mutable #:prefab)
; msg is the text to display


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
    ((or (player? o)
         (plasma? o)
         (shield? o)
         (effect? o)
         (message? o))
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
