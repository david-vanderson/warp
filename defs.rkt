#lang racket/base

(require racket/math)

(provide (all-defined-out))

(define PORT 22381)
(define TICK 33)  ; ms time slice for physics, also determines max client frame rate
(define AI_INTERVAL 1000)  ; ms between ai runs (at least)
(define WIDTH 1024.0)  ; how many meters wide is the screen view
(define HEIGHT 768.0)  ; how many meters tall is the screen view
(define LEFT (/ (- WIDTH) 2))  ; left edge of canonical view
(define RIGHT (/ WIDTH 2))
(define TOP (/ HEIGHT 2))
(define BOTTOM (/ (- HEIGHT) 2))
(define 2pi (* 2 pi))
(define pi/2 (* 0.5 pi))
(define AI_GOTO_DIST 50.0)  ; if you are this close you've hit it
(define bgcolor "black")
(define fgcolor "white")
(define nocolor "hotpink")  ; used with a transparent pen/brush

(define PLASMA_SPEED 60.0)
(define SHIELD_SPEED 60.0)
(define MSG_FADE_TIME 10000.0)

(define server? (make-parameter #t))  ; clients set this to #f
(define idimag (make-parameter 0))  ; clients set this to their player id

(define next-id
  (let ((id 0))
    (lambda ()
      (set! id (add1 id))
      (make-rectangular id (idimag)))))


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

(define (obj-x obj) (posvel-x (obj-posvel obj)))
(define (obj-y obj) (posvel-y (obj-posvel obj)))
(define (obj-r obj) (posvel-r (obj-posvel obj)))

(struct player ob (name faction) #:mutable #:prefab)
; name is what is shown in UIs

(struct pod ob (name player npc? angle dist facing spread energy maxe tools) #:mutable #:prefab)
; name is a string that you see and tells you where you are
; player is #f if unoccupied
; npc? is #t if controlled by the computer when a player is absent
; - server also puts ms time in npc? when the ai is running
; angle/dist is where this pod is with respect to the ship
; facing is where the pod is directed towards (with respect to the ship)
; - if #f, then pod doesn't have a direction (can click anywhere)
; spread is angle within which we can shoot (centered on facing)
; energy is how much is in our batteries
; maxe is the capacity of our batteries
; tools is a list of things the player can interact with
(define (pod-e p)
  (max 0 (pod-energy p)))

(struct lounge pod (crew) #:mutable #:prefab)
; crew is a list of players that are in the observation lounge

(struct hangar pod (crew ships) #:mutable #:prefab)
; crew is a list of players that are in the hangar (so they see the ships)
; ships is a list of ships on this ship

(struct tool ob (dmgs) #:mutable #:prefab)
; this is something that a player/ai can interact with
; dmgs is list of dmg structs

(struct lthrust tool (on) #:mutable #:prefab)  ; turn left
(struct rthrust tool (on) #:mutable #:prefab)  ; turn right
(struct fthrust tool (on) #:mutable #:prefab)  ; forward
; on is bool
(struct steer tool (course) #:mutable #:prefab)  ; turn towards course
; course is angle pilot wants to point at
(struct dock tool (on) #:mutable #:prefab)
; on is whether to dock if we hit friendly ship
; dock also does launching
(struct pbolt tool (plasma-size) #:mutable #:prefab)
; plasma-size is how much we fire each time
(struct shbolt tool (shield-size) #:mutable #:prefab)
; shield-size is how big of a shield we shoot

(struct stats ob (type name faction power bat maxbat con maxcon radius mass thrust rthrust radar start) #:mutable #:prefab)
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
; radar is fog of war radius for this ship
; start is if you can start on this ship

(struct ship obj (stats pods ai-strategy dmgfx cargo) #:mutable #:prefab)
; pods is a list of all the pods on the ship
; ai-strategy is a list of strategies, do them in order
; damages is a list of ship-level damage, plus temporary damage effects
; cargo is stuff you're carrying

(define (ship-name s) (stats-name (ship-stats s)))
(define (ship-type s) (stats-type (ship-stats s)))
(define (ship-faction s) (stats-faction (ship-stats s)))
(define (ship-power s) (stats-power (ship-stats s)))
(define (ship-bat s) (stats-bat (ship-stats s)))
(define (ship-maxbat s) (stats-maxbat (ship-stats s)))
(define (ship-con s) (stats-con (ship-stats s)))
(define (ship-maxcon s) (stats-maxcon (ship-stats s)))
(define (ship-radius s) (stats-radius (ship-stats s)))
(define (ship-mass s) (stats-mass (ship-stats s)))
(define (ship-strategy s) (if (null? (ship-ai-strategy s)) #f (car (ship-ai-strategy s))))
(define (ship-radar s) (stats-radar (ship-stats s)))
(define (ship-start s) (stats-start (ship-stats s)))

(struct spacesuit ship () #:mutable #:prefab)
(struct spaceship ship () #:mutable #:prefab)

(struct plasma obj (e ownship-id) #:mutable #:prefab)
; e energy
; ownship is id of the ship that fired it, or #f if it belongs to no ship

(struct effect obj (size duration) #:mutable #:prefab)

(struct backeffect effect () #:mutable #:prefab)
; effect where we want to render it behind everything else (like engine output)

(struct shield obj (e) #:mutable #:prefab)
; e is energy

(struct upgrade obj (type) #:mutable #:prefab)
; type is string saying which part of the ship it upgrades

(struct space (time width height players objects) #:mutable #:prefab)
; time is msec since the scenario started

(struct strategy (t name arg) #:mutable #:prefab)
; name is the state we are in, arg is the parameter(s) for that state

(struct dmgfx obj (type size) #:mutable #:prefab)
; type is a string that tells us how to show this
; size is an int that says how big to show this

(struct dmg ob (type size energy fixing?) #:mutable #:prefab)
; dmg details how a part of a pod is damaged
; type is a string that says what is damaged and how
; size is amount of energy needed to fix
; energy is amount of energy contributed so far
; fixing is bool

(struct ann obj () #:mutable #:prefab)
; map annotation

(struct ann-button ann (w h text msg) #:mutable #:prefab)
; clickable button
; obj-x/y is lower left-hand corner
; w/h is size of button
; text is what the button says
; msg is what is sent to server when a player clicks it
;  - gets delivered to the scenario's on-message as a (anncmd ann-button-id msg) struct


;; Changes

(struct command (id cmd) #:mutable #:prefab)
; general purpose command
; id points to a tool
; cmd is anything, interpreted by the tool

(struct anncmd command () #:mutable #:prefab)
; for commands that the server will pick out before sending to apply-change!

(struct chrole (playerid to) #:mutable #:prefab)
; to is:
; - id of pod
; - #f means we are choosing a starting role
; - "spacesuit" means we are jumping ship

(struct update (time changes pvs) #:mutable #:prefab)
; time is ms since scenario started
; changes is list of above structs
; pvs is a list of pvupdates

(struct chadd (o to) #:mutable #:prefab)
; o is the new object to add to space-objects
; to is id of object to add o to, or #f for top level

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

(struct new-strat (ship-id strats) #:mutable #:prefab)
; ship-id is the id of the ship
; strat is the list of new strategies

(struct message obj (msg) #:mutable #:prefab)
; msg is the text to display

(struct chstats (id newstats) #:mutable #:prefab)
; id is of the ship
; newstats is stats

;; UI

(struct button (draw key x y width height label f) #:mutable #:prefab)
; draw is:
;  'normal - draw button and respond to clicks
;  'disabled - draw button disabled and no clicks
;  'hidden-text - draw only text and respond to clicks
;  'hidden - draw nothing, respond to clicks
; key is the hotkey for this button
; x y is bottom left corner
; if height is #f, then x y is center of circle with radius width
; label is what is written on the button
; f is function to call when the button is clicked or key pressed
;  - takes two args x y of where in the button the click was or <key-code> #f if key pressed

(struct dmgbutton button (frac fixing?) #:mutable #:prefab)
; frac is percentage fixed
; fixing? is #t if this is being fixed