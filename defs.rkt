#lang racket/base

(require racket/math)

(provide (all-defined-out))

(define DEBUG #f)
(define PORT 22381)
(define TICK 33)  ; ms time slice for physics, also determines max client frame rate
(define AI_INTERVAL 1000)  ; ms between ai runs (at least)
(define AHEAD_THRESHOLD 100)  ; if client is ahead by this many ms you see it on the screen

(define TEXTH (if (equal? 'windows (system-type)) 12.0 16.0))

(define 2pi (* 2 pi))
(define pi/2 (* 0.5 pi))
(define AI_GOTO_DIST 50.0)  ; if you are this close you've hit it
(define bgcolor "black")
(define fgcolor "white")
(define nocolor "hotpink")  ; used with a transparent pen/brush

(define LAYER_FOW_GRAY 0)  ; paint everything gray
(define LAYER_FOW_BLACK 1)  ; paint black everywhere you can see
(define LAYER_MAP 2)  ; map lines, annotations, stars, backeffects
(define LAYER_SHIPS 3)  ; ships, plasmas, normal objects
(define LAYER_EFFECTS 4)  ; explosions, damage dots
(define LAYER_OVERLAY 5)  ; pod tool overlay
; when your ship is on another ship, or when inside a hangar:
; - normal effects are pushed down to LAYER_SHIPS
; - hangar or circular background on LAYER_EFFECTS
; - ships or hangar contents on LAYER_OVERLAY
; - your ship's pod energy arcs/damage dots on LAYER_UI
(define LAYER_UI 6)
(define LAYER_UI_TEXT 7)
(define LAYER_NUM 8)

(define PLASMA_SPEED 60.0)
(define SHIELD_SPEED 60.0)
(define MSG_FADE_TIME 10000.0)

(define DMG_SIZE 50.0)
(define DMG_FIX? #t)  ; whether damages start out with fixing?

(define server? (make-parameter #t))  ; clients set this to #f
(define (client?) (not (server?)))
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

(struct obj ob (start-time alive? posvel) #:mutable #:prefab)
; start-time is seconds since scenario start that this object was created
;  - used for animations
; alive? is #t normally
; - set to #f if this obj needs to be removed
; - used to support delayed removal of top-level objs
; if posvel is #f, then this obj is inside something else
; if posvel is not #f, then this is a top-level object that is drawn

(define (obj-x obj) (posvel-x (obj-posvel obj)))
(define (obj-y obj) (posvel-y (obj-posvel obj)))
(define (obj-r obj) (posvel-r (obj-posvel obj)))
(define (obj-dx obj) (posvel-dx (obj-posvel obj)))
(define (obj-dy obj) (posvel-dy (obj-posvel obj)))
(define (obj-dr obj) (posvel-dr (obj-posvel obj)))

(struct player ob (name faction cmdlevel commands rcid cbid) #:mutable #:prefab)
; name is what is shown in UIs
; faction is string
; cmdlevel increases each time the player moves to a different place (or remote control)
; - the server drops commands with old cmdlevels (stale)
; commands is a list of symbols that say which UI elements are being pressed
; - '(fthrust) : pressing engine forward button
; - '(fthrust warp) : pressing engine and warp buttons
; rcid is #f, or the id of the missile/probe this player is controlling
; cbid is #f, or the id of the cannonball this player is shooting (so they can detonate it)

(struct tool ob (name val rc visible? while-warping? dmgs) #:mutable #:prefab)
; this is something that a player/ai can interact with
; name is a symbol
; val is a value or list of values that parameterize the tool
; normally the tool acts according to how many players have it on
; if there are no players, the rc field controls it
; - used by missles/probes where the players are not colocated
; - also dock only uses rc
; visible? is #t if a player can interact with it
; - on missiles, players can't interact with the engine tool
; - steer tool is only used by ai, players use turnleft/turnright
; while-warping? is #t if this tool can be used while a ship is warping
; - if #f, then tool buttons are shown disabled during warping
; dmgs is list of dmg structs

(define (tools-pilot engine-power engine-on? turn-power
                     #:engine-visible? (v #t)
                     #:dock? (dock? #t))
  (append (list (tool (next-id) 'engine engine-power engine-on? v #f '())
                (tool (next-id) 'turnleft turn-power #f #t #f '())
                (tool (next-id) 'turnright turn-power #f #t #f '())
                (tool (next-id) 'steer turn-power #f #f #f '()))
          (if dock?
              (list (tool (next-id) 'dock #f #t #t #t '()))
              '())))

(define (tool-endrc life)
  (tool (next-id) 'endrc #f life #t #t '()))

(define (tool-pbolt power)
  (tool (next-id) 'pbolt power #f #t #f '()))

(define (tool-probe life)
  (tool (next-id) 'probe life #f #t #t '()))

(define (tool-missile life power)
  (tool (next-id) 'missile (list life power) #f #t #f '()))

(define (tool-cannon power)
  (tool (next-id) 'cannon power #f #t #f '()))

(define (tool-warp speed threshold)
  (tool (next-id) 'warp (list speed threshold 0.0) #f #t #t '()))


(define MOUSE_TOOLS '(pbolt))

(struct shbolt tool (shield-size aim) #:mutable #:prefab)
; shield-size is how big of a shield we shoot
; aim is like for pbolt

(struct stats ob (type name faction con maxcon radius mass radar drag start) #:mutable #:prefab)
; carries all the stats for a ship
; name is the name of the ship
; faction is the name that this ship belongs to
; con (containment) is how much health you have left
; maxcon is the max containment you can have
; radius is how big your hit area is
; mass controls how you bump into other ships
; radar is fog of war radius for this ship, also agro distance
; drag is the coeffecient for how fast this ship slows down
; start is if you can start on this ship

(struct ship obj (stats tools playerids hangar cargo dmgfx ai? ai-freq ai-strategy ai-strat-time) #:mutable #:prefab)
; tools is a list of the systems available on this ship
; players is a list of the player ids on this ship
; hangar is list of ships in the hangar or #f if this ship has no hangar
; cargo is stuff you're carrying
; dmgfx is size of dmgfx affecting this ship
; ai? is #t if the ship is ai when no players are aboard
; ai-freq is ms between when we should run the ai
; ai-strategy is a list of strategies, do them in order
; ai-strat-time is the time that we last got new strategies
; - used to know "how long have we been doing the same thing"?

(define (ship-players space s)
  (for/list ((idx (in-list (ship-playerids s))))
    (findf (lambda (o) (equal? idx (ob-id o))) (space-players space))))

(define (ship-name s) (stats-name (ship-stats s)))
(define (ship-type s) (stats-type (ship-stats s)))
(define (ship-faction s) (stats-faction (ship-stats s)))
(define (ship-con s) (stats-con (ship-stats s)))
(define (ship-maxcon s) (stats-maxcon (ship-stats s)))
(define (ship-radius s) (stats-radius (ship-stats s)))
(define (ship-mass s) (stats-mass (ship-stats s)))
(define (ship-strategy s) (if (null? (ship-ai-strategy s)) #f (car (ship-ai-strategy s))))
(define (ship-radar s) (stats-radar (ship-stats s)))
(define (ship-drag s) (stats-drag (ship-stats s)))
(define (ship-start s) (stats-start (ship-stats s)))

(struct spacesuit ship () #:mutable #:prefab)
(struct spaceship ship () #:mutable #:prefab)
(struct probe ship () #:mutable #:prefab)
(struct missile ship () #:mutable #:prefab)
(struct cannonball ship () #:mutable #:prefab)

(struct plasma obj (e) #:mutable #:prefab)
; e energy

(struct explosion obj (size maxsize expand dmg-rate) #:mutable #:prefab)
; disc where everything touching it takes damage
; size is current radius
; maxsize is radius where it stops growing
; expand is how fast radius grows per sec
; dmg is how much damage per second things take when they touch it

(define (make-explosion space x y size max speed dmg)
  (explosion (next-id) (space-time space) #t
             (posvel (space-time space) x y 0.0 0.0 0.0 0.0)
             size max speed dmg))

(struct effect obj (size duration) #:mutable #:prefab)

(struct backeffect effect () #:mutable #:prefab)
; effect where we want to render it behind everything else (like engine output)

(struct shield obj (e) #:mutable #:prefab)
; e is energy

(struct upgrade obj (type life) #:mutable #:prefab)
; type is string saying which part of the ship it upgrades
; life is ms the upgrade should stay before disappearing of #f if doesn't die

(struct space (id time width height players orders objects) #:mutable #:prefab)
; id is used when we change scenarios to drop stuff coming in for the old space
; time is msec since the scenario started
; orders is a (list (faction ordertree) ...)
; ordertree is an instance of ord

(struct strategy (t name arg) #:mutable #:prefab)
; t is the time we made this strategy
; - used for "has it been a long time since we revisited this old strat?"
; name is the state we are in, arg is the parameter(s) for that state

(struct dmg ob (type size energy fixing?) #:mutable #:prefab)
; dmg details how a part of a pod is damaged
; type is a string that says what is damaged and how
; size is amount of energy needed to fix
; energy is amount of energy contributed so far
; fixing is bool

(struct ann obj (showtab? txt) #:mutable #:prefab)
; map annotation
; if showtab? annotation is only shown when client showtab?
;  - this doesn't affect annotations inside orders

(struct ann-button ann (msg) #:mutable #:prefab)
; clickable button
; obj-x/y is center of button offset from top-left window corner (in canon coords)
; obj-dx/dy is size of button (in canon coords)
; text is what the button says
; msg is what is sent to server when a player clicks it
;  - gets delivered to the scenario's on-message as a (anncmd ann-button-id #f) struct

(struct ann-text ann (life) #:mutable #:prefab)
; text annotation
; obj-x/y is top-left corner (in canon coords)
; life is msec to show the text, then fade then remove
; - if life is #f, show forever

(struct ann-circle ann (radius) #:mutable #:prefab)
; circle on map
; obj-x/y is center

(struct ann-ship ann (id) #:mutable #:prefab)
; annotation pointing out a particular ship


(struct ord (done? text) #:mutable #:prefab)
; base order
; done? is #t if this order is completed (can go back to #f for some orders)
; text tells the player what to do

(struct order ord (anns f) #:mutable #:prefab)
; anns is list of ann structs to show on the map
; f is #f on clients
; - on server it's a (space order -> bool) function that says if the order is done
;   - also gets passed its containing order so it can modify itself
;   - example is when a waypoint is scouted, the function is replaced with (lambda (s o) #t)
;   - so as to "lock" the order as done

(struct ordercomb ord (type orders) #:mutable #:prefab)
; type is symbol saying how to combine
; orders is a list

(struct ordertime order (subtotal start ot) #:mutable #:prefab)
; kind of order that sets a time limit counting down from subtotal
; if text has "~a" inside, it's replaced with 00:00 style time
; - if start is #f, then time is subtotal
; - if start is a time, then time is subtotal - (space-time - start)


;; Changes

(struct command (id level cmd arg) #:mutable #:prefab)
; general purpose command
; id points to the player giving the command
; level is the player-cmdlevel when this command was issued
; cmd is anything, usually a symbol
; arg is #t if turning something on, #f if turning it off

(struct anncmd (id) #:mutable #:prefab)
; for things that the server will pick out before sending to apply-change!
; id points to a annbutton

(struct chfaction (playerid newf) #:mutable #:prefab)
(struct chorders (faction ot) #:mutable #:prefab)
; ot is an instance of ord

(struct update (id time changes pvs) #:mutable #:prefab)
; id is space-id of the ownspace this update is for
; time is ms since scenario started
; changes is list of above structs
; pvs is a list of pvupdates

(struct chadd (o to) #:mutable #:prefab)
; o is the new object to add to space-objects
; to is id of object to add o to, or #f for top level

(struct chrc (pid rcid) #:mutable #:prefab)
; when a player launches a missile or probe,
; server sends this message so they begin remote-controlling it

(struct endrc (pid rcid) #:mutable #:prefab)
(struct endcb (pid cbid) #:mutable #:prefab)
; when a player stops rcing something (or moves/leaves), send this
; also used by the server to clean up
; tries to lookup the player via pid and use player-rcid/player-cbid
; if we can't find the player, use rcid/cbid

(struct chrm (id) #:mutable #:prefab)
; id is of the object to remove

(struct chmov (id to pv) #:mutable #:prefab)
; id is of the object to move
; to is id of where to put the object, #f means top level
; pv is the new posvel to use for this object

(struct chdam (id damage fx) #:mutable #:prefab)
; id is of the object that is being damaged
; damage is the amount
; fx is #t if the thing being damaged should shake the screen

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

(struct chstat (id what val) #:mutable #:prefab)
; id is of the ship
; what is a symbol describing what to change
; val is the new value

;; UI

(struct button (draw key x y width height label f) #:mutable #:prefab)
; draw is:
;  'normal - draw button and respond to clicks
;  'disabled - draw button disabled and no clicks
;  'outline - draw button outline and text and respond to clicks
;  'hidden - draw nothing, respond to clicks
;  'dmg - draw offline button, no clicks
; key is the hotkey for this button
; x y is bottom left corner
; if height is #f, then x y is center of circle with radius width
; label is what is written on the button
; f is function to call when the button is clicked or key pressed
;  - takes two args x y of where in the button the click was or <key-code> #f if key pressed

(struct holdbutton button (frelease) #:mutable #:prefab)
; button that responds to click-hold-release instead of click
; button-f is run on mouse/key down
; frelease is run on mouse/key up (or if you leave the pod)

(struct dmgbutton button (frac fixing?) #:mutable #:prefab)
; frac is percentage fixed
; fixing? is #t if this is being fixed
