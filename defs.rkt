#lang racket/base

(require racket/math
         racket/runtime-path)

(provide (all-defined-out))

(define VERSION 6)  ; client will bomb if version doesn't match server

(define-runtime-path IMAGEDIR "images")

(define CLIENT_SPECIAL? #f)  ; when #t client will show annotations with #f for faction
(define CLIENT_OUTPUT_TIME #f)
(define COMPRESS #t)
(define PORT 22381)
(define TICK 33)  ; ms time slice for physics, also determines max client frame rate
(define AI_INTERVAL 1000)  ; ms between ai runs (at least)
(define AHEAD_THRESHOLD 100)  ; if client is ahead by this many ms you see it on the screen
(define BUTTON_PRESS_TIME 100)  ; time a button is shown as pressed
(define PLAYER_RATIO 0.666)  ; how much extra does each player add to engines/warp

(define TEXTH (if (equal? 'macosx (system-type)) 16.0 12.0))

; distance between ships (so add hit-distance)
(define AI_HIT_CLOSE 50.0)
(define AI_STRAT_TOO_CLOSE 100.0)
(define AI_STRAT_TOO_FAR 200.0)

(define 2pi (* 2 pi))
(define pi/2 (* 0.5 pi))

(define nocolor "hotpink")  ; used with a transparent pen/brush

; screen starts gray (except for start screen, help screen)
(define LAYER_FOW_RADAR 0)  ; paint black everywhere you can see with radar
(define LAYER_FOW_BLOCK 1)  ; paint gray everywhere that blocks radar
(define LAYER_FOW_VISIBLE 2)  ; paint black everywhere you can see visually
(define LAYER_MAP 3)  ; map lines, annotations, stars, backeffects
(define LAYER_SHIPS 4)  ; ships, plasmas, normal objects
(define LAYER_EFFECTS 5)  ; explosions, overlays, hp bars
; when in hangar:
; - ships and effects go down to map
; - hangar background is on layer_ships
; - hangar stuff is on layer_effects
(define LAYER_UI 6)  ; buttons
(define LAYER_UI_TEXT 7)  ; button text/fill
(define LAYER_NUM 8)

(define PLASMA_SPEED 60.0)
(define SHIELD_SPEED 60.0)

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

; clients set this to a function that's called whenever they change seats
; - used to clear out held and pressed button states
(define player-cleanup-client! (make-parameter (lambda (pid) #f)))

; debugging
(define debug-num (make-parameter 0))

;; Game State

(struct ob (id) #:mutable #:prefab)
; id is used to uniquely identify any game object

(struct posvel (t x y r dx dy dr) #:mutable #:prefab)
; t is the last spacetime that this posvel was sent to clients

(struct obj ob (start-time alive? neb posvel) #:mutable #:prefab)
; start-time is seconds since scenario start that this object was created
;  - used for animations
; alive? is #t normally
; - set to #f if this obj needs to be removed
; - used to support delayed removal of top-level objs
; neb is 0-1, 0 is inside nebula, 1 is outside
; - calculated during collision, so updated each tick
; if posvel is #f, then this obj is inside something else
; if posvel is not #f, then this is a top-level object that is drawn

(define (obj-x obj) (posvel-x (obj-posvel obj)))
(define (obj-y obj) (posvel-y (obj-posvel obj)))
(define (obj-r obj) (posvel-r (obj-posvel obj)))
(define (obj-dx obj) (posvel-dx (obj-posvel obj)))
(define (obj-dy obj) (posvel-dy (obj-posvel obj)))
(define (obj-dr obj) (posvel-dr (obj-posvel obj)))

; use when you need an obj for convenience but only use it for x,y
(define (pvobj x y [r #f])
  (obj 'pvobj #f #f #f (posvel #f x y r #f #f #f)))

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
  (append (list (tool (next-id) 'engine engine-power engine-on? v #f '()))
          (if turn-power
              (list (tool (next-id) 'turnleft turn-power #f #t #f '())
                    (tool (next-id) 'turnright turn-power #f #t #f '())
                    (tool (next-id) 'steer turn-power #f #f #f '()))
              '())
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

(define (tool-mine power)
  (tool (next-id) 'mine power #f #t #f '()))

(define (tool-warp speed threshold)
  (tool (next-id) 'warp (list speed threshold 0.0) #f #t #t '()))

(define (tool-regen hp-per-sec)
  (tool (next-id) 'regen hp-per-sec #f #f #t '()))

(define (tool-factory points ships)
  (tool (next-id) 'factory (list points ships) #f #t #t '()))


(define MOUSE_TOOLS '(pbolt))

(struct shbolt tool (shield-size aim) #:mutable #:prefab)
; shield-size is how big of a shield we shoot
; aim is like for pbolt

(struct overlay (sym fow?) #:mutable #:prefab)

(struct ship obj (type name faction engine-name con maxcon mass drag start
                  radar visible price invincible? sprite-size radius tools
                  playerids hangar overlays cargo dmgfx
                  ai ai-time ai-freq ai-strategy ai-strat-time) #:mutable #:prefab)
; type is the symbol for the sprite
; name is the name of the ship
; faction is the name that this ship belongs to
; engine-sym is the symbol for the engine output sprite class
; con (containment) is how much health you have left
; maxcon is the max containment you can have
; mass controls how you bump into other ships
; drag is the coeffecient for how fast this ship slows down
; start is if you can start on this ship
; radar is radius it can see (not into nebula)
; visible is radius it can see (into nebula)
; price is how much material it takes to construct this ship, or how much you get for scrapping
; - #f means you can't scrap it
; invincible? is #t if this ship can't take damage (asteroids, spawning ships)
; - #f for asteroids, missiles, cannonballs, etc.
; sprite-size is how many meters wide the sprite should show as
; radius is how big your hit area is
; tools is a list of the systems available on this ship
; players is a list of the player ids on this ship
; hangar is list of ships in the hangar or #f if this ship has no hangar
; overlays is an assoc list of (faction . overlay struct)
; cargo is stuff you're carrying
; dmgfx is size of dmgfx affecting this ship
; ai says what kind of ai the ship has:
; - #f means none
; - 'empty means ai when no players are aboard
; - 'always means ai always
; ai-time is last space-time the ai ran
; ai-freq is ms between when we should run the ai
; ai-strategy is a list of strategies, do them in order
; ai-strat-time is the time that we last got new strategies
; - used to know "how long have we been doing the same thing"?

(define (ship-players space s)
  (for/list ((idx (in-list (ship-playerids s))))
    (findf (lambda (o) (equal? idx (ob-id o))) (space-players space))))

(define (ship-strategy s) (if (null? (ship-ai-strategy s)) #f (car (ship-ai-strategy s))))

(struct spacesuit ship () #:mutable #:prefab)
(struct spaceship ship () #:mutable #:prefab)
(struct probe ship () #:mutable #:prefab)
(struct missile ship () #:mutable #:prefab)
(struct cannonball ship () #:mutable #:prefab)
(struct mine ship () #:mutable #:prefab)

(struct plasma obj (e) #:mutable #:prefab)
; e energy

(define (make-nebula t x y rad dr)
  (nebula (next-id) t #t 1.0 (posvel #f x y 0.0 0.0 0.0 dr) rad))
(struct nebula obj (radius) #:mutable #:prefab)
; radius tells you how big this nebula is

(struct fow (x y radar visible) #:transparent)
; describes a disc visible to the player
; - x,y is center
; - radar is radius we can see things normally (they disappear in nebula)
; - visible is the radius we can see things even in nebula

(struct explosion obj (size maxsize expand dmg-rate) #:mutable #:prefab)
; disc where everything touching it takes damage
; size is current radius
; maxsize is radius where it stops growing
; expand is how fast radius grows per sec
; dmg is how much damage per second things take when they touch it

(define (make-explosion space x y size max speed dmg)
  (explosion (next-id) (space-time space) #t 1.0
             (posvel (space-time space) x y 0.0 0.0 0.0 0.0)
             size max speed dmg))

(struct effect obj (size duration) #:mutable #:prefab)

(struct backeffect effect () #:mutable #:prefab)
; effect where we want to render it behind everything else (like engine output)

(struct shield obj (e) #:mutable #:prefab)
; e is energy

(define (make-upgrade start-time type color life pv)
  (upgrade (next-id) start-time #t 1.0 pv type color life))

(struct upgrade obj (type color life) #:mutable #:prefab)
; type is symbol saying what it does
; - ugprades are handled in the scenario on-tick, so the scenario can put whatever
; color is string or make-color, what it looks like in space
; life is ms the upgrade should stay before disappearing or #f if doesn't die

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

(struct ann obj (tab? faction txt) #:mutable #:prefab)
; map annotation
; if tab? is #t, only see it if you press tab
; if faction is #t, everyone sees it
; - if #f, only hacked clients see it (prevent accidental restarting scenarios in large groups)
; - otherwise only if they have the same faction

(define (make-ann-button x y w h txt msg
                         #:pos [pos 'center]
                         #:tab? [tab? #f]
                         #:faction [faction #t])
  (ann-button (next-id) 0 #t 1.0 (posvel #f x y 0 w h pos)
              tab? faction txt msg))
(struct ann-button ann (msg) #:mutable #:prefab)
; clickable button
; obj-x/y is center of button offset from pos (in canon coords)
; - pos of 'center is offset from center of screen
; - pos of 'topleft is offset from topleft of screen
; obj-dx/dy is size of button (in canon coords)
; text is what the button says
; msg is used to easily know which button was clicked
;  - gets delivered to the scenario's on-message as a (anncmd client-id ann-button-id) struct

(define (make-ann-text x y start life gone txt
                       #:pos [pos 'center])
  (ann-text (next-id) start #t 1.0 (posvel #f x y 0 0 0 pos)
            #f #t txt life gone))
(struct ann-text ann (life gone) #:mutable #:prefab)
; text annotation
; obj-x/y is top-left corner (in canon coords)
; life is msec to show the text before fade
; - if life is #f, show forever
; gone is ms when it's completely faded

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

(struct anncmd (pid id) #:mutable #:prefab)
; for things that the server will pick out before sending to apply-change!
; pid is the player id who sent this command
; id points to a annbutton

(struct chfaction (id newf) #:mutable #:prefab)

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
; - 'jump means player is jumping out of their ship
; - 'restart means player is restarting from spacesuit

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

(define (make-message ownspace msg [life 5000] [gone 10000])
  (message (next-id) (space-time ownspace) #t 1.0 #f msg life gone))
(struct message obj (msg life gone) #:mutable #:prefab)
; msg is the text to display
; life is ms time to display before fade
; gone is ms time when it fades completely

(struct chstat (id what val) #:mutable #:prefab)
; id is of the ship
; what is a symbol describing what to change
; val is the new value

;; UI

(struct button (draw key ctrl? x y width height label f) #:mutable #:prefab)
; draw is:
;  'normal - draw button and respond to clicks
;  'disabled - draw button disabled and no clicks
;  'outline - draw button outline and text and respond to clicks
;  'hidden - draw nothing, respond to clicks
;  'dmg - draw offline button, no clicks
; key is the hotkey for this button or unique id
; - id is used for pressed animation
; ctrl? is #t if the hotkey is ctrl-<hotkey>
; x y is bottom left corner
; if height is #f, then x y is center of circle with radius width
; label is what is written on the button
; f is function to call when the button is clicked or key pressed
;  - takes two args x y of where in the button the click was or <key-code> #f if key pressed

(struct press (key time) #:prefab)
; key is the keyboard shortcut (for id purposes)
; time is current-milliseconds when the button was pressed

(struct holdbutton button (frelease) #:mutable #:prefab)
; button that responds to click-hold-release instead of click
; button-f is run on mouse/key down
; frelease is run on mouse/key up (or if you leave the pod)

(struct hold (held key frelease) #:prefab)
; held is the keyboard shortcut (or 'mouse)
; key is the keyboard shortcut (for id purposes)
; frelease is the holdbutton-frelease function
