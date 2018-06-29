#lang racket/gui

(require
  racket/fixnum
  mode-lambda
  mode-lambda/static
  mode-lambda/text
  (prefix-in gl: mode-lambda/backend/gl)
  pict
  "defs.rkt"
  "utils.rkt"
  "change.rkt"
  "physics.rkt"
  "draw-utils.rkt"
  "draw.rkt"
  "pilot.rkt"
  "pbolt.rkt"
  "effect.rkt"
  "ships.rkt"
  "upgrade.rkt"
  "dmg.rkt"
  "order.rkt"
  "missile.rkt"
  "warp.rkt"
  "explosion.rkt"
  "plasma.rkt")

(provide start-client)


(define (start-client ip port name
                      #:new-eventspace? (new-eventspace? #f)
                      #:gui? (gui? #t)
                      #:spacebox (sspace #f))
  (cond
    (new-eventspace?
      (thread
        (lambda ()
          (current-eventspace (make-eventspace))
          (start-client* ip port name gui? sspace)
          (yield 'wait))))
    (else
      (start-client* ip port name gui? sspace)
      (yield 'wait))))



(define (start-client* ip port name gui? sspace)
  (server? #f)
  
  (define playing? #t)
  (define server-in-port #f)
  (define server-out-port #f)
  (define server-in-t #f)
  (define server-out-t #f)
  (define meid #f)  ; integer? or #f
  (define ownspace #f)
  (define my-stack #f)
  (define buttons #f)
  (define sprites #f)
  (define active-mouse-tool (box #f))
  (define in-hangar? (box #f))
  (define last-pbolt-time 0)  ; space-time of last pbolt fire

  (define pressed '())
  ; list of press structs from recently pressed buttons
  (define (press! key)
    (prepend! pressed (press key (current-milliseconds))))
  
  (define holding '())
  ; if you are holding a key/mouse button (from a holdbutton?)
  ; this is a list of holds structs

  (player-cleanup-client!
   (lambda (pid)
     (when (equal? pid meid)
       (set! pressed '())
       (for ((h (in-list holding)))
         ((hold-frelease h)))
       (set! holding '()))))
  
  ; if the mouse is over something (other than a button) that when clicked will
  ; do some action, clickcmds is a lambda that returns a list of commands we send
  (define clickcmds #f)
  (define frames '())  ; list of last few frame times
  (define aheads '())  ; list of last few ahead calculations
  (define last-update-time #f)  ; space-time of last update we got from server

  (define showtab #f)  ; tab toggles an overlay showing players and goals
  (define showsector? #f)  ; tilde toggles showing the whole sector or the regular view
  (define zerocenter (obj #f #f #f (posvel #f 0.0 0.0 #f #f #f #f)))

  (define center #f)  ; updated each frame for the click handler and mouse cursor drawing
  (define center-follow? #t)  ; show player position in the center?

  ; when (not center-follow?)
  ; center of the screen when panning
  (define centerxy (obj #f #f #f (posvel #f 0.0 0.0 #f #f #f #f)))
  (define dragxypx '(0 . 0))  ; last xy of drag in pixels
  (define dragstate "none")  ; "none", "start", "drag"
  
  (define scale-play 1.0)  ; scale for zooming

  (define first-scale #t)
  ; when a new scenario happens and you are not on a ship, set this to #t
  ; if you are put onto a ship and this is #t, set to #f and set scale-play to 1.0
  ; effectively, the "first" time you are put on a ship, zoom to normal scale
  
  (define (min-scale)
    (if ownspace
        (min (/ canon-width (space-width ownspace) 1.25)
             (/ canon-height (space-height ownspace) 1.25))
        .01))
  (define (max-scale) 30.0)
    
  (define (set-scale z #:immediate? (imm? #f))
    (define newz (clamp (min-scale) (max-scale) z))
    (set! scale-play newz)
    (when imm? (set! shown-scale scale-play)))
  
  (define shown-scale scale-play)  ; scale that is actually used
  (define (get-scale) (if showsector? (min-scale) shown-scale))
  (define (update-scale)  ; move shown-scale towards the scale we want
    (define diff (abs (- scale-play shown-scale)))
    (if (shown-scale . < . scale-play)
        (set! shown-scale (min scale-play (+ shown-scale (max 0.1 (* 0.4 diff)))))
        (set! shown-scale (max scale-play (- shown-scale (max 0.1 (* 0.4 diff)))))))

  (define canvas #f)
  
 (when gui?
   
  (define (in-button? buttons x y)
    (for/first ((b (in-list buttons))
                #:when (if (button-height b)
                           (and (<= (abs (- (button-x b) x)) (/ (button-width b) 2.0))
                                (<= (abs (- (button-y b) y)) (/ (button-height b) 2.0)))
                           (and (<= (sqrt (+ (* (- x (button-x b)) (- x (button-x b)))
                                             (* (- y (button-y b)) (- y (button-y b)))))
                                    (/ (button-width b) 2.0)))))
      b))

  (define (key-button? buttons key ctrl?)
    (for/first ((b (in-list buttons))
                #:when (and (equal? (button-key b) key)
                            (equal? (button-ctrl? b) ctrl?)
                            (not (member (button-draw b) '(disabled dmg)))))
      b))
  
  
  (define (click canvas event)
    (define-values (x y) (screen->canon canvas (send event get-x) (send event get-y)))
    (define b (in-button? buttons x y))
    ;(printf "click ~a ~a ~a\n" x y b)
    (cond
      (b
       (when (not (member (button-draw b) '(disabled dmg)))
         ;(printf "clicked button ~v\nship is ~v\n" b (if my-stack (get-ship my-stack) #f))
         ((button-f b) (- x (button-x b)) (- y (button-y b)))
         (cond
           ((holdbutton? b)
            (prepend! holding (hold 'mouse (button-key b) (holdbutton-frelease b))))
           (else
            (press! (button-key b))))
         ))
      (clickcmds
       (send-commands (clickcmds)))))
  
  
  (define (draw-screen canvas dc)

    (define t1 0)
    (define t2 0)
    (define t3 0)
    (define t4 0)
    (define t5 0)
    (define t6 0)
    (define t7 0)
    (define t8 0)
    (define t9 0)

    (timeit t1
    (set! buttons '())
    (set! sprites '())
    (define cursordrawn #f)
    (set! clickcmds #f)
    
    (update-scale)
    
    (set! center
          (cond ((or (not ownspace) showsector?)
                 zerocenter)
                ((and my-stack center-follow?)
                 (get-center ownspace my-stack))
                (center-follow?
                 zerocenter)
                (else
                 centerxy)))
    (when center-follow?
      ; record the center so we start from there if center-follow? becomes #f
      (set-posvel-x! (obj-posvel centerxy) (obj-x center))
      (set-posvel-y! (obj-posvel centerxy) (obj-y center)))
    
    (when (not ownspace)
      ; This screen is where you type in your name and the server IP
      (define txt "O")
      (define-values (w h) (textsr txt))
      (for* ((i (in-range (left) (right) w))
             (j (in-range (top) (bottom) h)))
        (prepend! sprites (text-sprite textr textsr txt i j LAYER_UI))))
    )
    
    (when ownspace
      (timeit t2
      (define p (findfid meid (space-players ownspace)))
      (define fac (if p (player-faction p) #f))
      (define ordertree (get-space-orders-for ownspace fac))

      (define myshipid #f)
      (when my-stack
        (define rcship (if (player-rcid p)
                           (find-id ownspace ownspace (player-rcid p)) #f))
        (define topship (or rcship (get-topship my-stack)))
        (set! myshipid (ob-id topship)))
      
      ; make background be fog of war gray
      (prepend! sprites (sprite 0.0 0.0 (sprite-idx csd '100x100) #:layer LAYER_FOW_GRAY
                                #:m (* 2.0 (/ (max canon-width canon-height) 100.0))
                                #:r (send fowcol red)
                                #:g (send fowcol green)
                                #:b (send fowcol blue)))

      ; put a black circle wherever we can see
      (define fowlist
        (for/list ((s (in-list (space-objects ownspace)))
                   #:when (and fac
                               (or (spaceship? s)
                                   (spacesuit? s)
                                   (probe? s))
                               ((faction-check fac (ship-faction s)) . > . 0)))
          (list (obj-x s) (obj-y s) (ship-radar s))))

      (for ((f fowlist))
        (define rad (caddr f))
        (define-values (x y) (xy->screen (car f) (cadr f) center (get-scale)))
        (prepend! sprites (sprite x y (sprite-idx csd 'circle) #:layer LAYER_FOW_BLACK
                                  #:m (* rad (get-scale) (/ 50.0)))))
      )

      (timeit t3
      (prepend! sprites (draw-sector-lines csd center (get-scale) ownspace))

      ; map annotations
      
      ; order annotations
      (when ordertree
        (define a (+ 0.85 (* 0.15 (cycletri (space-time ownspace) 2000))))
        (define bright (linear-color "blue" "blue" 1.0 a))
        (define dim (linear-color "blue" "blue" 1.0 0.85))
        (for-orders ordertree showtab
          (lambda (ot depth highlight?)
            (when (order? ot)
              (for ((a (in-list (order-anns ot))))
                (cond
                  ((ann-circle? a)
                   (define col (if highlight? bright dim))
                   (prepend! sprites
                             (obj-sprite a csd center (get-scale) LAYER_MAP 'circle-outline
                                         (/ (* 2.0 (ann-circle-radius a)) 104)
                                         (send col alpha) 0.0 col))
                   (define-values (x y) (obj->screen a center (get-scale)))
                   (prepend! sprites (textr (ann-txt a)
                                            x y #:layer LAYER_MAP #:a (send col alpha)
                                            #:r (send col red)
                                            #:g (send col green)
                                            #:b (send col blue))))
                  ((ann-ship? a)
                   (define st (find-stack ownspace ownspace (ann-ship-id a)))
                   (when st
                     (define s (get-topship st))
                     (define col (if highlight? bright dim))
                     (prepend! sprites (obj-sprite s csd center (get-scale)
                                                   LAYER_MAP 'target
                                                   (max (/ 35.0 110 (get-scale))
                                                        (/ (* 4.0 (ship-radius s)) 110))
                                                   (send col alpha) 0.0 col))))
                  (else
                   (error "don't know how to draw annotation ~v" a))))))))
      )

      ;(set! sprites (cons (draw-background-stars csd center (get-scale) fowlist) sprites))

      (timeit t4
      (for ((o (in-list (space-objects ownspace)))
            #:when (obj-alive? o))
        (define fowa (if (obj-posvel o) (get-alpha (obj-x o) (obj-y o) fowlist) 1.0))
        (when (fowa . > . 0)
          (define spr (draw-object csd textr textsr center (get-scale) o ownspace myshipid
                                 showtab fowa fac))
          (prepend! sprites spr)))
      )

      ; draw stuff specific to the ship you are on
      ; - stacked if we are on a ship inside another ship
      (timeit t5
      (when my-stack
        (define p (car my-stack))
        (define rcship (if (player-rcid p) (find-id ownspace ownspace (player-rcid p)) #f))
        (define ship (get-ship my-stack))
        (define topship (get-topship my-stack))
        (when (and (unbox in-hangar?) (not (ship-hangar ship)))
          ; just to make sure, maybe our ship had a hangar and it went away?
          (set-box! in-hangar? #f))
        (cond 
          ((unbox in-hangar?)
           ; hangar background
           (define size (* 0.8 (min canon-width canon-height)))
           (prepend! sprites (sprite 0.0 0.0 (sprite-idx csd 'square)
                                     #:layer LAYER_HANGAR
                                     #:m (/ size (sprite-width csd
                                                   (sprite-idx csd 'square)))
                                     #:a 0.75))
           (set! size (+ size 5.0))  ; help cover the corners
           (prepend! sprites (sprite (- (/ size 2.0)) 0.0 (sprite-idx csd '100x1)
                                      #:layer LAYER_HANGAR
                                      #:m (/ size 100.0) #:theta pi/2
                                      #:r 255 #:g 255 #:b 255))
           (prepend! sprites (sprite (/ size 2.0) 0.0 (sprite-idx csd '100x1)
                                      #:layer LAYER_HANGAR
                                      #:m (/ size 100.0) #:theta pi/2
                                      #:r 255 #:g 255 #:b 255))
           (prepend! sprites (sprite 0.0 (- (/ size 2.0)) (sprite-idx csd '100x1)
                                      #:layer LAYER_HANGAR
                                      #:m (/ size 100.0)
                                      #:r 255 #:g 255 #:b 255))
           (prepend! sprites (sprite 0.0 (/ size 2.0) (sprite-idx csd '100x1)
                                      #:layer LAYER_HANGAR
                                      #:m (/ size 100.0)
                                      #:r 255 #:g 255 #:b 255))
           
           ; draw all the ships in the hangar
           (define shipmax (* 2.0 (apply max 1.0 (map (lambda (s) (ship-w s 1.0))
                                                      (ship-hangar ship)))))
           (for ((s (in-list (ship-hangar ship)))
                 (i (in-naturals)))
             (define x (+ (* -0.5 size) 10 (* (quotient i 4) 180) (/ shipmax 2)))
             (define y (+ (* -0.5 size) 10 (* (remainder i 4) 150) (/ shipmax 2)))
             (define sym (string->symbol (ship-type s)))
             (prepend! sprites (sprite x y (sprite-idx csd sym)
                                       #:layer LAYER_UI #:theta (- pi/2)
                                       #:m (/ (exact->inexact (ship-sprite-size s))
                                              (sprite-size csd sym))
                                       #:r (get-red ownspace s)))

             (define w (ship-w s 1.0))
             (prepend! sprites (draw-hp-bar s x y w csd LAYER_UI))
             
             (define b (button 'normal (ob-id s) #f
                               (+ x (/ shipmax 2) 80)
                               (+ y (- (/ shipmax 2)) 15) 150 30 (ship-name s)
                               (lambda (x y)
                                 (send-commands (chmov meid (ob-id s) #f)))))
             (prepend! buttons b)
             (define players (find-all ownspace s player?))
             (for ((i (modulo (debug-num) 10)))
               (append! players (player -1 (~a "player" i) "fac1" -1 '() #f #f)))
             (for ((p (in-list players))
                   (i (in-naturals)))
               (prepend! sprites (text-sprite textr textsr (player-name p)
                                              (+ x (/ shipmax 2) 10)
                                              (+ y (- (/ shipmax 2)) 40 (* i 20))
                                              LAYER_UI)))))
          ((not (ship-flying? ship))
           ; our ship is inside another
           ; draw black circle on top of topship
           (prepend! sprites (obj-sprite topship csd center (get-scale)
                                         LAYER_EFFECTS 'circle
                                         (/ (* 2.2 (ship-radius topship)) 100)
                                         0.75 0.0 (make-color 0 0 0 1.0)))
           ; draw our ship inside black circle
           (define-values (x y) (obj->screen topship center (get-scale)))
           (define sym (string->symbol (ship-type ship)))
           (prepend! sprites (sprite x y (sprite-idx csd sym)
                                     #:layer LAYER_HANGAR #:theta (- pi/2)
                                     #:m (* (get-scale)
                                            (/ (ship-sprite-size ship)
                                               (sprite-size csd sym)))
                                     #:r (get-red ownspace ship)))
           (define w (ship-w ship (get-scale)))
           (prepend! sprites (draw-hp-bar ship x y w csd LAYER_UI))))

        ; draw ship UI
        (prepend! sprites (draw-ship-hp csd textr center (get-scale) my-stack))
        
        ; draw tool UI
        (when (not (unbox in-hangar?))
          (define tools (filter tool-visible? (ship-tools (or rcship ship))))
          (for ((t (in-list tools)))
            (define-values (bs ss)
              (draw-tool-ui csd center (get-scale) ownspace meid (or rcship ship)
                            t my-stack send-commands active-mouse-tool last-pbolt-time))
            (prepend! buttons bs)
            (prepend! sprites ss)))

        (when (and (not rcship) (ship-hangar ship))
          (define b (button 'normal #\h #f (- (right) 80) (+ (top) 70) 140 40
                            (~a "Hangar [h] " (length (ship-hangar ship)))
                            (lambda (x y)
                              (set-box! in-hangar? #t))))
          (when (unbox in-hangar?)
            (set-button-label! b "Exit Hangar [h]")
            (set-button-f! b (lambda (x y) (set-box! in-hangar? #f))))
          (prepend! buttons b))

        ; tool overlay
        ; XXX when we have dock on
        ;(dock? t)
        ;(when (dock-on t)
        ;(draw-docking csd center (get-scale) (get-space my-stack) my-stack)))
        
        ) ; when my-stack
      )
      
      ; draw annotations that exist in canon space
      (timeit t6
      (for ((a (in-list (space-objects ownspace)))
            #:when (ann? a))
        (when (and (ann-button? a) (or (not (ann-showtab? a)) showtab))
          (define ab (button 'normal (ob-id a) #f
                             (+ (left) (obj-x a)) (+ (top) (obj-y a))
                             (obj-dx a) (obj-dy a) (ann-txt a)
                             (lambda (k y) (send-commands (anncmd (ob-id a))))))
          (prepend! buttons ab))
        (when (and (ann-text? a) (or (not (ann-showtab? a)) showtab))
          (define z
            (cond
              ((ann-text-life a)
               (linear-fade (obj-age ownspace a)
                            (ann-text-life a)
                            (+ (ann-text-life a) MSG_FADE_TIME)))
              (else 1.0)))
          (define txts (string-split (ann-txt a) "\n"))
          (for ((t (in-list txts))
                (i (in-naturals)))
            (prepend! sprites (text-sprite textr textsr t
                                          (obj-x a) (+ (obj-y a) (* i 20))
                                          LAYER_UI_TEXT z)))))

      (when showtab
        ; list all players
        (define str (format "Players (~a):" (length (space-players ownspace))))
        (prepend! sprites (text-sprite textr textsr str
                                      200 (+ (top) 80) LAYER_UI_TEXT))
        (for ((p (in-list (space-players ownspace)))
              (i (in-naturals)))
          (define str (if (player-faction p)
                          (~a (player-name p) " " (player-faction p))
                          (player-name p)))
          (prepend! sprites (text-sprite textr textsr str
                                        200 (+ (top) 100 (* i 20)) LAYER_UI_TEXT))))
      
      ; draw orders
      (define line 0)
      (when ordertree
        (define lefte (+ (left) 150))
        (prepend! sprites (text-sprite textr textsr "Orders:"
                                       lefte (top) LAYER_UI_TEXT))
        (set! lefte (+ lefte 80))
        (define tope (top))
        (for-orders ordertree showtab
          (lambda (ot depth highlight?)
            (when showtab
              (define color (send the-color-database find-color (if (ord-done? ot) "green" "red")))
              (prepend! sprites (xy-sprite (+ lefte (* 10 depth) 5) (+ tope (* 20 line) 10)
                                           csd (get-scale) LAYER_UI 'circle
                                           (/ 0.05 (get-scale))
                                           1.0 0.0 color)))
            (define color (send the-color-database find-color (if highlight? "white" "gray")))
            (define txt (ord-text ot))
            (when (and (ordertime? ot) (string-contains? txt "~a"))
              (define secleft (ceiling (/ (- (ordertime-subtotal ot)
                                             (- (space-time ownspace) (ordertime-start ot)))
                                          1000)))
              (define-values (min sec) (quotient/remainder secleft 60))
              (set! txt (format (ord-text ot)
                                (~a (~a min #:min-width 2 #:align 'right
                                        #:pad-string "0")
                                    ":"
                                    (~a sec #:min-width 2 #:align 'right
                                        #:pad-string "0")))))
            (prepend! sprites (text-sprite textr textsr txt
                                      (+ lefte 12 (* 10 depth)) (+ tope (* 20 line))
                                      LAYER_UI_TEXT 1.0 color))
            (set! line (+ line 1)))))
      
      (when (not my-stack)
        (define start-ships
          (find-all ownspace ownspace (lambda (o)
                                        (and (ship? o)
                                             (ship-flying? o)
                                             (ship-start o)
                                             (equal? fac (ship-faction o))))))
        
        (when (not fac)
          (prepend! sprites (textr "Waiting for faction assignment..."
                                   0.0 0.0 #:layer LAYER_UI
                                   #:r 255 #:g 255 #:b 255)))
        
        (for ((s (in-list start-ships))
              (i (in-naturals)))
          (define x (+ (left) 100 (* (remainder i 3) 250)))
          (define y (+ (top) 30 (* (quotient i 3) 60)))
          (define b (button 'normal (ob-id s) #f x y 200 30
                            (format "~a" (ship-name s))
                            (lambda (x y)
                              ; leaving sector overview, so center on ship and reset scale
                              (set! scale-play 1.0)
                              (set! center-follow? #t)
                              (send-commands (chmov meid (ob-id s) #f)))))
          (prepend! buttons b)))
            
      ; draw game UI
      
      ; zoom scale
      (when (not showsector?)
        (define zw 20)
        (define zh 150)
        (define zcx (- (right) 10 (/ zw 2)))
        (define zcy (+ (top) 100 (/ zh 2)))
        (prepend! sprites (sprite zcx (+ zcy (- (/ zh 2))) (sprite-idx csd '20x2)
                                  #:layer LAYER_UI #:b (send mapcol blue)))
        (prepend! sprites (sprite zcx (+ zcy (/ zh 2)) (sprite-idx csd '20x2)
                                  #:layer LAYER_UI #:b (send mapcol blue)))
        (prepend! sprites (sprite zcx zcy (sprite-idx csd '2x150)
                                  #:layer LAYER_UI #:b (send mapcol blue)))
        
        (define zfrac (/ (- (log scale-play) (log (min-scale)))
                         (- (log (max-scale)) (log (min-scale)))))
        (prepend! sprites (sprite zcx (+ zcy (/ zh 2) (- (* zfrac zh)))
                                  (sprite-idx csd '20x2)
                                  #:layer LAYER_UI #:b (send mapcol blue)))
        (define zbutton (button 'hidden -1 #f zcx zcy zw zh "Zoom"
                                (lambda (x y)
                                  (define zfracy (/ (+ (/ zh 2) (- y)) zh))
                                  (define z (exp (+ (log (min-scale))
                                                    (* zfracy (- (log (max-scale))
                                                                 (log (min-scale)))))))
                                  (set-scale z))))
        (define zkeyb (button 'hidden #\r #f 0 0 0 0 "Zoom In"
                              (lambda (k y) (set-scale (* scale-play 1.1)))))
        
        (define xkeyb (button 'hidden #\t #f 0 0 0 0 "Zoom Out"
                              (lambda (k y) (set-scale (/ scale-play 1.1)))))
        
        (prepend! buttons zbutton zkeyb xkeyb))

      ; auto-center button
      (when (not center-follow?)
        (define b (button 'normal #\z #f 0.0 (+ (top) 50) 120 50 "Auto Center"
                          (lambda (x y) (set! center-follow? #t))))
        (prepend! buttons b))
        
      ; normally quit with ctrl-q
      (define quit-button
        (button 'hidden #\q #t
                0 0 0 0 "Quit"
                (lambda (x y)
                  (define ans (message-box/custom "Quit?" "Done Playing?"
                                                  "Quit" "Keep Playing" #f
                                                  frame '(default=2)))
                  (when (equal? 1 ans)
                    (drop-connection "clicked exit")
                    (set! playing? #f)
                    (send frame show #f)))))
      (when (not my-stack)
        ; if we are not in the game, show the button
        (set-button-draw! quit-button 'normal)
        (set-button-x! quit-button (- (right) 50))
        (set-button-y! quit-button (- (bottom) 25))
        (set-button-width! quit-button 100)
        (set-button-height! quit-button 50))
      (prepend! buttons quit-button)

      (define leave-button (button 'normal 'escape #f
                                   (+ (left) 50) (+ (top) 25) 100 50
                                   "Exit" #f))
      (cond
        ((not my-stack)
         ; nothing to leave
         )
        ((unbox in-hangar?)
         (set-button-f! leave-button (lambda (x y) (set-box! in-hangar? #f)))
         (prepend! buttons leave-button))
        ((spacesuit? (get-ship my-stack))
         ; dying
         (set-button-f! leave-button (lambda (x y)
                                       (set! center-follow? #t)  ; sector/ship centered
                                       (send-commands (chmov meid #f #f))))
         (prepend! buttons leave-button))
        ((and (player-rcid (car my-stack))
              (find-id ownspace ownspace (player-rcid (car my-stack))))
         ; remote controlling something
         )
        ((ship-flying? (get-ship my-stack))
         ; jumping ship
         (set-button-label! leave-button "Jump")
         (set-button-key! leave-button #f)  ; turn off keyboard shortcut
         (set-button-f! leave-button (lambda (x y) (send-commands (chmov meid #f #f))))
         (prepend! buttons leave-button))
        (else
         ; leaving this ship into mothership
         (define ms (cadr (get-ships my-stack)))
         (set-button-f! leave-button (lambda (x y)
                                       (send-commands (chmov meid (ob-id ms) #f))))
         (prepend! buttons leave-button)))
      
      ; draw mouse cursor and green corners
      (when my-stack
        (define player (car my-stack))
        (define rcship (if (player-rcid player)
                           (find-id ownspace ownspace (player-rcid player)) #f))
      
        (define ship (or rcship (get-ship my-stack)))
        
        (define-values (p mods) (get-current-mouse-state))
        (define-values (wx wy) (send canvas screen->client
                                     (+ (send p get-x) left-inset)
                                     (+ (send p get-y) top-inset)))
        (define-values (x y) (screen->canon canvas wx wy))
        (when (and (not (in-button? buttons x y))
                   (not (unbox in-hangar?)))
          (define mypos (get-center ownspace my-stack))
          (define-values (mx my) (obj->screen mypos center (get-scale)))
          (define a (angle-norm (atan0 (- my y) (- x mx))))
          (cond
            #;((or (and mt (find-id ownspace ownspace (mtube-mid mt)))
                 (and pt (find-id ownspace ownspace (ptube-pid pt)))
                 (and st (tool-online? st)))
             (set! cursordrawn #t)
             (set! clickcmds (command
                              (cond (mt (mtube-mid mt))
                                    (pt
                                     (define probe (find-id ownspace ownspace
                                                            (ptube-pid pt)))
                                     (ob-id (findf steer?
                                                   (pod-tools (car (ship-pods probe))))))
                                    (else (ob-id st)))
                              a))
             (prepend! sprites (sprite (exact->inexact x) (exact->inexact y)
                                      (sprite-idx csd 'arrowhead) #:layer LAYER_UI_TEXT
                                      #:theta (- a) #:b 150)))
            ((and (equal? 'pbolt (unbox active-mouse-tool))
                  (ship-tool ship 'pbolt)
                  (ship-flying? ship)
                  (not (warping? ship)))
             (set! cursordrawn #t)
             (define cmds (command meid (player-cmdlevel player) 'pbolt
                                   (list a a
                                         (pbolt-frac last-pbolt-time (space-time ownspace)))))
             (set! clickcmds (lambda ()
                               (set! last-pbolt-time (space-time ownspace))
                               cmds))
             (prepend! sprites (sprite (exact->inexact x) (exact->inexact y)
                                       (sprite-idx csd 'target) #:layer LAYER_UI_TEXT
                                       #:r 100 #:g 100 #:b 255 #:m 0.7))))))
      
      (prepend! buttons
               (button 'hidden #\tab #f 0 0 0 0 "Mission Info"
                       (lambda (k y) (set! showtab (not showtab))))
               (button 'hidden #\` #f 0 0 0 0 "Show Sector"
                       (lambda (k y) (set! showsector? (not showsector?)))))

      ; messages
      (when ownspace
        (define max 6)
        (define num 0)
        (let loop ((l (space-objects ownspace)))
          (when (and (not (null? l)) (num . < . max))
            (when (message? (car l))
              (define m (car l))
              (set! num (+ num 1))
              (define z (linear-fade (obj-age ownspace m) (/ MSG_FADE_TIME 2) MSG_FADE_TIME))
              (prepend! sprites (text-sprite textr textsr (message-msg m)
                                             (+ (left) 5.0) (+ (top) 200.0 (* num 20))
                                             LAYER_UI_TEXT z)))
            (loop (cdr l)))))

      ; debugging
      (when (and sspace (unbox sspace))
        (for ((o (space-objects (unbox sspace)))
              #:when (ship? o))
          (prepend! sprites
                    (obj-sprite o csd center (get-scale) LAYER_UI_TEXT 'circle
                                (/ 7.0 100 (get-scale)) 1.0 0.0 "pink"))))
      )
      ) ; when ownspace

    (timeit t7
    (send canvas set-cursor (make-object cursor% (if cursordrawn 'blank 'arrow)))

    ; framerate
    (when ((length frames) . > . 1)
      (define start (list-ref frames (- (length frames) 1)))
      (define end (first frames))
      (define span (/ (- end start) 1000))
      (define txt (format "FPS: ~a" (round (/ (- (length frames) 1) span))))
      (prepend! sprites (text-sprite textr textsr txt (- (right) 80) (top) LAYER_UI)))

    ; network issues?
    (define ma (apply max 0 aheads))
    (when (ma . > . AHEAD_THRESHOLD)
      (define txt (format "Ahead: ~a" ma))
      (prepend! sprites (text-sprite textr textsr txt (- (right) 200) (top) LAYER_UI)))
    
    (prepend! sprites (button-sprites csd textr buttons
                                      (if ownspace (space-time ownspace) 0)
                                      holding pressed))
    
    (define-values (dmgx dmgy)
      (cond (my-stack
             (define ship (get-ship (reverse my-stack)))
             (define d (ship-dmgfx ship))
             (values (random-between (- d) d)
                     (random-between (- d) d)))
            (else
             (values 0.0 0.0))))

    (define width (+ canon-width dmgx))
    (define height (+ canon-height dmgy))
    (define layers (for/vector ((i LAYER_NUM)) (layer width height)))
    )
    (timeit t8
    (define r (render layers '() sprites))
    )
    (timeit t9
    (r (fl->fx canon-width) (fl->fx canon-height) dc)
    )

    ;(outputtime "client render"
    ;            (if ownspace (space-time ownspace) #f)
    ;            t1 t2 t3 t4 t5 t6 t7 t8 #;t9)
    )
    
  
  (define-values (left-inset top-inset) (get-display-left-top-inset))
  (define-values (screen-w screen-h) (get-display-size #t))

  ;(printf "insets ~a ~a size ~a ~a\n" left-inset top-inset screen-w screen-h)
  
  (define frame (new frame%
                     (label "Warp")
                     (width (fl->fx canon-width))
                     (height (fl->fx canon-height))))


  (define (zoom-mouse z)
    (define old-scale (get-scale))
    (set-scale (* scale-play z) #:immediate? #t)
    (when (not center-follow?)
      ; zoom around mouse pointer
      (define-values (p mods) (get-current-mouse-state))
      (define-values (wx wy) (send canvas screen->client
                                   (+ (send p get-x) left-inset)
                                   (+ (send p get-y) top-inset)))
      (define-values (x y) (screen->canon canvas wx wy))
      (define-values (ox oy) (canon->space center old-scale x y))
      (define-values (mx my) (canon->space center (get-scale) x y))
      ; adjust centerxy so ox,oy will display at mx,my
      (set-posvel-x! (obj-posvel centerxy) (- (obj-x centerxy) (- mx ox)))
      (set-posvel-y! (obj-posvel centerxy) (- (obj-y centerxy) (- my oy)))))
  
  (define my-canvas%
    (class canvas%
      (super-new)
      (define/override (on-size w h)
        ;(printf "canvas on-size ~a ~a\n" w h)
        (when (odd? w) (set! w (- w 1)))
        (when (odd? h) (set! h (- h 1)))
        (set-canon-width! (fx->fl w))
        (set-canon-height! (fx->fl h))
        (set! render (gl:stage-draw/dc csd
                                       (fl->fx canon-width)
                                       (fl->fx canon-height)
                                       LAYER_NUM)))
      (define/override (on-event event)
        (case (send event get-event-type)
          ((left-down)
           (click this event))
          ((left-up)
           (define h (for/first ((h (in-list holding))
                                 #:when (equal? 'mouse (hold-held h)))
                       h))
           (when h ((hold-frelease h)))  ; run the release function
           (set! holding (remove h holding)))  ; cancel hold
          ((right-down)
           (when (not showsector?)
             (set! dragstate "start")
             (set! dragxypx (cons (send event get-x) (send event get-y)))))
          ((motion)
           (when (and (send event dragging?) (send event get-right-down))
             (when (or (and (equal? dragstate "start")
                            (or ((abs (- (send event get-x) (car dragxypx))) . > . 3)
                                ((abs (- (send event get-y) (cdr dragxypx))) . > . 3)))
                       (equal? dragstate "drag"))
               (set! dragstate "drag")
               (set! center-follow? #f)
               (define dx (/ (- (send event get-x) (car dragxypx)) (get-scale)))
               (define dy (/ (- (send event get-y) (cdr dragxypx)) (get-scale)))
               (set-posvel-x! (obj-posvel centerxy) (- (obj-x centerxy) dx))
               (set-posvel-y! (obj-posvel centerxy) (+ (obj-y centerxy) dy))
               
               (set! dragxypx (cons (send event get-x) (send event get-y))))))
          ((right-up)
           (set! dragstate "none"))
          ))
      (define/override (on-char event)
        (define kc (send event get-key-code))
        (define b (key-button? buttons kc (send event get-control-down)))
        (define h (for/first ((h (in-list holding))
                              #:when (equal? kc (hold-key h)))
                    h))
        ;(printf "on-char ~v ~v ~v\n" kc b h)
        (cond
          ((not kc)
           ;(printf "got #f for on-char get-key-code\n")
           )
          (h
           ; repeated keypress from holding the key down, drop it
           )
          ((equal? kc 'release)
           (set! kc (send event get-key-release-code))
           (define h (for/first ((h (in-list holding))
                                 #:when (equal? kc (hold-key h)))
                       h))
           (when h
             ; released the key being held
             ((hold-frelease h))  ; run the release function
             (set! holding (remove h holding))))  ; cancel the hold
          (b
           ((button-f b) kc #f)
           (cond
             ((holdbutton? b)
              (prepend! holding (hold kc kc (holdbutton-frelease b)))
              (when ((length holding) . > . 5)
                (printf "holding ~a\n" (length holding))))
             (else
              (press! kc))))
          (else
           (case kc
             ((#\f)
              (when (send event get-control-down)
                (send frame fullscreen (not (send frame is-fullscreened?)))))
             ((#\0)
              (debug-num (+ 1 (debug-num))))
             #;((#\d)
              (when ownspace
                (define cmds '())
                (for ((s (in-list (space-objects ownspace)))
                      #:when (spaceship? s))
                  (append! cmds (list (chdam (ob-id s) 10 #f))))

                #;(when my-stack
                  (define s (get-ship my-stack))
                  (append! cmds (dmg-ship s 20 (- pi/2))))
                
                (send-commands cmds)))
             #;((#\m)
              (when ownspace
                (send-commands (message (next-id) (space-time ownspace) #t #f
                                        (~a "message " (space-time ownspace))))))
             #;((#\e)
              (when ownspace
                (send-commands (chadd (make-explosion ownspace
                                                      0.0 0.0 10.0 50.0 100.0 100.0) #f))))
             ((wheel-up)
              (when (and ownspace (not showsector?))
                (zoom-mouse 1.05)))
             ((wheel-down)
              (when (and ownspace (not showsector?))
                (zoom-mouse (/ 1.0 1.05))))
             #;((#\u)
              (when ownspace
                (send-commands (chadd (random-upgrade ownspace
                                                      (posvel -1 0 0 0
                                                              (random 100)
                                                              (random 100) 0)) #f))))
             #;((#\p)
              (when ownspace
                (send-commands (chadd (plasma (next-id)
                                              (space-time ownspace)
                                              (posvel -1 0 0
                                                      (random-between 0 2pi)
                                                      (random 100)
                                                      (random 100) 0)
                                              (random 10)) #f))))
;             ((#\s)
;              (when ownspace
;                (define r (random-between 0 2pi))
;                (define s (random 100))
;                (send-commands (chadd (shield (next-id)
;                                              (space-time ownspace)
;                                              (posvel -1 0 0 r (* s (cos r)) (* s (sin r)) 0)
;                                              (random 30)) #f)))) 
;             ((#\n)
;              (new-stars))
             ))))
      ))


  (define glconfig (new gl-config%))
  (send glconfig set-legacy? #f)
  
  (set! canvas
    (new my-canvas%
         (parent frame)
         (paint-callback draw-screen)
         (gl-config glconfig)
         (style '(no-autoclear gl))))
  
  (define sd (make-sprite-db))
  (let ()
    (add-sprite!/value sd 'button-normal
                       (inset (filled-rectangle 100 50 #:color "gray"
                                                #:border-color "white" #:border-width 2) 1))
    (add-sprite!/value sd 'button-outline
                       (inset (rectangle 100 50 #:border-color "gray" #:border-width 2) 1))
    (add-sprite!/value sd 'button-disabled
                       (inset (filled-rectangle 100 50 #:color "black"
                                                #:border-color "gray" #:border-width 2) 1))
    (add-sprite!/value sd 'button-normal-circle
                       (inset (filled-ellipse 100 100 #:color "gray"
                                              #:border-color "white" #:border-width 2) 1))
    (add-sprite!/value sd 'button-disabled-circle
                       (inset (filled-ellipse 100 100 #:color "black"
                                              #:border-color "gray" #:border-width 2) 1))
    (add-sprite!/value sd 'dmgbutton-normal
                       (inset (rectangle 100 50 #:border-color "black" #:border-width 2) 1))
    (add-sprite!/value sd 'dmgbutton-fill
                       (inset (filled-rectangle 100 50 #:color "black"
                                                #:border-color "black" #:border-width 0) 2))
    (add-sprite!/value sd '100x1
                       (colorize (filled-rectangle 100 1) "black"))
    (add-sprite!/value sd '5x1
                       (colorize (filled-rectangle 5 1) "black"))
    (add-sprite!/value sd '1x1
                       (colorize (filled-rectangle 1 1) "black"))
    (add-sprite!/value sd '20x2
                       (colorize (filled-rectangle 20 2) "black"))
    (add-sprite!/value sd '2x150
                       (colorize (filled-rectangle 2 150) "black"))
    (add-sprite!/value sd '100x100
                       (colorize (filled-rectangle 100 100) "black"))
    (add-sprite!/value sd 'circle
                       (colorize (filled-ellipse 100 100) "black"))
    (add-sprite!/value sd 'shield
                       (colorize (filled-rounded-rectangle 10 100 -.5 #:draw-border? #f)
                                 "black"))
    (add-sprite!/value sd 'circle-outline
                       (colorize (inset
                                  (ellipse 100 100 #:border-color "black" #:border-width 2)
                                  2) "black"))
    (add-sprite!/value sd 'square
                       (colorize (filled-rectangle 100 100) "black"))
    (add-sprite!/value sd 'square-outline
                       (colorize (rectangle 100 100 #:border-color "black" #:border-width 10)
                                 "black"))
    (add-sprite!/value sd 'star
                       (colorize (cc-superimpose (hline 1 1) (vline 1 1)) "white"))
    (add-sprite!/value sd 'arrowhead
                       (colorize (arrowhead 30 0) "black"))
    (add-sprite!/value sd 'target
                       (colorize (linewidth 2.5
                                   (dc (lambda (dc dx dy)
                                         (define b (send dc get-brush))
                                         (send dc set-brush nocolor 'transparent)
                                         (send dc draw-ellipse 5 5 100 100)
                                         (send dc draw-line 55 5 55 105)
                                         (send dc draw-line 5 55 105 55)
                                         (send dc set-brush b))
                                       110 110)) "black"))
    (add-sprite!/value sd 'overlay-qm
                       (text "?" '(bold) 24))
    (add-sprite!/value sd 'overlay-cargo
                       (colorize (linewidth 2.5
                                   (dc (lambda (dc dx dy)
                                         (define b (send dc get-brush))
                                         (send dc set-brush nocolor 'transparent)
                                         (send dc draw-line 5 5 5 15)
                                         (send dc draw-line 5 15 15 15)
                                         (send dc draw-line 15 15 15 5)
                                         (send dc draw-line 5 5 15 5)
                                         (send dc draw-line 5 5 15 15)
                                         (send dc draw-line 5 15 15 5)
                                         (send dc set-brush b))
                                       20 20)) "black"))
    (add-sprite!/value sd 'corner
                       (colorize
                        (dc (lambda (dc dx dy)
                              (define b (send dc get-brush))
                              (send dc set-brush nocolor 'transparent)
                              (send dc draw-line 0 0 1 0)
                              (send dc draw-line 0 0 0 1)
                              (send dc set-brush b))
                            2 2) "black"))
    )
  (define textfont (load-font! sd #:size TEXTH #:face "Verdana" #:family 'modern))
  (load-ships sd)
  (plasma-setup-pre! sd)
  (explosion-setup-pre! sd)
  (add-sprite!/file sd 'missile (string-append "images/missile.png"))
  (add-sprite!/file sd 'cannonball (string-append "images/asteroid_43.png"))
  
  (define csd (compile-sprite-db sd #:padding 2))
  ;(save-csd! csd "csd" #:debug? #t)
  (plasma-setup-post! csd)
  (explosion-setup-post! csd)
  (define textr (make-text-aligned-renderer textfont csd))
  (define textsr (make-text-aligned-sizer textfont csd))
  (gl:gl-smoothing? #t)
  (define render (gl:stage-draw/dc csd
                                   (fl->fx canon-width)
                                   (fl->fx canon-height)
                                   LAYER_NUM))
  
  (send frame show #t)
 ) ; when gui?
  
  
  (define (tick-space-client! space)
    (set-space-time! space (+ (space-time space) TICK))
    (for ((o (in-list (space-objects space)))
          #:when (obj-alive? o))
      (update-physics! space o (/ TICK 1000.0))
      (update-stats! space o (/ TICK 1000.0))
      (add-backeffects! space o)))
  
  
  (define (drop-connection msg)
    (printf "drop server ~a\n" msg)
    (when server-in-port
      (close-input-port server-in-port)
      (close-output-port server-out-port)
      (kill-thread server-in-t)
      (kill-thread server-out-t))
    (set! server-in-port #f)
    (set! server-out-port #f)
    (set! server-in-t #f)
    (set! server-out-t #f)
    (set! meid #f)
    (set! ownspace #f))

  
  (define (send-commands cmds)
    (when (not (list? cmds))
      (set! cmds (list cmds)))
    (when (and server-out-t ((length cmds) . > . 0))
      ;(printf "send-commands ~v\n" cmds)
      (thread-send server-out-t
                   (serialize
                    (update (if ownspace (space-id ownspace) #f)
                            last-update-time cmds #f)))))
  

  (define start-loop-time #f)
  ; time we want to show
  (define target-time #f)
  ; saved ownspace if we predicted forward
  (define oldspace #f)
  
  
  (define (client-loop)

    (when (not start-loop-time)
      (set! start-loop-time (current-milliseconds)))

    (define time-updates 0)
    (define time-predict 0)
    (define time-copy 0)
    (define time-setup 0)
    (define time-render 0)
    
    (when (not server-in-port)
      (when gui?
        (define newname
          (get-text-from-user "Player Name"
                              "Player Name"
                              #f
                              (or name "")))
        
        (when (not newname) (exit 0))
        (when newname (set! name newname))
        
        ; ask the user for address
        (define newip
          (get-text-from-user "IP of server"
                              "IP address of the Server"
                              #f
                              (or ip "")))
        
        (when (not newip) (exit 0))
        (when newip (set! ip newip)))
        
      ; connect to server
      (define-values (in out)
        (with-handlers ((exn:fail:network?
                         (lambda (exn)
                           ((error-display-handler) (exn-message exn) exn)
                           (sleep 1)
                           (values #f #f))))
          (printf "trying to connect to ~a:~a\n" ip port)
          (tcp-connect ip port)))
      
      (set! server-in-port in)
      (set! server-out-port out)
      
      (when server-out-port
        (set! server-in-t (make-in-thread #f in (current-thread)))
        (set-tcp-nodelay! out #t)
        (set! server-out-t (make-out-thread #f out (current-thread)))
        ; send our name to the server
        (send-commands (player #f name #f #f '() #f #f))))

    (define num-updates 0)
    (define num-ticks 0)
    
    ; process received info
    (timeit time-updates
    (let loop ()
      (define v (thread-try-receive))
      (when v
        (define id (car v))
        (define input (cdr v))
        ;(printf "~a client input: ~v\n" name input)
        (cond
          ((not input)
           (drop-connection ""))
          ((player? input)
           ; should only happen once when we connect to the server
           (printf "got player id from server ~a\n" (ob-id input))
           (set! meid (ob-id input))
           (idimag meid))
          ((space? input)
           (set! ownspace input)
           (set! oldspace #f)
           (set! target-time (space-time ownspace))
           (set! last-update-time (space-time ownspace))
           (set! last-pbolt-time (space-time ownspace))
           ;(printf "~a client new ownspace ~a\n" name (space-time ownspace))

           ; new ownspace, reset view stuff
           (set! showsector? #f)
           (set! center #f)
           (set! center-follow? #t)
           (set-posvel-x! (obj-posvel centerxy) 0)
           (set-posvel-y! (obj-posvel centerxy) 0)
           (set! dragstate "none")

           (when (not (find-id ownspace ownspace meid))
             ; set scale so we see the whole sector
             (set! scale-play (min-scale))
             (set! first-scale #t)))
          ((update? input)
           (set! num-updates (+ 1 num-updates))
           (cond
             ((not ownspace)
              (printf "~a client dropping update ~a (no ownspace)\n"
                      last-update-time (update-time input)))
             ((not (= (space-id ownspace) (update-id input)))
              (printf "~a client dropping update id ~a (ownspace id ~a)\n"
                      last-update-time (update-id input) (space-id ownspace)))
             ((not (= (update-time input) (+ last-update-time TICK)))
              (printf "~a client dropping update at time ~a, expecting ~a\n"
                      last-update-time (update-time input) (+ last-update-time TICK)))
             (else
              (set! last-update-time (update-time input))

              (when ((update-time input) . > . target-time)
                ; got this update sooner than expected, maybe our lag decreased?
                ;(printf "client jumping forward to ~a\n" (update-time input))
                (set! target-time (update-time input))

                ; target-time usually gets the loop time added to it, but
                ; we are resetting target-time here, so restart the loop time
                ; otherwise target-time goes too far into the future
                ; - test case is run combined.rkt, click "Quit", wait a few secs
                ; - then click keep playing
                (set! start-loop-time (current-milliseconds)))

              (when oldspace
                (when (not (equal? (space-time oldspace) (update-time input)))
                  (error "oldspace time ~a != update-time ~a\n"
                         (space-time oldspace) (update-time input)))
                (set! ownspace oldspace)
                (set! oldspace #f))

              (when ((space-time ownspace) . < . (update-time input))
                ;(printf "client ticking ownspace forward for input ~a\n" (update-time input))
                (set! num-ticks (+ 1 num-ticks))
                (tick-space-client! ownspace))

              (when ((space-time ownspace) . < . (update-time input))
                (error "client ownspace still behind update time\n"))

              (for ((c (in-list (update-changes input))))
                ;(printf "client applying change ~v\n" c)
                (apply-all-changes! ownspace (list c) "client")
                ;(define-values (forward? useless-changes)
                ;  (apply-change! ownspace c "client"))
                ;(when (not (null? useless-changes))
                ;  (printf "client produced useless changes:\n  ~v\n" useless-changes))
                (when (and (chmov? c) (equal? meid (chmov-id c)))
                  (set-box! in-hangar? #f))
                )
              (for ((pvu (in-list (update-pvs input))))
                (define o (find-top-id ownspace (pvupdate-id pvu)))
                (cond
                  (o
                   (set-obj-posvel! o (pvupdate-pv pvu)))
                  (else
                   (printf "client dropped pvupdate couldn't find obj id ~v\n"
                           (pvupdate-id pvu)))))))))
        (loop)))
    )
    
    (when ownspace

      (timeit time-predict      
      (when ((+ target-time (- TICK)) . > . (space-time ownspace))
        ; maybe our lag increased, so slowly reduce the target-time
        ;(printf "target-time-- ~a\n" (- (+ target-time (- TICK)) (space-time ownspace)))
        (set! target-time (- target-time 1)))

      
      (define motion? #f)

      ; for debugging to artificially make the client predict ahead
      (define future 0)

      
      (while ((+ target-time future) . > . (space-time ownspace))
        ; we are behind, forward predict
        (when (and (not oldspace)
                   (last-update-time . < . (space-time ownspace)))
          ;(printf "client saving oldspace ~a\n" (space-time ownspace))
          (timeit time-copy
          (set! oldspace (copy ownspace))
          )
          )

        (tick-space-client! ownspace)
        (set! motion? #t)
        (set! num-ticks (+ num-ticks 1))

        (when oldspace
          ;(printf "client forward predict to ~a\n" (space-time ownspace))
          (set! num-ticks (- num-ticks 1))))
      )

      (set-space-objects! ownspace (filter obj-alive? (space-objects ownspace)))

      ;(printf "updates ~a ticks ~a ~a\n" num-updates num-ticks (if motion? "motion" ""))

      (timeit time-setup
      (define ahead (- (space-time ownspace) last-update-time))
      (set! aheads (add-frame-time ahead aheads))
      ;(when (ahead . > . AHEAD_THRESHOLD)
      ;  (printf "~a client is ahead by ~a\n" last-update-time ahead))
      
      
      (set! my-stack (find-stack ownspace ownspace meid))
      (when my-stack
        (when first-scale
          (set! first-scale #f)
          (set! scale-play 1.0))
        (when (get-ship my-stack)
          (define tools (map tool-name (ship-tools (get-ship my-stack))))
          (cond
            ((member (unbox active-mouse-tool) tools)
             ; our tool exists in whatever ship we find ourselves in
             )
            (else
             ; our tool doesn't exist, default to the first available
             (set-box! active-mouse-tool (for/first ((t tools)
                                                     #:when (member t MOUSE_TOOLS))
                                           t))))))
      )
      )
    
    ; render a frame
    (timeit time-render
    (define now (current-milliseconds))
    ; filter button presses
    (set! pressed (filter (lambda (p) ((- now (press-time p)) . < . BUTTON_PRESS_TIME))
                          pressed))
    (set! frames (add-frame-time now frames))
    (when gui?
      (send canvas refresh-now))
    )

    (when (and (not gui?)
               my-stack
               ((random) . < . 0.01))
      (define player (car my-stack))
      (define cmd (command meid (player-cmdlevel player) 'pbolt (list pi/2 pi/2 1.0)))
      (send-commands cmd))

    ; for debugging low-fps situations
    ;(define sum 0)
    ;(for ((i 1000000))
    ;  (set! sum (+ i sum)))

    ; housekeeping
    (flush-output)
    (collect-garbage 'incremental)
    
;    (when (time-for (current-milliseconds) 1000)
;      (displayln (~a "mem: " (~r (/ (current-memory-use) (* 1024.0 1024.0)) #:precision 2))))

    ; sleep so we don't hog the whole racket vm
    ; how long has it been since we should have woken up?
    (define loop-time (- (current-milliseconds) start-loop-time))
    ; how long should we sleep (always at least 5ms for the GUI to process GUI events)
    (define extra-time (- TICK loop-time))
    (define min-sleep 5)
    (define sleep-time (max min-sleep extra-time))
    (define total (+ loop-time sleep-time))

    (when (extra-time . < . min-sleep)
      (printf "client extra-time ~a num objects ~a\n"
              extra-time (if ownspace (length (space-objects ownspace)) #f))
      (outputtime "client"
                 (if ownspace (space-time ownspace) #f)
                 time-updates
                 time-predict
                 time-copy
                 time-setup
                 time-render))

    ; set the start of the loop time to be exactly when we want to wake up
    ; we might actually sleep a bit longer, that will count as part of loop-time
    ; during the next loop
    (set! start-loop-time (+ start-loop-time total))
    (when target-time
      (set! target-time (+ target-time total)))

    (sleep/yield (/ sleep-time 1000.0))
    (when playing? (client-loop)))
  
  (queue-callback client-loop #f))



(module+ main
  ;(require profile)
  ;(profile #:threads #t #:delay 0.0
    ;(begin
    (start-client "127.0.0.1" PORT "Dave")
    ;))
  ;(exit 0)
  )
