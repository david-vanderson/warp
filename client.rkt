#lang racket/gui

(require
  racket/fixnum
  mode-lambda
  mode-lambda/static
  mode-lambda/text
  (prefix-in gl: mode-lambda/backend/gl)
  "defs.rkt"
  "utils.rkt"
  "change.rkt"
  "physics.rkt"
  "draw-utils.rkt"
  "draw.rkt"
  "pilot.rkt"
  "weapons.rkt"
  "pbolt.rkt"
  "effect.rkt"
  "ships.rkt"
  "upgrade.rkt"
  "dmg.rkt"
  "order.rkt"
  "missile.rkt"
  "plasma.rkt")

(provide start-client)

(define serverspace #f)

(define (start-client ip port name new-eventspace? sspace)
  (server? #f)
  (when sspace
    (set! serverspace sspace))
  (when new-eventspace?
    (current-eventspace (make-eventspace)))

  (define playing? #t)
  (define server-in-port #f)
  (define server-out-port #f)
  (define meid #f)  ; integer? or #f
  (define ownspace #f)
  (define my-stack #f)
  (define buttons #f)
  (define sprites #f)
  (define active-mouse-tool (box #f))
  (define in-hangar? (box #f))

  (define holding '())
  ; if you are holding a key/mouse button (from a holdbutton?), this is a list of pairs
  ; car is keycode (or 'mouse) of the key that's being held
  ; cdr is the holdbutton-frelease function
  
  ; if the mouse is over something (other than a button) that when clicked will
  ; do some action, clickcmds is a list of commands we send
  (define clickcmds #f)
  (define frames '())  ; list of last few frame times
  (define last-update-time #f)

  (define showtab #f)  ; tab toggles an overlay showing players and goals
  (define showsector? #f)  ; tilde toggles showing the whole sector or the regular view
  (define zerocenter (obj #f #f (posvel #f 0.0 0.0 #f #f #f #f)))

  (define center #f)  ; updated each frame for the click handler and mouse cursor drawing
  (define center-follow? #t)  ; show player position in the center?

  ; when (not center-follow?)
  (define centerxy (obj #f #f (posvel #f 0.0 0.0 #f #f #f #f)))  ; center of the screen when panning
  (define dragxypx '(0 . 0))  ; last xy of drag in pixels
  (define dragstate "none")  ; "none", "start", "drag"
  
  (define scale-play 1.0)  ; scale for zooming
  
  (define (min-scale)
    (if ownspace
        (min (/ WIDTH (space-width ownspace)) (/ HEIGHT (space-height ownspace)))
        .01))
  (define (max-scale) 30.0)
    
  (define (set-scale z)
    (define newz (clamp (min-scale) (max-scale) z))
    (set! scale-play newz))
  
  (define shown-scale scale-play)  ; scale that is actually used
  (define (get-scale) (if showsector? (min-scale) shown-scale))
  (define (update-scale)  ; move shown-scale towards the scale we want
    (define target scale-play)
    (define diff (abs (- target shown-scale)))
    (if (shown-scale . < . target)
        (set! shown-scale (min target (+ shown-scale (max 0.1 (* 0.4 diff)))))
        (set! shown-scale (max target (- shown-scale (max 0.1 (* 0.4 diff)))))))
  
  
  (define (in-button? buttons x y)
    (for/first ((b (in-list buttons))
                #:when (if (button-height b)
                           (and (<= (abs (- (button-x b) x)) (/ (button-width b) 2.0))
                                (<= (abs (- (button-y b) y)) (/ (button-height b) 2.0)))
                           (and (<= (sqrt (+ (* (- x (button-x b)) (- x (button-x b)))
                                             (* (- y (button-y b)) (- y (button-y b)))))
                                    (/ (button-width b) 2.0)))))
      b))

  (define (key-button? buttons key)
    (for/first ((b (in-list buttons))
                #:when (equal? (button-key b) key))
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
         (when (holdbutton? b)
           (set! holding (cons (cons 'mouse (holdbutton-frelease b)) holding)))
         ))
      (clickcmds
       (send-commands clickcmds))))
  
  
  (define (draw-screen canvas dc)
;    (when (and serverspace ownspace)
;      (printf "serverspace time ~a\n   ownspace time ~a\n" (space-time serverspace) (space-time ownspace)))

    (set! buttons '())
    (set! sprites '())
    (define cursordrawn #f)
    (set! clickcmds #f)
    
    (update-scale)
    (define canvas-scale (min (/ (send canvas get-width) WIDTH) (/ (send canvas get-height) HEIGHT)))
    
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
      (for* ((i (in-range LEFT RIGHT w))
             (j (in-range TOP BOTTOM h)))
        (append! sprites (text-sprite textr textsr txt i j LAYER_UI))))
    
    (when ownspace
      (define p (findfid meid (space-players ownspace)))
      (define fac (if p (player-faction p) #f))
      (when (and (not fac) my-stack)
        (set! fac (ship-faction (get-ship my-stack))))
      (define ordertree (get-space-orders-for ownspace fac))
      
      ; make background be fog of war gray
      (append! sprites (sprite 0.0 0.0 (sprite-idx csd 'gray) #:layer LAYER_FOW_GRAY
                               #:m (* canvas-scale (min WIDTH HEIGHT))))

      ; put a black circle wherever we can see
      (define fowlist
        (for/list ((s (in-list (space-objects ownspace)))
                   #:when (and fac (ship? s) ((faction-check fac (ship-faction s)) . > . 0)))
          (list (obj-x s) (obj-y s) (ship-radar s))))

      (for ((f fowlist))
        (define rad (caddr f))
        (define-values (x y) (obj->screen (obj #f #f (posvel 0 (car f) (cadr f) 0 0 0 0)) center (get-scale)))
        (append! sprites (sprite x y (sprite-idx csd 'circle) #:layer LAYER_FOW_BLACK
                                 #:m (* rad (get-scale) (/ 1.0 50)))))

      (append! sprites (draw-sector-lines csd center (get-scale) ownspace))

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
                   (append! sprites
                            (obj-sprite a csd center (get-scale) LAYER_MAP 'circle-outline
                                        (* 2.0 (ann-circle-radius a)) (send col alpha) 0.0 col))
                   (define-values (x y) (obj->screen a center (get-scale)))
                   (append! sprites (textr (ann-txt a)
                                           x y #:layer LAYER_MAP #:a (send col alpha)
                                           #:r (send col red) #:g (send col green) #:b (send col blue))))
                  ((ann-ship? a)
                   (define st (find-stack ownspace (ann-ship-id a)))
                   (when st
                     (define s (get-topship st))
                     (define col (if highlight? bright dim))
                     (append! sprites (obj-sprite s csd center (get-scale) LAYER_MAP 'target
                                                  (max (/ 35.0 (get-scale))
                                                       (* 4.0 (ship-radius s))) (send col alpha) 0.0 col))))
                  (else
                   (error "don't know how to draw annotation ~v" a))))))))

      ;(append! sprites (draw-background-stars csd center (get-scale) fowlist))

      (for ((o (in-list (space-objects ownspace))))
        (define fowa (if (obj-posvel o) (get-alpha (obj-x o) (obj-y o) fowlist) 1.0))
        (when (fowa . > . 0)
          (append! sprites
                   (draw-object csd textr textsr center (get-scale) o ownspace meid showtab fowa LAYER_EFFECTS fac))))
      
      ; draw stuff specific to the ship you are on
      ; - stacked if we are on a ship inside another ship
      (when my-stack
        (define p (car my-stack))
        (define rcship (if (player-rcid p) (find-id ownspace (player-rcid p)) #f))
        (define ship (get-ship my-stack))
        (define topship (get-topship my-stack))
        (when (and (unbox in-hangar?) (not (ship-hangar ship)))
          (set-box! in-hangar? #f))
        (cond 
          ((unbox in-hangar?)
           ; hangar background
           (define size (* 0.8 (min WIDTH HEIGHT)))
           (append! sprites (sprite 0.0 0.0 (sprite-idx csd 'square-outline) #:layer LAYER_SHIPS
                         #:m (/ (+ size 2.0) (sprite-width csd (sprite-idx csd 'square-outline)))
                         #:r 255 #:g 255 #:b 255))
           (append! sprites (sprite 0.0 0.0 (sprite-idx csd 'square) #:layer LAYER_EFFECTS
                                   #:m (/ size (sprite-width csd (sprite-idx csd 'square))) #:a 0.9))
           ; draw all the ships in the hangar
           (define shipmax 54)
           (for ((s (in-list (ship-hangar ship)))
                 (i (in-naturals)))
             (define x (+ (* -0.5 size) 10 (* (quotient i 4) 180) (/ shipmax 2)))
             (define y (+ (* -0.5 size) 10 (* (remainder i 4) 150) (/ shipmax 2)))
             (append! sprites (sprite x y (sprite-idx csd (string->symbol (ship-type s)))
                                      #:layer LAYER_OVERLAY #:theta (- pi/2)
                                      #:r (get-red ownspace s)))

             ; draw ship hp/res
             (define con (/ (ship-con s) 10.0))
             (define mcon (/ (ship-maxcon s) 10.0))
             (define concol (send the-color-database find-color (stoplight-color con mcon)))
             (append! sprites (sprite (+ x (* 0.5 shipmax) (- (/ mcon 2.0)))
                                      (- y (* 0.5 shipmax))
                                      (sprite-idx csd 'square-outline)
                                      #:layer LAYER_OVERLAY
                                      #:mx (/ (+ 2.0 mcon) (sprite-width csd (sprite-idx csd 'square-outline)))
                                      #:my (/ 6.0 (sprite-height csd (sprite-idx csd 'square-outline)))
                                      #:r 255 #:g 255 #:b 255))
             (append! sprites (sprite (+ x (* 0.5 shipmax) (- (/ con 2.0)))
                                      (+ y (* -0.5 shipmax))
                                      (sprite-idx csd 'square)
                                      #:layer LAYER_UI
                                      #:mx (/ con (sprite-width csd (sprite-idx csd 'square)) 1.0)
                                      #:my (/ 4.0 (sprite-height csd (sprite-idx csd 'square)))
                                      #:r (send concol red) #:g (send concol green) #:b (send concol blue)))

             (define b (button 'normal #f
                               (+ x (/ shipmax 2) 80)
                               (+ y (- (/ shipmax 2)) 15) 150 30 (ship-name s)
                               (lambda (x y)
                                 (send-commands (chmov meid (ob-id s) #f)))))
             (append! buttons b)
             (define players (find-all s player?))
             ;(append! players (player -1 "player1" "fac1" '() #f) (player -1 "player2" "fac2" '() #f))
             (for ((p (in-list players))
                   (i (in-naturals)))
               (append! sprites (text-sprite textr textsr (player-name p)
                                             (+ x (/ shipmax 2) 10)
                                             (+ y (- (/ shipmax 2)) 40 (* i 20)) LAYER_UI_TEXT)))))
          ((not (ship-flying? ship))
           ; our ship is inside another
           ; draw black circle on top of topship
           (append! sprites (obj-sprite topship csd center (get-scale) LAYER_EFFECTS 'circle
                                        (* 2.2 (ship-radius topship)) 0.9 0.0 "black"))
           ; draw our ship inside black circle
           (define-values (x y) (obj->screen topship center (get-scale)))
           (append! sprites (sprite x y (sprite-idx csd (string->symbol (ship-type ship)))
                                    #:layer LAYER_OVERLAY #:m (get-scale) #:theta (- pi/2)
                                    #:r (get-red ownspace ship)))))

        ; draw ship UI
        (append! sprites (draw-ship-hp csd textr center (get-scale) my-stack))
        
        ; draw tool UI
        (when (not (unbox in-hangar?))
          (define tools
            (cond
              (rcship
               (filter-not (lambda (t) (equal? 'engine (tool-name t)))
                           (ship-tools rcship)))
              (else
               (ship-tools ship))))
          (for ((t (in-list tools)))
            (define-values (bs ss)
              (draw-tool-ui csd center (get-scale) ownspace meid rcship t my-stack send-commands active-mouse-tool))
            (append! buttons bs)
            (append! sprites ss)))

        (when (and (not rcship) (ship-hangar ship) (not (unbox in-hangar?)))
          (define b (button 'normal #\h (- RIGHT 80) (+ TOP 70) 140 40
                            (~a "Hangar [h] " (length (ship-hangar ship)))
                            (lambda (x y)
                              (set-box! in-hangar? #t))))
          (append! buttons b))

        ; tool overlay
        ; XXX when we have dock on
        ;(dock? t)
        ;(when (dock-on t)
        ;(draw-docking csd center (get-scale) (get-space my-stack) my-stack)))
        
        ) ; when my-stack
      
      ; draw annotations that exist in canon space
      (for ((a (in-list (space-objects ownspace)))
            #:when (ann? a))
        (when (and (ann-button? a) (or (not (ann-showtab? a)) showtab))
          (define ab (button 'normal #f
                             (obj-x a) (obj-y a) (obj-dx a) (obj-dy a) (ann-txt a)
                             (lambda (k y) (send-commands (anncmd (ob-id a) #f #f)))))
          (append! buttons ab))
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
            (append! sprites (text-sprite textr textsr t
                                          (obj-x a) (+ (obj-y a) (* i 20)) LAYER_UI_TEXT z)))))

      (when showtab
        ; list all players
        (append! sprites (text-sprite textr textsr "Players:"
                                      200 (+ TOP 80) LAYER_UI_TEXT))
        (for ((p (in-list (space-players ownspace)))
              (i (in-naturals)))
          (define str (if (player-faction p)
                          (~a (player-name p) " " (player-faction p))
                          (player-name p)))
          (append! sprites (text-sprite textr textsr str
                                        200 (+ TOP 100 (* i 20)) LAYER_UI_TEXT))))
      
      ; draw orders
      (define line 0)
      (when ordertree
        (define left (+ LEFT 150))
        (append! sprites (text-sprite textr textsr "Orders:"
                                      left TOP LAYER_UI_TEXT))
        (set! left (+ left 80))
        (define top TOP)
        (for-orders ordertree showtab
          (lambda (ot depth highlight?)
            (when showtab
              (define color (if (ord-done? ot) "green" "red"))
              (append! sprites (xy-sprite (+ left (* 10 depth) 5) (+ top (* 20 line) 10)
                                          csd (get-scale) LAYER_UI 'circle (/ 5.0 (get-scale))
                                          1.0 0.0 color)))
            (define color (if highlight? "white" "gray"))
            (define txt (ord-text ot))
            (when (and (ordertime? ot) (string-contains? txt "~a"))
              (define secleft (ceiling (/ (- (ordertime-subtotal ot)
                                             (- (space-time ownspace) (ordertime-start ot)))
                                          1000)))
              (define-values (min sec) (quotient/remainder secleft 60))
              (set! txt (format (ord-text ot)
                                (~a (~a min #:min-width 2 #:align 'right #:pad-string "0")
                                    ":"
                                    (~a sec #:min-width 2 #:align 'right #:pad-string "0")))))
            (append! sprites (text-sprite textr textsr txt
                                      (+ left 12 (* 10 depth)) (+ top (* 20 line)) LAYER_UI_TEXT 1.0 color))
            (set! line (+ line 1)))))
      
      (when (not my-stack)
        (define start-ships
          (find-all ownspace (lambda (o) (and (ship? o)
                                              (ship-flying? o)
                                              (ship-start o)
                                              (equal? fac (ship-faction o))))))
        
        (when (not fac)
          (append! sprites (textr "Waiting for faction assignment..."
                                  0.0 0.0 #:layer LAYER_UI
                                  #:r 255 #:g 255 #:b 255)))
        
        (for ((s (in-list start-ships))
              (i (in-naturals)))
          (define b (button 'normal #f (+ LEFT 100 (* i 250) 100) (- BOTTOM 60 15) 200 30
                            (format "~a" (ship-name s))
                            (lambda (x y)
                              ; leaving sector overview, so center on ship and reset scale
                              (set! scale-play 1.0)
                              (set! center-follow? #t)
                              (send-commands (chmov meid (ob-id s) #f)))))
          (append! buttons b)))
            
      ; draw game UI
      
      ; zoom scale
      (when (not showsector?)
        (define zw 20)
        (define zh 150)
        (define zcx (- RIGHT 10 (/ zw 2)))
        (define zcy (+ TOP 100 (/ zh 2)))
        (append! sprites (sprite zcx (+ zcy (- (/ zh 2))) (sprite-idx csd 'blue)
                                 #:layer LAYER_UI #:mx (/ 20.0 (sprite-width csd (sprite-idx csd 'blue)))))
        (append! sprites (sprite zcx (+ zcy (/ zh 2)) (sprite-idx csd 'blue)
                                 #:layer LAYER_UI #:mx (/ 20.0 (sprite-width csd (sprite-idx csd 'blue)))))
        (append! sprites (sprite zcx zcy (sprite-idx csd 'blue)
                                 #:layer LAYER_UI #:my (/ 150.0 (sprite-height csd (sprite-idx csd 'blue)))))
        
        (define zfrac (/ (- (log scale-play) (log (min-scale)))
                         (- (log (max-scale)) (log (min-scale)))))
        (append! sprites (sprite zcx (+ zcy (/ zh 2) (- (* zfrac zh))) (sprite-idx csd 'blue)
                                 #:layer LAYER_UI #:mx (/ 20.0 (sprite-width csd (sprite-idx csd 'blue)))))
        (define zbutton (button 'hidden #f zcx zcy zw zh "Zoom"
                                (lambda (x y)
                                  (define zfracy (/ (+ (/ zh 2) (- y)) zh))
                                  (define z (exp (+ (log (min-scale))
                                                    (* zfracy (- (log (max-scale)) (log (min-scale)))))))
                                  (set-scale z))))
        (define zkeyb (button 'hidden #\z 0 0 0 0 "Zoom In"
                              (lambda (k y) (set-scale (* scale-play 1.1)))))
        
        (define xkeyb (button 'hidden #\x 0 0 0 0 "Zoom Out"
                              (lambda (k y) (set-scale (/ scale-play 1.1)))))
        
        (append! buttons zbutton zkeyb xkeyb))

        
      (define leave-button (button 'normal 'escape (+ LEFT 50) (+ TOP 25) 100 50 "Exit" #f))
      (define quit-button (button 'normal 'escape (- RIGHT 50) (- BOTTOM 25) 100 50 "Quit" #f))
      (cond
        ((not center-follow?)
         (set-button-label! leave-button "Back")
         (set-button-f! leave-button
                        (lambda (x y)
                          (set! center-follow? #t)))
         (append! buttons leave-button))
        ((not my-stack)
         (set-button-f! quit-button
                        (lambda (x y)
                          (define ans (message-box/custom "Quit?" "Done Playing?"
                                                          "Quit" "Keep Playing" #f
                                                          frame '(default=2)))
                          (when (equal? 1 ans)
                            (drop-connection "clicked exit")
                            (set! playing? #f)
                            (send frame show #f))))
         (append! buttons quit-button))
        ((spacesuit? (get-ship my-stack))
         ; dying
         (set-button-f! leave-button (lambda (x y)
                                       ; reset scale so starting screen shows whole sector
                                       (set! scale-play (min-scale))
                                       (set! center-follow? #t)  ; sector centered
                                       (send-commands (chmov meid #f #f))))
         (append! buttons leave-button))
        ((and (player-rcid (car my-stack)) (find-id ownspace (player-rcid (car my-stack))))
         ; remote controlling something
         )
        ((ship-flying? (get-ship my-stack))
         (cond
           ((unbox in-hangar?)
            (set-button-f! leave-button (lambda (x y) (set-box! in-hangar? #f)))
            (append! buttons leave-button))
           (else
            ; jumping ship
            (set-button-label! quit-button "Jump")
            (set-button-key! quit-button #f)  ; turn off keyboard shortcut
            (set-button-f! quit-button (lambda (x y) (send-commands (chmov meid #f #f))))
            (append! buttons quit-button))))
        ((unbox in-hangar?)
         ; looking into a hangar, just stop looking
         (set-button-f! leave-button (lambda (x y)
                                       (set-box! in-hangar? #f)))
         (append! buttons leave-button))
        (else
         ; leaving this ship into mothership
         (define ms (cadr (get-ships my-stack)))
         (set-button-f! leave-button (lambda (x y)
                                       (send-commands (chmov meid (ob-id ms) #f))))
         (append! buttons leave-button)))
      
      ; draw mouse cursor
      (when my-stack
        (define player (car my-stack))
        (define rcship (if (player-rcid player) (find-id ownspace (player-rcid player)) #f))
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
            #;((or (and mt (find-id ownspace (mtube-mid mt)))
                 (and pt (find-id ownspace (ptube-pid pt)))
                 (and st (tool-online? st)))
             (set! cursordrawn #t)
             (set! clickcmds (command
                              (cond (mt (mtube-mid mt))
                                    (pt
                                     (define probe (find-id ownspace (ptube-pid pt)))
                                     (ob-id (findf steer? (pod-tools (car (ship-pods probe))))))
                                    (else (ob-id st)))
                              a))
             (append! sprites (sprite (exact->inexact x) (exact->inexact y)
                                      (sprite-idx csd 'arrowhead) #:layer LAYER_UI_TEXT
                                      #:theta (- a) #:b 150)))
            ((and (equal? 'pbolt (unbox active-mouse-tool))
                  (ship-tool ship 'pbolt)
                  (ship-flying? ship))
             (set! cursordrawn #t)
             (set! clickcmds (command meid 'pbolt a))
             (append! sprites (sprite (exact->inexact x) (exact->inexact y)
                                      (sprite-idx csd 'target) #:layer LAYER_UI_TEXT
                                      #:r 100 #:g 100 #:b 255 #:m 0.7))))))
      
      (append! buttons
               (button 'hidden #\tab 0 0 0 0 "Mission Info"
                       (lambda (k y) (set! showtab (not showtab))))
               (button 'hidden #\` 0 0 0 0 "Show Sector"
                       (lambda (k y) (set! showsector? (not showsector?)))))

      (append! sprites (draw-overlay textr textsr ownspace my-stack))
  
      ) ; when ownspace

    (send canvas set-cursor (make-object cursor% (if cursordrawn 'blank 'arrow)))

    ; framerate
    (when ((length frames) . > . 1)
      (define start (list-ref frames (- (length frames) 1)))
      (define end (first frames))
      (define span (/ (- end start) 1000))
      (define txt (format "FPS: ~a" (truncate (/ (- (length frames) 1) span))))
      (append! sprites (text-sprite textr textsr txt (- RIGHT 80) TOP LAYER_UI)))
    
    (append! sprites (button-sprites csd textr buttons (if ownspace (space-time ownspace) 0)))
    
    (define-values (dmgx dmgy)
      (if (and (not DEBUG) my-stack)
          (get-dmgfx my-stack)
          (values 0.0 0.0)))

    (define width (+ WIDTH dmgx))
    (define height (+ HEIGHT dmgy))
    ; some machines show visual artifacts sometimes when the layers are exactly aligned?
    ;(when (equal? width (round width)) (set! width (+ width 0.00005)))
    ;(when (equal? height (round height)) (set! height (+ height 0.00005)))
    (define layers (for/vector ((i 8)) (layer width height)))
    
    (define r (render layers '() sprites))
    (r (send canvas get-width) (send canvas get-height) dc))
    
  
  (define-values (left-inset top-inset) (get-display-left-top-inset))
  (define-values (screen-w screen-h) (get-display-size #t))

  ;(printf "insets ~a ~a size ~a ~a\n" left-inset top-inset screen-w screen-h)
  
  (define frame (new frame%
                     (label "Warp")
                     ; use below instead for fullscreen
                     ; (x (- left-inset))
                     ; (y (- top-inset))
                     ; (width screen-w)
                     ; (height screen-h)
                     ; (style '(hide-menu-bar no-caption no-resize-border))
                     ))

  
  
  (define my-canvas%
    (class canvas%
      (super-new)
      (define/override (on-size w h)
        (printf "canvas on-size ~a ~a\n" w h))
      (define/override (on-event event)
        (case (send event get-event-type)
          ((left-down)
           (click this event))
          ((left-up)
           (define v (assoc 'mouse holding))
           (when v ((cdr v)))  ; run the release function
           (set! holding (remove v holding)))  ; cancel hold
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
               (define scale (* (get-scale) (min (/ (send this get-width) WIDTH)
                                                 (/ (send this get-height) HEIGHT))))
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
        (define b (key-button? buttons kc))
        ;(printf "on-char ~v ~v\n" kc b)
        (define v (assoc kc holding))
        (cond
          ((not kc)
           (printf "got #f for on-char get-key-code\n")
           )
          (v
           ; repeated keypress from holding the key down, drop it
           )
          ((equal? kc 'release)
           (set! kc (send event get-key-release-code))
           (define v (assoc kc holding))
           (when v
             ; released the key being held
             ((cdr v))  ; run the release function
             (set! holding (remove v holding))))  ; cancel the hold
          (b
           (when (not (member (button-draw b) '(disabled dmg)))
             ((button-f b) kc #f)
             (when (holdbutton? b)
               (set! holding (cons (cons kc (holdbutton-frelease b)) holding)))))
          (else
           (case kc
             #;((#\d)
              (when ownspace
                (define cmds '())
                (for ((s (in-list (space-objects ownspace)))
                      #:when (spaceship? s))
                  (append! cmds (list (chdam (ob-id s) 10))))

                #;(when my-stack
                  (define s (get-ship my-stack))
                  (append! cmds (dmg-ship s 20 (- pi/2))))
                
                (send-commands cmds)))
             #;((#\m)
              (when ownspace
                (send-commands (message (next-id) (space-time ownspace) #f
                                        (~a "message " (space-time ownspace))))))
             ((wheel-up)
              (when (and ownspace (not showsector?))
                (set-scale (* scale-play 1.05))))
             ((wheel-down)
              (when (and ownspace (not showsector?))
                (set-scale (/ scale-play 1.05))))
             #;((#\u)
              (when ownspace
                (send-commands (chadd (random-upgrade ownspace (posvel -1 0 0 0 (random 100) (random 100) 0)) #f))))
             #;((#\p)
              (when ownspace
                (send-commands (chadd (plasma (next-id) (space-time ownspace) (posvel -1 0 0 (random-between 0 2pi) (random 100) (random 100) 0) (random 10) #f) #f))))
;             ((#\s)
;              (when ownspace
;                (define r (random-between 0 2pi))
;                (define s (random 100))
;                (send-commands (chadd (shield (next-id) (space-time ownspace) (posvel -1 0 0 r (* s (cos r)) (* s (sin r)) 0) (random 30)) #f)))) 
;             ((#\n)
;              (new-stars))
             ))))
      ))


  (define glconfig (new gl-config%))
  (send glconfig set-legacy? #f)

  ; make sure the real canvas is surrounded by black
  (define bc (make-object color% "black"))
  (define vpane (new vertical-pane% (parent frame)))
  (define topblack (new canvas% (parent vpane)))
  (send topblack set-canvas-background bc)

  (define hpane (new horizontal-pane% (parent vpane) (stretchable-height #f)))
  (define leftblack (new canvas% (parent hpane)))
  (send leftblack set-canvas-background bc)
  
  (define canvas
    (new my-canvas%
         (parent hpane)
         (min-width (inexact->exact (round (/ WIDTH 1.0))))
         (min-height (inexact->exact (round (/ HEIGHT 1.0))))
         (stretchable-width #f)
         (stretchable-height #f)
         (paint-callback draw-screen)
         (gl-config glconfig)
         (style '(no-autoclear gl))))

  (define rightblack (new canvas% (parent hpane)))
  (send rightblack set-canvas-background bc)
  (define botblack (new canvas% (parent vpane)))
  (send botblack set-canvas-background bc)
  
  (define sd (make-sprite-db))
  (let ()
    (local-require pict)
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
    (add-sprite!/value sd 'blue
                       (colorize (rectangle 2 2) "blue"))
    (add-sprite!/value sd 'gray
                       (colorize (rectangle 2 2) "gray"))
    (add-sprite!/value sd 'circle
                       (colorize (filled-ellipse 100 100) "black"))
    (add-sprite!/value sd 'shield
                       (colorize (filled-rounded-rectangle 10 100 -.5 #:draw-border? #f) "black"))
    (add-sprite!/value sd 'circle-outline
                       (colorize (inset
                                  (ellipse 100 100 #:border-color "black" #:border-width 2)
                                  2) "black"))
    (add-sprite!/value sd 'square
                       (colorize (filled-rectangle 100 100) "black"))
    (add-sprite!/value sd 'square-outline
                       (colorize (rectangle 100 100 #:border-color "black" #:border-width 10) "black"))
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
    (add-sprite!/value sd 'podarc
                       (colorize (linewidth 10.0
                                   (dc (lambda (dc dx dy)
                                         (define b (send dc get-brush))
                                         (send dc set-brush nocolor 'transparent)
                                         (send dc draw-arc
                                               5 5 100 100 (- (/ pi 4)) (/ pi 4))
                                         (send dc set-brush b))
                                       110 110)) "black"))
    )
  (define textfont (load-font! sd #:size TEXTH #:face "Verdana" #:family 'modern))
  (load-ships sd)
  (add-sprite!/file sd 'plasma (string-append "images/plasma.png"))
  (add-sprite!/file sd 'missile (string-append "images/missile.png"))
  
  (define csd (compile-sprite-db sd))
  ;(save-csd! csd "csd" #:debug? #t)
  (define textr (make-text-aligned-renderer textfont csd))
  (define textsr (make-text-aligned-sizer textfont csd))
  (gl:gl-smoothing? #t)
  (define render (gl:stage-draw/dc csd (fl->fx WIDTH) (fl->fx HEIGHT) LAYER_NUM))
  
  (send frame show #t)
  
  (define start-space-time #f)
  (define start-time #f)
  
  (define (calc-dt curtime start curspace startspace)
    (- (- curtime start) (- curspace startspace)))
  
  (define (tick-space! space)
    (set-space-time! space (+ (space-time space) TICK))
    (for ((o (in-list (space-objects space))))
      (update-physics! space o (/ TICK 1000.0))
      (when (ship? o) (repair-subships! (/ TICK 1000.0) o))
      (add-backeffects! space o)))
  
  
  
  (define (drop-connection msg)
    (printf "drop server ~a\n" msg)
    (when server-in-port
      (close-input-port server-in-port)
      (close-output-port server-out-port))
    (set! server-in-port #f)
    (set! server-out-port #f)
    (set! meid #f)
    (set! ownspace #f))
  
  
  (define (send-commands cmds)
    (when (not (list? cmds)) (set! cmds (list cmds)))
    (when ((length cmds) . > . 0)
      ;(printf "send-commands ~v\n" cmds)
      (with-handlers ((exn:fail:network? (lambda (exn)
                                           (drop-connection "send-command"))))
        (write (update (if ownspace (space-time ownspace) #f) cmds #f) server-out-port)
        (flush-output server-out-port))))
  
  
  (define (read-from-server)
    (with-handlers ((exn:fail:network? (lambda (exn)
                                         (drop-connection "read-from-server")
                                         eof)))
      (define x (read server-in-port))
      (when (eof-object? x)
        (drop-connection "read got eof"))
      x))
  
  
  (define (client-loop)
    (define start-loop-time (current-milliseconds))
    
    (when (not server-in-port)
      (define newname
        #;"Testing" (get-text-from-user "Player Name"
                            "Player Name"
                            #f
                            (or name "")))
      
      (when (not newname) (exit 0))
      (when newname (set! name newname))
      
      ; ask the user for address
      (define newip
        #;"127.0.0.1" (get-text-from-user "IP of server"
                            "IP address of the Server"
                            #f
                            (or ip "")))
      
      (when (not newip) (exit 0))
      (when newip
        (set! ip newip)
        
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
          ; send our name to the server
          (send-commands (player #f name #f '() #f)))
        
        (when server-in-port
          ; read a player struct that has our unique id
          (define newme (read-from-server))
          (when (not (eof-object? newme))
            (set! meid (ob-id newme))
            (idimag meid)))))
    
    ; get new world
    (while (and server-in-port (byte-ready? server-in-port))
      (define input (read-from-server))
      ;(printf "client input: ~v\n" input)
      (cond ((space? input)
             (set! ownspace input)
             (set! start-space-time (space-time ownspace))
             (set! start-time (current-milliseconds))
             (set! last-update-time start-space-time)

             ; new ownspace, reset view stuff
             (set! showsector? #f)
             (set! center #f)
             (set! center-follow? #t)
             (set-posvel-x! (obj-posvel centerxy) 0)
             (set-posvel-y! (obj-posvel centerxy) 0)
             (set! dragstate "none")

             (when (not (find-id ownspace meid))
               ; set scale so we see the whole sector
               (set! scale-play (min-scale)))
             )
            ((and ownspace (update? input))
             (when (not (= (update-time input) (+ last-update-time TICK)))
               (error "UPDATE TIMES DID NOT MATCH\n"))
             (set! last-update-time (update-time input))
             
             ;(printf "client update space-time ~a update-time ~a\n" (space-time ownspace) (update-time input))
             
             (when ((space-time ownspace) . < . (update-time input))
               ;(printf "client ticking ownspace forward for input ~a\n" (update-time input))
               (tick-space! ownspace))
             (when ((space-time ownspace) . < . (update-time input))
               (error "client ownspace still behind update time\n"))
             (for ((c (in-list (update-changes input))))
               ;(printf "client applying change ~v\n" c)
               (define-values (forward? useless-changes)
                 (apply-change! ownspace c (update-time input) "client"))
               (when (and (chmov? c) (equal? meid (chmov-id c)))
                 (set-box! in-hangar? #f))
               (when (not (null? useless-changes))
                 (printf "client produced useless changes:\n  ~v\n" useless-changes))
               )
             (for ((pvu (in-list (update-pvs input))))
               (update-posvel! ownspace pvu (update-time input)))))
      
      (when ownspace
        ; If the first space is delayed, then our own clock got started late.
        ; Need to use (current-milliseconds) here in case we hiccupped
        ; since the start of the loop
        (define dt (calc-dt (current-milliseconds) start-time (space-time ownspace) start-space-time))
        ;(printf "calc-dt ~a ~a ~a ~a ~a\n" (current-milliseconds) start-time (space-time ownspace) start-space-time dt)
        (when (dt . < . 0)
          (printf "started too late ~a\n" dt)
          (set! start-time (- start-time (- dt)))))
      )
    
    (when ownspace
      (set! my-stack (find-stack ownspace meid))
      (when (and my-stack (get-ship my-stack))
        (define tools (map tool-name (ship-tools (get-ship my-stack))))
        (cond
          ((member (unbox active-mouse-tool) tools)
            ; our tool exists in whatever ship we find ourselves in
           )
          (else
           ; our tool doesn't exist, default to the first available
           (set-box! active-mouse-tool (for/first ((t tools)
                                                   #:when (member t MOUSE_TOOLS))
                                         t)))))
        
      
      ; physics prediction
      (define dt (calc-dt (current-milliseconds) start-time (space-time ownspace) start-space-time))
      (when (dt . > . TICK)
        ;(printf "client ticking forward for prediction ~a\n" dt)
        (tick-space! ownspace)
        (set! dt (calc-dt (current-milliseconds) start-time (space-time ownspace) start-space-time)))
      
      ;(printf "client is ahead by ~a\n" (- (space-time ownspace) last-update-time))
      )
    
    ;rendering
    ;(printf "client render ~a" (current-milliseconds))
    (set! frames (add-frame-time (current-milliseconds) frames))
    (send canvas refresh-now #:flush? #f)
    ;(printf "  ~a\n" (current-milliseconds))
    
;    (when (time-for (current-milliseconds) 1000)
;      (displayln (~a "mem: " (~r (/ (current-memory-use) (* 1024.0 1024.0)) #:precision 2))))
          
    ; sleep so we don't hog the whole racket vm
    (define sleep-time
      (add1
       (if ownspace
           (- (calc-dt (current-milliseconds) start-time
                       (+ (space-time ownspace) TICK) start-space-time))
           (- (+ start-loop-time TICK) (current-milliseconds)))))
    
    (cond
      ((sleep-time . > . 0)
       ;(printf "client sleeping ~a\n" sleep-time)
       (sleep/yield (/ sleep-time 1000.0)))
      (else
       ;(printf "client skipping sleep ~a\n" sleep-time)
       (sleep/yield .001)))
    
    (flush-output)
    (collect-garbage 'incremental)
    (when playing? (client-loop)))
  
  (queue-callback client-loop #f))



(module+ main
  ;(require profile)
  ;(profile #:threads #t #:delay 0.0
    ;(begin
    (start-client "127.0.0.1" PORT "Dave" #f #f)
    (yield 'wait)
    ;))
  ;(exit 0)
  )
