#lang racket/gui

(require "defs.rkt"
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
  
  (define server-in-port #f)
  (define server-out-port #f)
  (define meid #f)  ; integer? or #f
  (define ownspace #f)
  (define my-stack #f)
  (define buttons #f)
  ; if you are holding a key/mouse button (from a holdbutton?), this is a pair
  ; car is keycode (or 'mouse) of the key that's being held
  ; cdr is the holdbutton-frelease function
  (define holding? #f)
  ; if the mouse is over something (other than a button) that when clicked will
  ; do some action, clickcmds is a list of commands we send
  (define clickcmds #f)
  (define frames '())  ; list of last few frame times
  (define last-update-time #f)

  (define showtab #f)  ; tab toggles an overlay showing players and goals
  (define showsector? #f)  ; tilde toggles showing the whole sector or the regular view
  (define zerocenter (obj #f #f (posvel #f 0 0 #f #f #f #f)))

  (define center #f)  ; updated each frame for the click handler and mouse cursor drawing
  (define center-follow? #t)  ; show player position in the center?

  ; when (not center-follow?)
  (define centerxy (obj #f #f (posvel #f 0 0 #f #f #f #f)))  ; center of the screen when panning
  (define dragxypx '(0 . 0))  ; last xy of drag in pixels
  (define dragstate "none")  ; "none", "start", "drag"
  
  (define scale-play 1.0)  ; scale when we are in a normal pod
  (define scale-ship 10.0)  ; scale when we are in a lounge/hangar
  (define (scale-ship?)
    (cond (my-stack
           (define pod (get-pod my-stack))
           (define ship (get-ship my-stack))
           (if (and (spaceship? ship) (or (lounge? pod) (hangar? pod)))
               #t #f))
          (else #f)))
  (define (min-scale)
    (if ownspace
        (min (/ WIDTH (space-width ownspace)) (/ HEIGHT (space-height ownspace)))
        .01))
  (define (max-scale) 30.0)
    
  (define (get-future-scale)
    (if (scale-ship?) scale-ship scale-play))
  (define (set-scale z)
    (define newz (clamp (min-scale) (max-scale) z))
    (if (scale-ship?) (set! scale-ship newz) (set! scale-play newz)))
  
  (define shown-scale (get-future-scale))  ; scale that is actually used
  (define (get-scale) (if showsector? (min-scale) shown-scale))
  (define (update-scale)  ; move shown-scale towards the scale we want
    (define target (get-future-scale))
    (define diff (abs (- target shown-scale)))
    (if (shown-scale . < . target)
        (set! shown-scale (min target (+ shown-scale (max 0.1 (* 0.4 diff)))))
        (set! shown-scale (max target (- shown-scale (max 0.1 (* 0.4 diff)))))))
  
  
  (define (in-button? buttons x y)
    (for/first ((b (in-list buttons))
                #:when (if (button-height b)
                           (and (<= (button-x b) x (+ (button-x b) (button-width b)))
                                (<= (button-y b) y (+ (button-y b) (button-height b))))
                           (and (<= (sqrt (+ (* (- x (button-x b)) (- x (button-x b)))
                                             (* (- y (button-y b)) (- y (button-y b)))))
                                    (button-width b)))))
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
           (set! holding? (cons 'mouse (holdbutton-frelease b))))
         ))
      (clickcmds
       (send-commands clickcmds))))
  
  
  (define (draw-screen canvas dc)
;    (when (and serverspace ownspace)
;      (printf "serverspace time ~a\n   ownspace time ~a\n" (space-time serverspace) (space-time ownspace)))
    
    (send dc set-smoothing 'smoothed)
    (send dc set-background bgcolor)

    (send dc set-font normal-control-font)
    
    (keep-transform dc
      ; make sure whole screen is fog of war gray
      (send dc set-clipping-region #f)
      (send dc set-background (linear-color "black" "white" 0.13 1.0))
      (send dc clear)

      ; scale to canon
      (send dc translate (/ (send canvas get-width) 2) (/ (send canvas get-height) 2))
      (define scale (min (/ (send canvas get-width) WIDTH) (/ (send canvas get-height) HEIGHT)))
      (send dc scale scale (- scale))
      ; transformation is (center of screen, y up, WIDTHxHEIGHT logical units, rotation clockwise)
      ; must reverse y axis when drawing text
      
      ; reset alpha in case a damage effect changed it last frame
      (send dc set-alpha 1.0)

      (set! buttons '())
      (define cursordrawn #f)
      (set! clickcmds #f)
      
      (when (and (not DEBUG) my-stack)
        (draw-dmgfx dc my-stack))

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
        (draw-text dc "Intro Screen" 0 0))

      (when ownspace
        (define p (findfid meid (space-players ownspace)))
        (define fac (if p (player-faction p) #f))
        (when (and (not fac) my-stack)
          (set! fac (ship-faction (get-ship my-stack))))
        (define ordertree (get-space-orders-for ownspace fac))
        
        (keep-transform dc
          (send dc scale (get-scale) (get-scale))
          (send dc translate (- (obj-x center)) (- (obj-y center)))
          
          ; make fog of war region
          (define fow (new region% (dc dc)))
          (for ((s (in-list (space-objects ownspace)))
                #:when (and fac (ship? s) (equal? (ship-faction s) fac)))
            (define rad (ship-radar s))
            (define reg (new region% (dc dc)))
            (send reg set-ellipse (- (obj-x s) rad) (- (obj-y s) rad) (* 2 rad) (* 2 rad))
            (send fow union reg))

          ; make the background black for places you can see
          (send dc set-clipping-region fow)
          (send dc set-background "black")
          (send dc clear)

          ; turn off clipping for map stuff
          (send dc set-clipping-region #f)
          (draw-sector-lines dc ownspace)

          ; map annotations
          
          
          ; order annotations
          (when ordertree
            (define a (cycletri (space-time ownspace) 3000))
            (define bright (linear-color "blue" "blue" 1.0 (+ 0.5 (* 0.5 a))))
            (define dim (linear-color "blue" "blue" 1.0 0.5))
            (for-orders ordertree showtab
                        (lambda (ot depth highlight?)
                          (when (order? ot)
                            (for ((a (in-list (order-anns ot))))
                              (cond
                                ((ann-circle? a)
                                 (keep-transform dc
                                   (define col (if highlight? bright dim))
                                   (send dc set-pen col (/ 2.0 (dc-point-size dc)) 'solid)
                                   (send dc set-text-foreground col)
                                   (send dc set-brush nocolor 'transparent)
                                   (send dc translate (obj-x a) (obj-y a))
                                   (define r (ann-circle-radius a))
                                   (send dc draw-ellipse (- r) (- r) (* 2 r) (* 2 r))
                                   (send dc scale (/ 1.0 (get-scale)) (/ 1.0 (get-scale)))
                                   (draw-text dc (ann-txt a) 0 0)))
                                ((ann-ship? a)
                                 (define st (find-stack ownspace (ann-ship-id a)))
                                 (when st
                                   (define s (get-topship st))
                                   (define col (if highlight? bright dim))
                                   (send dc set-pen col (/ 2.0 (dc-point-size dc)) 'solid)
                                   (send dc set-brush nocolor 'transparent)
                                   (define r (* 2.0 (ship-radius s)))
                                   (keep-transform dc
                                     (send dc translate (obj-x s) (obj-y s))
                                     (send dc draw-ellipse (- r) (- r) (* 2 r) (* 2 r))
                                     (send dc draw-line (- r) 0 r 0)
                                     (send dc draw-line 0 (- r) 0 r))))
                                (else
                                 (error "don't know how to draw annotation ~v" a))))))))
          
          ;; debug, show radar distance for all ships
          (when DEBUG
            (send dc set-brush nocolor 'transparent)
            (send dc set-pen "pink" (/ 1.0 (dc-point-size dc)) 'solid)
            (for ((s (in-list (space-objects ownspace)))
                  #:when (ship? s))
              (keep-transform dc
                (define r (ship-radar s))
                (send dc translate (obj-x s) (obj-y s))
                (send dc draw-ellipse (- r) (- r) (* 2 r) (* 2 r)))))
          
          ; turn clipping back on for regular stuff
          (when (not DEBUG)
            (send dc set-clipping-region fow))
          (draw-background-stars dc center (get-scale))
          (draw-objects dc ownspace meid showtab)

          ; draw stuff specific to the ship you are on
          ; - stacked if we are on a ship inside another ship
          ; - pod buttons to change pods
          (when my-stack
            (keep-transform dc
              (center-on dc (get-topship my-stack) #f)
              
              ; if we are on a ship inside another ship, draw our ships stacked
              (define last-ship #f)
              (for ((s (in-list (reverse (filter ship? my-stack))))
                    (i (in-naturals)))
                (cond
                  ((= i 0) (set! last-ship s))
                  (else
                   (send dc set-pen nocolor 1 'transparent)
                   (send dc set-brush (make-color 0 0 0 0.8) 'solid)
                   (define r (* 0.8 (ship-radius last-ship)))
                   (send dc draw-ellipse (- r) (- r) (* 2 r) (* 2 r))
                   (when showtab (draw-playerlist dc s))
                   (keep-transform dc
                     (send dc rotate (- pi/2))
                     (draw-ship-raw dc s)
                     (draw-ship-info dc s ownspace))
                   )))

              (define ship (get-ship my-stack))
              (define rot (if (ship-flying? ship) (obj-r ship) pi/2))
              (send dc rotate (- rot))
              
              (when (and (not (spacesuit? ship))
                         (not (hangar? (get-pod my-stack))))
                (define bs
                  (draw-pods dc ship rot my-stack send-commands canvas meid))
                (set! buttons (append buttons bs))))

            ; transform is back to scaled space
            (for ((t (in-list (pod-tools (get-pod my-stack)))))
              (draw-tool-overlay dc t my-stack)))

          )
        ; now we are back to the canon transform

        ; turn clipping off so we can draw all the UI stuff
        (send dc set-clipping-region #f)

        (when my-stack
          (when (hangar? (get-pod my-stack))
            ; draw hangar background
            (send dc set-pen fgcolor 1.0 'solid)
            (send dc set-brush (make-color 0 0 0 .8) 'solid)
            (define size (* 0.8 (min WIDTH HEIGHT)))
            (send dc draw-rectangle (* -0.5 size) (* -0.5 size) size size)
            
            ; draw all the ships in the hangar
            (define shipmax 54)
            (for ((s (in-list (hangar-ships (get-pod my-stack))))
                  (i (in-naturals)))
              (keep-transform dc
                (send dc translate
                      (+ (* -0.5 size) 10 (* (quotient i 4) 180) (/ shipmax 2))
                      (- (* 0.5 size) 10 (* (remainder i 4) 150) (/ shipmax 2)))
                (keep-transform dc
                  (send dc rotate (- pi/2))
                  (draw-ship-raw dc s)
                  (draw-ship-info dc s ownspace))
                (send dc translate (+ 10 (/ shipmax 2)) (/ shipmax 2))
                (define-values (x y) (dc->canon canvas dc 0 -30))
                (define bl (lambda (x y)
                             (send-commands (chrole meid (ob-id (ship-lounge s))))))
                (append! buttons (button 'normal #f x y 105 30 (ship-name s) bl))

                (send dc set-text-foreground "white")
                (define players (find-all s player?))
                ;(append! players (player -1 "player1" "fac1") (player -1 "player2" "fac2"))
                (for ((p (in-list players))
                      (i (in-naturals)))
                  (draw-text dc (player-name p) 0 (- -32 (* i 20))))
                )))

          ; draw pod UI
          (draw-pod-ui dc my-stack)
        
          ; draw tool UI
          (for ((t (in-list (pod-tools (get-pod my-stack)))))
            (define bs (draw-tool-ui dc ownspace t my-stack send-commands))
            (append! buttons bs))

          ) ; when my-stack

        ; draw annotations that exist in canon space
        (for ((a (in-list (space-objects ownspace)))
                #:when (ann? a))
          (when (and (ann-button? a) (or (not (ann-showtab? a)) showtab))
            (define ab (button 'normal #f
                               (obj-x a) (obj-y a) (obj-dx a) (obj-dy a) (ann-txt a)
                               (lambda (k y) (send-commands (anncmd (ob-id a) #f)))))
            (append! buttons ab))
          (when (and (ann-text? a) (or (not (ann-showtab? a)) showtab))
            (cond
              ((ann-text-life a)
               (define z (linear-fade (obj-age ownspace a)
                                      (ann-text-life a)
                                      (+ (ann-text-life a) MSG_FADE_TIME)))
               (define cc (linear-color "white" "white" z z))
               (send dc set-text-foreground cc))
              (else
               (send dc set-text-foreground "white")))
            (define txts (string-split (ann-txt a) "\n"))
            (for ((t (in-list txts))
                  (i (in-naturals)))
              (draw-text dc t (obj-x a) (- (obj-y a) (* i 20))))))

        (send dc set-text-foreground "white")
        
        (when showtab
          ; list all players
          (draw-text dc "Players:" 200 (- TOP 80))
          (for ((p (in-list (space-players ownspace)))
                (i (in-naturals)))
            (define str (if (player-faction p)
                            (~a (player-name p) " " (player-faction p))
                            (player-name p)))
            (draw-text dc str 200 (- TOP 100 (* i 20)))))
        
        ; draw orders
        (define line 0)
        (send dc set-pen nocolor 1 'transparent)
        (when ordertree
          (define left (+ LEFT 130))
          (draw-text dc "Orders:" left TOP)
          (set! left (+ left 50))
          (define top TOP)
          (for-orders ordertree showtab
                      (lambda (ot depth highlight?)
                        (when showtab
                          (if (ord-done? ot)
                              (send dc set-brush "green" 'solid)
                              (send dc set-brush "red" 'solid))
                          (send dc draw-ellipse (+ left (* 10 depth) 2) (- top (* 20 line) 9) 5 5))
                        (if highlight?
                            (send dc set-text-foreground "white")
                            (send dc set-text-foreground "gray"))
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
                        (draw-text dc txt (+ left 12 (* 10 depth)) (- top (* 20 line)))
                        (set! line (+ line 1)))))

        (when (not my-stack)
          (define start-stacks
            (search ownspace (lambda (o) (and (ship? o)
                                              (ship-flying? o)
                                              (ship-start o)
                                              (equal? fac (ship-faction o)))) #t))

          (when (not fac)
            (draw-text dc "Waiting for faction assignment..." -100 0))
          
          (for ((s (in-list start-stacks))
                (i (in-naturals)))
            (define mp (car s)) 
            (define b (button 'normal #f (+ LEFT 100 (* i 250)) (+ BOTTOM 60) 200 30
                              (format "~a" (ship-name (get-ship s)))
                              (lambda (x y)
                                ; leaving sector overview, so center on ship and reset scale
                                (set! scale-play 1.0)
                                (set! center-follow? #t)
                                (send-commands (chrole meid (ob-id (ship-lounge (get-ship s))))))))
            (append! buttons b)))
            
        ; draw game UI
        
        ; zoom scale
        (when (not showsector?)
          (define zw 20)
          (define zh 150)
          (define zx (- RIGHT 10 zw))
          (define zy (- TOP 70 zh))
          (send dc set-pen "blue" 1.5 'solid)
          (send dc draw-line zx zy (+ zx zw) zy)
          (send dc draw-line zx (+ zy zh) (+ zx zw) (+ zy zh))
          (send dc draw-line (+ zx (/ zw 2.0)) zy (+ zx (/ zw 2.0)) (+ zy zh))
          (define zfrac (/ (- (log (get-future-scale)) (log (min-scale)))
                           (- (log (max-scale)) (log (min-scale)))))
          (define zfracy (+ zy (* zfrac zh)))
          (send dc draw-line (+ zx 2) zfracy (+ zx zw -2) zfracy)
          (define zbutton (button 'hidden #f zx zy zw zh "Zoom"
                                  (lambda (x y)
                                    (define zfracy (/ y zh))
                                    (define z (exp (+ (log (min-scale))
                                                      (* zfracy (- (log (max-scale)) (log (min-scale)))))))
                                    (set-scale z))))
          (define zkeyb (button 'hidden #\z 0 0 0 0 "Zoom In"
                                (lambda (k y) (set-scale (* (get-future-scale) 1.1)))))
          
          (define xkeyb (button 'hidden #\x 0 0 0 0 "Zoom Out"
                                (lambda (k y) (set-scale (/ (get-future-scale) 1.1)))))
          
          (append! buttons zbutton zkeyb xkeyb))

        
        (define leave-button (button 'normal 'escape LEFT (- TOP 50) 50 50 "Exit" #f))
        (define quit-button (button 'normal 'escape (- RIGHT 50) BOTTOM 50 50 "Quit" #f))
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
                 (exit 0))))
           (append! buttons quit-button))
          ((and (lounge? (get-pod my-stack)) (spacesuit? (get-ship my-stack)))
           ; dying
           (set-button-f! leave-button (lambda (x y)
                                         ; reset scale so starting screen shows whole sector
                                         (set! scale-play (min-scale))
                                         (set! center-follow? #t)  ; sector centered
                                         (send-commands (chrole meid #f))))
           (append! buttons leave-button))
          ((and (lounge? (get-pod my-stack)) (ship-flying? (get-ship my-stack)))
           ; jumping ship
           (set-button-label! quit-button "Jump")
           (set-button-key! quit-button #f)  ; turn off keyboard shortcut
           (set-button-f! quit-button (lambda (x y) (send-commands (chrole meid "spacesuit"))))
           (append! buttons quit-button))
          ((lounge? (get-pod my-stack))
           ; leaving this ship into mothership hangar
           (define ms (cadr (get-ships my-stack)))
           (set-button-f! leave-button (lambda (x y)
                                         (send-commands (chrole meid (ob-id (ship-hangar ms))))))
           (append! buttons leave-button))
          (else
           ; move to lounge
           (set-button-f! leave-button
                          (lambda (x y)
                            (send-commands (chrole meid (ob-id (ship-lounge (get-ship my-stack)))))))
           (append! buttons leave-button)))
          
        ; draw mouse cursor
        (when my-stack
          (define-values (p mods) (get-current-mouse-state))
          (define-values (wx wy) (send canvas screen->client
                                       (+ (send p get-x) left-inset)
                                       (+ (send p get-y) top-inset)))
          (define-values (x y) (screen->canon canvas wx wy))
          (when (not (in-button? buttons x y))
            (define mypos (get-center ownspace my-stack))
            (define-values (mx my) (space->canon center (get-scale) (obj-x mypos) (obj-y mypos)))
            (define a (angle-norm (atan0 (- y my) (- x mx))))
            (send dc set-pen "blue" (/ 1.5 (dc-point-size dc)) 'solid)
            (send dc set-brush nocolor 'transparent)
            
            (define p (get-pod my-stack))
            (define mt (findf mtube? (pod-tools p)))
            (define st (findf steer? (pod-tools p)))
            (define pb (findf pbolt? (pod-tools p)))
            (define sb (findf shbolt? (pod-tools p)))
            (cond
              ((or (and mt (find-id ownspace (mtube-mid mt)))
                   (and st (tool-online? st)))
               (set! cursordrawn #t)
               (set! clickcmds (command (if mt (mtube-mid mt) (ob-id st)) a))
               (keep-transform dc
                 (send dc translate x y)
                 (send dc rotate (- a))
                 (send dc draw-lines '((0 . 0) (-15 . -5) (-15 . 5) (0 . 0)))))
              ((or (and pb (pbolt-aim pb) (tool-online? pb))
                   (and sb (shbolt-aim sb) (tool-online? sb)))
               (define s (get-ship my-stack))
               (when (ship-flying? s)
                 (define pf (angle-add (obj-r s) (pod-facing p)))
                 (when ((abs (angle-frto pf a)) . < . (/ (pod-spread p) 2.0))
                   (set! cursordrawn #t)
                   (set! clickcmds (command (ob-id (or pb sb)) a))
                   (keep-transform dc
                     (send dc translate x y)
                     (send dc draw-ellipse (- 10) (- 10) 20 20)
                     (send dc draw-line -12 0 12 0)
                     (send dc draw-line 0 -12 0 12))))))))
        
        (append! buttons
                 (button 'hidden #\tab 0 0 0 0 "Mission Info"
                         (lambda (k y) (set! showtab (not showtab))))
                 (button 'hidden #\` 0 0 0 0 "Show Sector"
                         (lambda (k y) (set! showsector? (not showsector?)))))

        (draw-overlay dc ownspace my-stack)
        (draw-framerate dc frames)
        
        ) ; when ownspace

      (send canvas set-cursor (make-object cursor% (if cursordrawn 'blank 'arrow)))
      (draw-buttons dc buttons (if ownspace (space-time ownspace) 0))
    ))
    
  
  (define-values (left-inset top-inset) (get-display-left-top-inset))
  (define-values (screen-w screen-h) (get-display-size #t))

  ;(printf "insets ~a ~a size ~a ~a\n" left-inset top-inset screen-w screen-h)
  
  (define frame (new frame%
                     (label "Warp")
                     (width (inexact->exact (round (/ WIDTH 1.5))))
                     (height (inexact->exact (round (/ HEIGHT 1.5))))
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
      (define/override (on-event event)
        (case (send event get-event-type)
          ((left-down)
           (click this event))
          ((left-up)
           (when (and holding? (equal? (car holding?) 'mouse))
             ((cdr holding?))  ; run the release function
             (set! holding? #f)  ; cancel the hold
             ))
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
               (define dx (/ (- (send event get-x) (car dragxypx)) scale))
               (define dy (/ (- (send event get-y) (cdr dragxypx)) scale))
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
        
        (cond
          ((not kc)
           (printf "got #f for on-char get-key-code\n")
           )
          ((and holding? (equal? kc (car holding?)))
           ; repeated keypress from holding the key down, drop it
           )
          ((and holding? (equal? kc 'release)
                (equal? (car holding?) (send event get-key-release-code)))
           ; released the key being held
           ((cdr holding?))  ; run the release function
           (set! holding? #f)  ; cancel the hold
           )
          (b
           (when (not (member (button-draw b) '(disabled dmg)))
             ((button-f b) kc #f)
             (when (holdbutton? b)
               (set! holding? (cons kc (holdbutton-frelease b))))
             ))
          (else
           (case kc
;             ((#\h)
;              (printf "hello\n"))
             ((#\d)
              (when ownspace
                (define cmds '())
                (for ((s (in-list (space-objects ownspace)))
                      #:when (spaceship? s))
                  (append! cmds (list (chdam (ob-id s) 10))))

                (when my-stack
                  (define s (get-ship my-stack))
                  (append! cmds (dmg-ship s 20 (- pi/2))))
                
                (send-commands cmds)))
             ((#\m)
              (when ownspace
                (send-commands (message (next-id) (space-time ownspace) #f
                                        (~a "message " (space-time ownspace))))))
             ((wheel-up)
              (when (and ownspace (not showsector?))
                (set-scale (* (get-future-scale) 1.05))))
             ((wheel-down)
              (when (and ownspace (not showsector?))
                (set-scale (/ (get-future-scale) 1.05))))
;             ((#\p)
;              (when ownspace
;                (send-commands (chadd (plasma (next-id) (space-time ownspace) (posvel -1 0 0 (random-between 0 2pi) (random 100) (random 100) 0) (random 100) #f) #f))))
;             ((#\s)
;              (when ownspace
;                (define r (random-between 0 2pi))
;                (define s (random 100))
;                (send-commands (chadd (shield (next-id) (space-time ownspace) (posvel -1 0 0 r (* s (cos r)) (* s (sin r)) 0) (random 30)) #f)))) 
;             ((#\j)
;              (set-scale (* (get-scale) 1.1)))
;             ((#\k)
;              (set-scale (/ (get-scale) 1.1)))
;             ((#\n)
;              (new-stars))
             ))))
      ))
  
  (define canvas
    (new my-canvas%
         (parent frame)
         (paint-callback draw-screen)
         (style '(no-autoclear))))

  (load-ships)
  (load-plasma)
  (load-missile)
  
  (send frame show #t)
  
  (define start-space-time #f)
  (define start-time #f)
  
  (define (calc-dt curtime start curspace startspace)
    (- (- curtime start) (- curspace startspace)))
  
  (define (tick-space! space)
    (set-space-time! space (+ (space-time space) TICK))
    (for ((o (in-list (space-objects space))))
      (update-physics! space o (/ TICK 1000.0))
      (when (ship? o) (update-energy! (/ TICK 1000.0) o 0.0))
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
        (write cmds server-out-port)
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
          (send-commands (player #f name #f)))
        
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
    (send canvas refresh-now)
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
    (client-loop))
  
  (queue-callback client-loop #f))

(module+ main
  ;(require profile)
  ;(profile #:threads #t
  ;  (begin
    (start-client "127.0.0.1" PORT "Dave" #t #f)
    (semaphore-wait (make-semaphore))
    ;))
  )
