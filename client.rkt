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


(define (start-client ip port
                      #:name [name "Racketeer"]
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

  (define key-for #f)
  ; #f for not entering anything
  ; 'name for entering name
  ; 'ip for entering ip address

  ; set to #t while trying to connect to server
  ; - reset to #f when we fail to connect or disconnect
  (define connecting? #f)
  ; shown on the intro screen, used for server connection problems
  (define intromsg "Press ? for quick help")

  (define help-screen? #f)
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

  (define frame #f)
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
       (send-commands (clickcmds #f)))))


   (define (quit?)
     (define ans (message-box/custom "Quit?" "Ready to Stop?"
                                     "Quit" "Keep Playing" #f
                                     frame '(default=2)))
     (when (equal? 1 ans)
       (drop-connection!)
       (set! playing? #f)
       (send frame show #f)))
  
  
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
    
    (set! buttons '())
    (set! sprites '())
    (define cursordrawn #f)
    (set! clickcmds #f)
    (define background-gray 0)
    
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

    (cond
     (help-screen?
      ; This screen shows quick help for the game
      (define size (* 0.8 (min canon-width canon-height)))
      (prepend! sprites (rect-outline csd 0.0 0.0 size size 3.5
                                      LAYER_HANGAR_BACKGROUND))
      (prepend! buttons (button 'normal 'back-to-game #f
                                0.0 (- (/ size 2.0) 33.0) 200.0 50.0
                                "Back to Game"
                                (lambda (x y)
                                  (set! help-screen? #f))))

      (define col (send the-color-database find-color "white"))

      (prepend! sprites
                (textr "Quick Help"
                       0.0 (+ (- (/ size 2.0)) 15.0) #:layer LAYER_UI
                       #:r (send col red)
                       #:g (send col green)
                       #:b (send col blue)))

      (define help-lines
        (list "ctrl-f toggles fullscreen"
              "ctrl-q to quit"
              "tab shows players"
              "backtick toggles full map"
              "r/t or mouse wheel zooms"
              "right-click drag to pan view"
              ""
              "a/w/d to fly"
              "space or mouse to fire"))
      
      (for ((t (in-list help-lines))
            (i (in-naturals 2)))
        (prepend! sprites
                  (text-sprite textr textsr t
                               (+ (- (/ size 2.0)) 8.0)
                               (+ (- (/ size 2.0)) 8.0 (* i 20))
                               LAYER_UI)))
      )
     ((not ownspace)
      (timeit t1
      ; This screen is where you type in your name and the server IP
      ;(define txt "O")
      ;(define-values (w h) (textsr txt))
      ;(for* ((i (in-range (left) (right) w))
      ;       (j (in-range (top) (bottom) h)))
      ;  (prepend! sprites (text-sprite textr textsr txt i j LAYER_UI)))

      (prepend! sprites (sprite 0.0 0.0 (sprite-idx csd 'intro) #:layer LAYER_FOW_BLACK))
      
      (prepend! sprites (text-sprite textr textsr "Name" -100.0 0.0 LAYER_UI))
      (define name? (equal? key-for 'name))
      (define nb (button (if name? 'disabled 'normal)
                         'name #f
                         0.0 35.0 200.0 26.0
                         (string-append name (if name? "_" ""))
                         (if name?
                             (lambda (x y) (void))
                             (lambda (x y) (set! key-for 'name)))))
      (prepend! buttons nb)

      (prepend! sprites (text-sprite textr textsr "Server IP" -100.0 55.0 LAYER_UI))
      (define ip? (equal? key-for 'ip))
      (define ipb (button (if ip? 'disabled 'normal)
                          'ip #f
                          0.0 90.0 200.0 26.0
                          (string-append ip (if ip? "_" ""))
                          (if ip?
                              (lambda (x y) (void))
                              (lambda (x y) (set! key-for 'ip)))))
      (prepend! buttons ipb)

      (define startb (button (if connecting? 'disabled 'normal) 'start #f
                             0.0 150.0 200.0 50.0
                             (if connecting? "Connecting..." "Connect")
                             (lambda (x y)
                               (connect/async!))))
      (prepend! buttons startb)

      (define txts (string-split intromsg "\n"))
      (for ((t (in-list txts))
            (i (in-naturals)))
        (prepend! sprites (text-sprite textr textsr t
                                       -100.0 (+ 190.0 (* i 20)) LAYER_UI)))
      ))
     (else
      ; we have ownspace      
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
      
      ; background starts as fog of war gray
      (set! background-gray 80)

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
           (prepend! sprites (rect-filled csd 0.0 0.0 size size
                                          LAYER_HANGAR_BACKGROUND
                                          #:r 0 #:g 0 #:b 0
                                          #:a 0.75))
           (prepend! sprites (rect-outline csd 0.0 0.0 size size 3.5
                                           LAYER_HANGAR_BACKGROUND))
           
           ; draw all the ships in the hangar
           (define shipmax (+ 20 (* 2.0 (apply max 1.0 (map (lambda (s) (ship-w s 1.0))
                                                            (ship-hangar ship))))))
           (define buf 8)
           (define numfit (max 1 (inexact->exact (floor (/ size (+ shipmax buf))))))
           (for ((s (in-list (ship-hangar ship)))
                 (i (in-naturals)))
             (define x (+ (* -0.5 size)
                          (* (quotient i numfit) (+ shipmax buf))
                          (/ shipmax 2)))
             (define y (+ (* -0.5 size)
                          (* (remainder i numfit) (+ shipmax buf))
                          (/ shipmax 2)))
             (define sym (string->symbol (ship-type s)))
             (prepend! sprites (sprite x y (sprite-idx csd sym)
                                       #:layer LAYER_HANGAR #:theta (- pi/2)
                                       #:m (/ (exact->inexact (ship-sprite-size s))
                                              (sprite-size csd sym))
                                       #:r (get-red ownspace s)))

             (define w (ship-w s 1.0))
             (prepend! sprites (draw-hp-bar s x y w csd LAYER_HANGAR 1.0))
             
             (define b (button 'outline (ob-id s) #f
                               x y shipmax shipmax ""
                               (lambda (x y)
                                 (send-commands (chmov meid (ob-id s) #f)))))
             (prepend! buttons b)
             (define players (find-all ownspace s player?))
             (for ((i (modulo (debug-num) 10)))
               (append! players (player -1 (~a "player" i) "fac1" -1 '() #f #f)))
             (for ((p (in-list players))
                   (i (in-naturals)))
               (prepend! sprites (text-sprite textr textsr (player-name p)
                                              (+ x (/ shipmax 2) buf)
                                              (+ y (- (/ shipmax 2)) (* i 20))
                                              LAYER_UI)))))
          ((not (ship-flying? ship))
           ; our ship is inside another
           ; draw black circle on top of topship
           (prepend! sprites (obj-sprite topship csd center (get-scale)
                                         LAYER_HANGAR_BACKGROUND 'circle
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
           (prepend! sprites (draw-hp-bar ship x y w csd LAYER_HANGAR 1.0))))

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
          (define b (button 'normal #\h #f (- (right) 206) (- (bottom) 28) 140 40
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
            #:when (and (ann? a)
                        (or showtab (not (ann-tab? a)))
                        (or (not (ann-faction a))
                            (equal? (ann-faction a) fac))))
        (define-values (x y)
          (if (equal? 'center (posvel-t (obj-posvel a)))
              (values 0.0 0.0)
              (values (left) (top))))
        (when (ann-button? a)
          (define ab (button 'normal (ob-id a) #f
                             (+ x (obj-x a)) (+ y (obj-y a))
                             (obj-dx a) (obj-dy a) (ann-txt a)
                             (lambda (k y) (send-commands (anncmd meid (ob-id a))))))
          (prepend! buttons ab))
        (when (ann-text? a)
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
                                          (+ x (obj-x a)) (+ y (obj-y a) (* i 20))
                                          LAYER_UI_TEXT z)))))

      (when showtab
        ; list all players by faction
        (define players (space-players ownspace))
        (for ((i (modulo (debug-num) 10)))
          (append! players (player -1 (~a "player" i) "team" -1 '() #f #f)))
        (define factions '())
        (define h (make-hash))
        (for ((p (in-list players)))
          (when (not (member (player-faction p) factions))
            (set! factions (cons (player-faction p) factions)))
          (hash-set! h (player-faction p)
                     (cons (player-name p) (hash-ref h (player-faction p) '()))))
        (set! factions (sort factions string<?))
        
        (define str (format "~a Players" (length players)))
        (prepend! sprites (text-sprite textr textsr str
                                      -200 (+ (top) 100) LAYER_UI_TEXT))
        (for (((fact names) (in-hash h))
              (i (in-naturals)))
          (prepend! sprites (text-sprite textr textsr fact
                                         (+ -200 (* 150 i)) (+ (top) 100 30) LAYER_UI_TEXT))
          (for ((name names)
                (k (in-naturals 1)))
            (prepend! sprites (text-sprite textr textsr name
                                           (+ -200 (* 150 i)) (+ (top) 100 40 (* k 20))
                                           LAYER_UI_TEXT)))))
      
      ; draw orders
      (define line 0)
      (when ordertree
        (define lefte (+ (left) 150))
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
                              (set-scale 1.0)
                              (set! center-follow? #t)
                              (send-commands (chmov meid (ob-id s) #f)))))
          (prepend! buttons b)))
            
      ; draw game UI
      
      ; zoom scale
      (when (not showsector?)
        (define zw 20)
        (define zh 150)
        (define zcx (- (right) 16 (/ zw 2)))
        (define zcy (+ (top) 60 (/ zh 2)))
        (prepend! sprites (sprite zcx (+ zcy (- (/ zh 2))) (sprite-idx csd '20x2)
                                  #:layer LAYER_UI
                                  #:r (send zoomcol red)
                                  #:g (send zoomcol green)
                                  #:b (send zoomcol blue)))
        (prepend! sprites (sprite zcx (+ zcy (/ zh 2)) (sprite-idx csd '20x2)
                                  #:layer LAYER_UI
                                  #:r (send zoomcol red)
                                  #:g (send zoomcol green)
                                  #:b (send zoomcol blue)))
        (prepend! sprites (sprite zcx zcy (sprite-idx csd '2x150)
                                  #:layer LAYER_UI
                                  #:r (send zoomcol red)
                                  #:g (send zoomcol green)
                                  #:b (send zoomcol blue)))
        
        (define zfrac (/ (- (log scale-play) (log (min-scale)))
                         (- (log (max-scale)) (log (min-scale)))))
        (prepend! sprites (sprite zcx (+ zcy (/ zh 2) (- (* zfrac zh)))
                                  (sprite-idx csd '20x2)
                                  #:layer LAYER_UI
                                  #:r (send zoomcol red)
                                  #:g (send zoomcol green)
                                  #:b (send zoomcol blue)))
        (define zbutton (button 'hidden 'zoom #f zcx zcy zw zh "Zoom"
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
      (define acby (+ (top) 60))
      (define acbw 180)
      (define acbh 40)
      (cond
        (showsector?
         (prepend! buttons (button 'normal 'leave-sector-map #f 0.0 acby acbw acbh
                                   "Leave Sector Map"
                                   (lambda (x y) (set! showsector? #f)))))
        ((not center-follow?)
         (prepend! buttons (button 'normal #\z #f 0.0 acby acbw acbh "Auto Center [z]"
                                   (lambda (x y) (set! center-follow? #t))))))

      (cond
        ((not my-stack)
         ; nothing to leave
         )
        ((unbox in-hangar?)
         ; the hangar button turns into an exit button
         )
        ((spacesuit? (get-ship my-stack))
         ; dying
         (define b (button 'normal 'restart #f
                           0.0 (- (bottom) 124) 100 40 "Restart"
                           (lambda (x y)
                             (set! center-follow? #t)  ; sector/ship centered
                             (send-commands (chmov meid #f #f)))))
         (prepend! buttons b))
        ((and (player-rcid (car my-stack))
              (find-id ownspace ownspace (player-rcid (car my-stack))))
         ; remote controlling something
         )
        ((ship-flying? (get-ship my-stack))
         ; jumping ship
         (define b (button 'normal 'jump #f
                           (+ (left) 58) (+ (top) 28) 100 40 "Jump"
                           (lambda (x y)
                             (send-commands (chmov meid #f #f)))))
         (prepend! buttons b))
        (else
         ; leaving this ship into mothership
         (define ms (cadr (get-ships my-stack)))
         (define b (button 'normal 'escape #f
                           0.0 (- (bottom) 76) 160 40 "Exit to Mothership"
                           (lambda (x y)
                             (send-commands (chmov meid (ob-id ms) #f)))))
         (prepend! buttons b)))
      
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
             (define t (space-time ownspace))
             (define cmd (command meid (player-cmdlevel player) 'pbolt
                                  (list a a
                                        (pbolt-frac last-pbolt-time t))))
             (define spacecmd (command meid (player-cmdlevel player) 'pbolt
                                       (list (obj-r ship) (obj-r ship)
                                             (pbolt-frac last-pbolt-time t))))
             (set! clickcmds (lambda (spacebar?)
                               (set! last-pbolt-time t)
                               (if spacebar? spacecmd cmd)))
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
        (define max 10)
        (define num 0)
        (let loop ((l (space-objects ownspace)))
          (when (and (not (null? l)) (num . < . max))
            (when (message? (car l))
              (define m (car l))
              (set! num (+ num 1))
              (define z (linear-fade (obj-age ownspace m) (/ MSG_FADE_TIME 2) MSG_FADE_TIME))
              (prepend! sprites (text-sprite textr textsr (message-msg m)
                                             (+ (left) 10.0) (+ (top) 100.0 (* num 20))
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
      ))
    ; normally quit with ctrl-q
    (define quit-button
      (button 'hidden #\q #t
              0 0 0 0 "Quit"
              (lambda (x y) (quit?))))
    (when (or (not ownspace)
              (not my-stack)
              (spacesuit? (get-ship my-stack)))
      ; if we are not in the game, show the button
      (set-button-draw! quit-button 'normal)
      (set-button-x! quit-button (- (right) 48))
      (set-button-y! quit-button (- (bottom) 28))
      (set-button-width! quit-button 80)
      (set-button-height! quit-button 40))
    (prepend! buttons quit-button)
    
    (timeit t7
    (send canvas set-cursor (make-object cursor% (if cursordrawn 'blank 'arrow)))

    ; framerate
    (when ((length frames) . > . 1)
      (define start (list-ref frames (- (length frames) 1)))
      (define end (first frames))
      (define span (/ (- end start) 1000))
      (define txt (format "FPS: ~a" (round (/ (- (length frames) 1) span))))
      (prepend! sprites (text-sprite textr textsr txt (- (right) 70) (+ (top) 30) LAYER_UI)))

    ; network issues?
    (define ma (apply max 0 aheads))
    (when (ma . > . AHEAD_THRESHOLD)
      (define txt (format "Ahead: ~a" ma))
      (prepend! sprites (text-sprite textr textsr txt (- (right) 200) (+ (top) 30) LAYER_UI)))
    
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
    (define r (render layers '() sprites
                      #:r background-gray
                      #:g background-gray
                      #:b background-gray))
    )
    (timeit t9
    (r (fl->fx canon-width) (fl->fx canon-height) dc)
    )

    (when CLIENT_OUTPUT_TIME
      (outputtime "client render"
                  (if ownspace (space-time ownspace) #f)
                  t1 t2 t3 t4 t5 t6 t7 t8 t9))
    )
    
  
  (define-values (left-inset top-inset) (get-display-left-top-inset))
  (define-values (screen-w screen-h) (get-display-size #t))

  ;(printf "insets ~a ~a size ~a ~a\n" left-inset top-inset screen-w screen-h)
  
  (set! frame
        (new (class frame%
               (super-new)
               (define (can-close?)
                 (quit?)
                 ; return #f
                 ; - if the player clicked "Quit" we'll do our own teardown
                 #f)
               (augment can-close?))
             (label "Warp")))


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
        (printf "canvas on-size ~a ~a\n" w h)
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
          (key-for
           (define val (if (equal? key-for 'name) name ip))
           (define escape? #f)
           (case kc
             ((escape)
              (set! escape? #t))
             ((#\backspace #\rubout)
              (when ((string-length val) . > . 0)
                (set! val (substring val 0 (- (string-length val) 1)))))
             (else
              (when (and (char? kc)
                         (member kc *ALL-ASCII*))
                (set! val (string-append val (string kc))))))
           (set! val (substring val 0 (min (string-length val) 30)))
           (if (equal? key-for 'name)
               (set! name val)
               (set! ip val))
           (when escape?
             (set! key-for #f)))
          (else
           (case kc
             ((wheel-up)
              (when (and ownspace (not showsector?))
                (zoom-mouse 1.05)))
             ((wheel-down)
              (when (and ownspace (not showsector?))
                (zoom-mouse (/ 1.0 1.05))))
             ((#\f)
              (when (send event get-control-down)
                (send frame fullscreen (not (send frame is-fullscreened?)))))
             ((#\space)
              (when clickcmds
                (send-commands (clickcmds #t))))
             ((#\0)
              (debug-num (+ 1 (debug-num))))
             ((#\?)
              (set! help-screen? (not help-screen?)))
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
             #;((#\u)
              (when ownspace
                (send-commands (chadd (random-upgrade (space-time ownspace)
                                                      (posvel -1 0 0 0
                                                              (random 100)
                                                              (random 100) 0)) #f))))
             ))))
      ))


  (define glconfig (new gl-config%))
  (send glconfig set-legacy? #f)
  
  (set! canvas
    (new my-canvas%
         (parent frame)
         ; start with a min-width and min-height so the frame shows it all
         ; this is reset after the frame is shown
         (min-width (fl->fx canon-width))
         (min-height (fl->fx canon-height))
         (paint-callback draw-screen)
         (gl-config glconfig)
         (style '(no-autoclear gl))))
  
  (define sd (make-sprite-db))
  (let ()
    ; used to draw lines
    ; we need multiple because:
    ; - scaling up causes fading at the edges
    ; - scaling down too much causes some kind of visual ringing
    (add-sprite!/value sd '10x10
                       (colorize (filled-rectangle 10 10) "black"))
    (add-sprite!/value sd '100x10
                       (colorize (filled-rectangle 100 10) "black"))
    (add-sprite!/value sd '1000x10
                       (colorize (filled-rectangle 1000 10) "black"))

    ; used to fill rectangles
    (add-sprite!/value sd '100x100
                       (colorize (filled-rectangle 100 100) "black"))
    (add-sprite!/value sd '1000x100
                       (colorize (filled-rectangle 1000 100) "black"))
    (add-sprite!/value sd '1000x1000
                       (colorize (filled-rectangle 1000 1000) "black"))
    
    (add-sprite!/value sd '5x1
                       (colorize (filled-rectangle 5 1) "black"))
    (add-sprite!/value sd '1x1
                       (colorize (filled-rectangle 1 1) "black"))
    (add-sprite!/value sd '20x2
                       (colorize (filled-rectangle 20 2) "black"))
    (add-sprite!/value sd '2x150
                       (colorize (filled-rectangle 2 150) "black"))
    (add-sprite!/value sd 'circle
                       (colorize (filled-ellipse 100 100) "black"))
    (add-sprite!/value sd 'shield
                       (colorize (filled-rounded-rectangle 10 100 -.5 #:draw-border? #f)
                                 "black"))
    (add-sprite!/value sd 'circle-outline
                       (colorize (inset
                                  (ellipse 100 100 #:border-color "black" #:border-width 2)
                                  2) "black"))
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

    (add-sprite!/value sd 'intro (read-bitmap (build-path IMAGEDIR "intro.png") 'png/alpha))
    )
   
  (define textfont (load-font! sd #:size TEXTH #:face "Verdana" #:family 'modern))
  (load-ships! sd)
  (plasma-setup-pre! sd)
  (explosion-setup-pre! sd)
  
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

  ; now that the frame is sized big enough to show the whole intro screen
  ; reset the min size so the user can resize the window
  (define-values (w h) (send canvas get-graphical-min-size))
  (send canvas min-width w)
  (send canvas min-height h)
   
 ) ; when gui?
  
  
  (define (tick-space-client! space)
    (set-space-time! space (+ (space-time space) TICK))
    (for ((o (in-list (space-objects space)))
          #:when (obj-alive? o))
      (update-physics! space o (/ TICK 1000.0))
      (update-stats! space o (/ TICK 1000.0))
      (add-backeffects! space o)))


  (struct server-ports (in out))
  
  ; try to connect to server
  (define (connect/async!)
    (set! connecting? #t)
    (set! intromsg "")
    (define th (current-thread))
    (define cust (make-custodian))
    (define s (make-semaphore 1))
    (parameterize ([current-custodian cust])
      (thread (lambda ()
                (define-values (in out)
                  (with-handlers ((exn:fail:network?
                                   (lambda (exn)
                                     (eprintf "~a\n" (exn-message exn))
                                     (set! intromsg (exn-message exn))
                                     (values #f #f))))
                    (tcp-connect ip port)))
                (when (semaphore-try-wait? s)
                  (cond
                    (in
                     ; success, send ports to main thread
                     (thread-send th (cons #f (server-ports in out))))
                    (else
                     ; fast network failure
                     (set! connecting? #f)))))))
    
    (thread (lambda ()
              (sleep 5)
              (when (semaphore-try-wait? s)
                (custodian-shutdown-all cust)
                (eprintf "Connection Timed Out\n")
                (set! intromsg "Connection Timed Out")
                (set! connecting? #f)))))
  
  
  (define (drop-connection!)
    (set! intromsg "Server Lost")
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
    (set! ownspace #f)
    (set! connecting? #f))

  
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

    (when (and (not gui?)
               (not server-in-port))
      ; headless client just tries to connect
      (when (not connecting?)
        (sleep 1)  ; don't try as fast as possible
        (connect/async!)))

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
           (drop-connection!))
          ((server-ports? input)
           (set! server-in-port (server-ports-in input))
           (set! server-out-port (server-ports-out input))
           (set! server-in-t (make-in-thread #f server-in-port (current-thread)))
           (set-tcp-nodelay! server-out-port #t)
           (set! server-out-t (make-out-thread #f server-out-port (current-thread)))
           ; send our name to the server
           ; send version in id space
           (send-commands (player VERSION name #f #f '() #f #f)))
          ((player? input)
           ; should only happen once when we connect to the server
           (when (not (equal? VERSION (player-name input)))
             (define msg
               (format (string-append "client version ~a not equal to server version ~a\n"
                                      "  try \"raco pkg update warp\"\n")
                       VERSION (player-name input)))
             (printf msg)
             (when gui?
               (message-box "Version Mismatch" msg frame))
             (exit 1))
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
           (set! key-for #f)

           ; set scale so we see the whole sector
           (set-scale (min-scale) #:immediate? #t)
           (set! first-scale #t))
          
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
          (set-scale 1.0))
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
    (start-client "127.0.0.1" PORT)
    ;))
  ;(exit 0)
  )
