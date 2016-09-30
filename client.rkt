#lang racket/gui

(require "defs.rkt"
         "utils.rkt"
         "change.rkt"
         "physics.rkt"
         "draw-utils.rkt"
         "draw.rkt"
         "draw-intro.rkt"
         "pilot.rkt"
         "weapons.rkt"
         "pbolt.rkt"
         "effect.rkt"
         "ships.rkt"
         "dmg.rkt"
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
  (define me #f)  ; player? or #f
  (define ownspace #f)
  (define my-stack #f)
  (define buttons #f)
  (define frames '())  ; list of last few frame times
  (define last-update-time #f)

  (define center #f)  ; updated each frame for the click handler
  (define center-follow? #t)  ; show player position in the center?

  ; when (not center-follow?)
  (define centerxy (obj #f #f (posvel #f 0 0 #f #f #f #f)))  ; center of the screen
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
          (else #t)))
  (define (min-scale)
    (if ownspace
        (min (/ WIDTH (space-width ownspace)) (/ HEIGHT (space-height ownspace)))
        .01))
  (define (max-scale) 30.0)
    
  (define (get-future-scale)
    (if (scale-ship?) scale-ship scale-play))
  (define (set-scale z)
    (define newz (max (min-scale) (min (max-scale) z)))
    (if (scale-ship?) (set! scale-ship newz) (set! scale-play newz)))
  
  (define shown-scale (get-future-scale))  ; scale that is actually used
  (define (get-scale) shown-scale)
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
         ((button-f b) (- x (button-x b)) (- y (button-y b)))))
      (my-stack
       (define mypos (get-center my-stack))
       (define-values (cx cy) (space->canon center (get-scale) (obj-x mypos) (obj-y mypos)))
       (define p (get-pod my-stack))
       (for ((t (in-list (pod-tools p))))
         (cond ((and (steer? t)
                     (ship-flying? (get-ship my-stack))
                     (not (findf (lambda (d) (equal? "offline" (dmg-type d))) (tool-dmgs t))))
                (send-commands (command (ob-id t) (angle-norm (atan0 (- y cy) (- x cx)))))))))))
  
  
  (define (draw-screen canvas dc)
;    (when (and serverspace ownspace)
;      (printf "serverspace time ~a\n   ownspace time ~a\n" (space-time serverspace) (space-time ownspace)))
    
    (send dc set-smoothing 'smoothed)
    (send dc set-background bgcolor)
    
    ;(send dc set-text-mode 'transparent)
    ;    (send dc set-font (send the-font-list find-or-create-font
    ;                          12 'default 'normal 'normal #f 'smoothed #f 'aligned))
    
    (keep-transform dc
      ; make sure whole screen is fog of war gray
      (send dc set-clipping-region #f)
      (send dc set-background (linear-color "black" "white" 0.1 1.0))
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
      
      (when my-stack (draw-dmgfx dc my-stack))

      (update-scale)
      
      (cond
        ((not ownspace)
         (draw-intro dc))
        ((not my-stack)
         (keep-transform dc
           (define scale (min (/ WIDTH (space-width ownspace)) (/ HEIGHT (space-height ownspace))))
           (send dc scale scale scale)
           (draw-sector-lines dc ownspace)
           (draw-objects dc ownspace))
         
         (define leave-button (button 'normal 'escape LEFT (- TOP 50) 50 50 "Exit"
                                      (lambda (x y)
                                        (drop-connection "clicked exit")
                                        (exit 0))))
         (set! buttons (cons leave-button buttons))
         
         (define start-stacks
           (search ownspace (lambda (o) (and (ship? o) (ship-flying? o) (ship-start o))) #t))
        
         (for ((s (in-list start-stacks))
               (i (in-naturals)))
           (define mp (car s)) 
           (define b (button 'normal #f (+ LEFT 100 (* i 250)) (+ BOTTOM 60) 200 30
                             (format "Crew on ~a" (ship-name (get-ship s)))
                             (lambda (x y)
                               (send-commands (chrole me (ob-id (ship-lounge (get-ship s))))))))
           (set! buttons (append buttons ( list b))))
         
         (draw-buttons dc buttons (space-time ownspace)))
        
        (my-stack
         (define oldclip (send dc get-clipping-region))
         (define mypos (get-center my-stack))
         (set! center (if center-follow?
                          mypos  ; includes pod
                          centerxy))
         (define z (get-scale))
         
         (keep-transform dc
           (send dc scale z z)  
           (send dc translate (- (obj-x center)) (- (obj-y center)))
           (when center-follow?
             ; record the center so we start from there if center-follow? becomes #f
             (set-posvel-x! (obj-posvel centerxy) (obj-x center))
             (set-posvel-y! (obj-posvel centerxy) (obj-y center)))

           (define shipcenter (get-topship my-stack))  ; only the ship

           ; make fog of war region
           (define fow (new region% (dc dc)))
           (define r (ship-radar shipcenter))
           (send fow set-ellipse (- (obj-x shipcenter) r) (- (obj-y shipcenter) r) (* 2 r) (* 2 r))
           (for ((s (in-list (space-objects ownspace)))
                 #:when (and (ship? s) (equal? (ship-faction s) (ship-faction (get-ship my-stack)))))
             (define rad (ship-radar s))
             (define reg (new region% (dc dc)))
             (send reg set-ellipse (- (obj-x s) rad) (- (obj-y s) rad) (* 2 rad) (* 2 rad))
             (send fow union reg))

           (send dc set-clipping-region fow)
           (send dc set-background "black")
           (send dc clear)

           ; sector lines draw regardless of fow
           (send dc set-clipping-region #f)
           (draw-sector-lines dc ownspace)
           
           (send dc set-clipping-region fow)
           (draw-background-stars dc center z)
           (draw-objects dc ownspace)

           (keep-transform dc
             (center-on dc shipcenter #f)

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
                  (draw-ship-up dc s))))

             (define ship (get-ship my-stack))
             (define rot (if (ship-flying? ship) (obj-r ship) pi/2))
             (send dc rotate (- rot))  ; rotate because we drew the ship pointing up
             (when (and (not (spacesuit? ship))
                        (not (hangar? (get-pod my-stack))))
               (define bs
                 (draw-pods dc ship rot my-stack send-commands canvas me))
               (set! buttons (append buttons bs)))
             )
           
           ; transform is back to scaled space
           (for ((t (in-list (pod-tools (get-pod my-stack)))))
             (draw-tool-overlay dc t my-stack))
           )

         ; now we are back to the canon transform
         (send dc set-clipping-region #f)

         (when (hangar? (get-pod my-stack))
           ; draw hangar background
           (send dc set-pen fgcolor 1.0 'solid)
           (send dc set-brush (make-color 0 0 0 .8) 'solid)
           (define size (* 0.9 (min WIDTH HEIGHT)))
           (send dc draw-rectangle (* -0.5 size) (* -0.5 size) size size)
           
           ; draw all the ships in the hangar
           (define shipmax 54)
           (for ((s (in-list (hangar-ships (get-pod my-stack))))
                 (i (in-naturals)))
             (keep-transform dc
               (send dc translate
                     (+ (* -0.5 size) 10 (/ shipmax 2))
                     (- (* 0.5 size) 10 (* i 100) (/ shipmax 2)))
               (draw-ship-up dc s)
               (send dc translate (+ 10 (/ shipmax 2)) (/ shipmax 2))
               (draw-text dc (format "~a" (ship-name s)) 0 -5)
               (define-values (x y) (dc->canon canvas dc 0 -60))
               (define bl (lambda (x y)
                            (send-commands (chrole me (ob-id (ship-lounge s))))))
               (set! buttons (append buttons (list (button 'normal #f x y 65 30 "Board" bl))))
               (for ((p (in-list (find-all s player?)))
                     (i (in-naturals)))
                 (draw-text dc (player-name p) 0 (- -70 (* i 20))))
               )))
            
         
         ; draw game UI

         ; zoom scale
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
         
                                 
         (set! buttons (append buttons (list zbutton zkeyb xkeyb)))
         
         
         ; draw pod UI
         (draw-pod-ui dc my-stack)
         
         ; draw tool UI
         (for ((t (in-list (pod-tools (get-pod my-stack)))))
           (define bs (draw-tool-ui dc t my-stack send-commands))
           (set! buttons (append buttons bs)))
                  
         (define leave-button (button 'normal 'escape LEFT (- TOP 50) 50 50
                                      (if center-follow? "Exit" "Back")
           (if (not center-follow?)
               (lambda (x y)
                 (set! center-follow? #t))
               (lambda (x y)
                 (define p (get-pod my-stack))
                 (define newid
                   (cond
                     ((and (lounge? p) (spacesuit? (get-ship my-stack)))
                      ; dying
                      #f
                      )
                     ((and (lounge? p) (ship-flying? (get-ship my-stack)))
                      ; jumping ship
                      "spacesuit"
                      )
                     ((lounge? p)
                      ; leaving this ship into mothership hangar
                      (define ms (cadr (get-ships my-stack)))
                      (ob-id (ship-hangar ms)))
                     (else
                      ; move to lounge
                      (ob-id (ship-lounge (get-ship my-stack))))))
                 (send-commands (chrole me newid))))))
         (set! buttons (append buttons (list leave-button)))
         (draw-buttons dc buttons (space-time ownspace))

         ; draw mouse cursor
         (define-values (p mods) (get-current-mouse-state))
         (define-values (wx wy) (send canvas screen->client
                                      (+ (send p get-x) left-inset)
                                      (+ (send p get-y) top-inset)))
         (define-values (mx my) (screen->canon canvas wx wy))
         (define drawn #f)
         (when (not (in-button? buttons mx my))
           (for ((t (in-list (pod-tools (get-pod my-stack)))))
             (when (and (steer? t) (not (findf (lambda (d) (equal? "offline" (dmg-type d))) (tool-dmgs t))))
               (set! drawn #t)
               (keep-transform dc
                 (send dc set-pen "blue" (/ 1.5 (dc-point-size dc)) 'solid)
                 (send dc translate mx my)
                 (define dx (- (+ (obj-x center) (/ mx z)) (obj-x mypos)))
                 (define dy (- (+ (obj-y center) (/ my z)) (obj-y mypos)))
                 (send dc rotate (- (atan0 dy dx)))
                 (send dc draw-lines '((0 . 0) (-15 . -5) (-15 . 5) (0 . 0)))))))

         (cond (drawn (send canvas set-cursor (make-object cursor% 'blank)))
               (else (send canvas set-cursor (make-object cursor% 'arrow))))
         
         )
        (else
         (error "didn't know what to draw")))
      
      ;(draw-buttons dc buttons)
      (draw-overlay dc ownspace my-stack)
      (draw-framerate dc frames)
      )
    )
    
  
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
          ((right-down)
           (set! dragstate "start")
           (set! dragxypx (cons (send event get-x) (send event get-y))))
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
        ;(displayln (~v kc))
        (define b (key-button? buttons kc))
        (cond
          (b
           (when (not (member (button-draw b) '(disabled dmg)))
             ((button-f b) kc #f)))
          (else
           (case kc
             ((#\h)
              (printf "hello\n"))
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
                (send-commands (message -1 (space-time ownspace) #f
                                        (~a "message " (space-time ownspace))))))
             ((#\p)
              (when ownspace
                (send-commands (chadd (plasma -1 (space-time ownspace) (posvel -1 0 0 (random-between 0 2pi) (random 100) (random 100) 0) (random 100) #f) #f))))
             ((#\s)
              (when ownspace
                (define r (random-between 0 2pi))
                (define s (random 100))
                (send-commands (chadd (shield -1 (space-time ownspace) (posvel -1 0 0 r (* s (cos r)) (* s (sin r)) 0) (random 30)) #f)))) 
             ((#\j)
              (set-scale (* (get-scale) 1.1)))
             ((#\k)
              (set-scale (/ (get-scale) 1.1)))
             ((#\n)
              (new-stars))
             ))))
      ))
  
  (define canvas
    (new my-canvas%
         (parent frame)
         (paint-callback draw-screen)
         (style '(no-autoclear))))

  (load-ships)
  (load-plasma)
  
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
    (set! me #f)
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
        "Testing" #;(get-text-from-user "Player Name"
                            "Player Name"
                            #f
                            (or name "")))
      
      (when (not newname) (exit 0))
      (when newname (set! name newname))
      
      ; ask the user for address
      (define newip
        "127.0.0.1" #;(get-text-from-user "IP of server"
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
                             (values #f #f))))
            (printf "trying to connect to ~a:~a\n" ip port)
            (tcp-connect ip port)))
        
        (set! server-in-port in)
        (set! server-out-port out)
        
        (when server-out-port
          ; send our name to the server
          (send-commands (player #f name)))
        
        (when server-in-port
          ; read a player struct that has our unique id
          (define newme (read-from-server))
          (when (not (eof-object? newme))
            (set! me newme)
            (set-player-name! me name)))))
    
    ; get new world
    (while (and server-in-port (byte-ready? server-in-port))
      (define input (read-from-server))
      ;(printf "client input: ~v\n" input)
      (cond ((space? input)
             (set! ownspace input)
             (set! start-space-time (space-time ownspace))
             (set! start-time (current-milliseconds))
             (set! last-update-time start-space-time))
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
      (set! my-stack (find-stack ownspace (ob-id me)))
      
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
