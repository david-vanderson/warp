#lang racket/gui

(require "defs.rkt"
         "utils.rkt"
         "change.rkt"
         "physics.rkt"
         "draw-utils.rkt"
         "draw.rkt"
         "draw-intro.rkt"
         "pilot.rkt"
         "crewer.rkt"
         "weapons.rkt"
         "tactics.rkt"
         "effect.rkt"
         "ships.rkt")

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
  (define testing #f)
  (when testing
    (set! ownspace (space 0 5000 2000 (list)))
    (for ((i 10))
      (define s (make-ship "blue-fighter" "Rebel Fighter" "Rebel"
                           #:x (random-between -1000.0 1000.0)
                           #:y (random-between -1000.0 1000.0)))
      (define f (make-ship "blue-frigate" "Rebel Frigate" "Rebel"
                           #:x (random-between -1000.0 1000.0)
                           #:y (random-between -1000.0 1000.0)
                           #:in-hangar
                           (list (make-ship "blue-fighter" "Rebel Fighter" "Rebel")
                                 (make-ship "blue-fighter" "Rebel Fighter" "Rebel"))))
      (define a (random-between 0 2pi))
      (define p (plasma (next-id) (space-time ownspace)
                        (posvel (space-time ownspace) (random-between -1000.0 1000.0) (random-between -1000.0 1000.0) 0.0
                                (+ (* PLASMA_SPEED (cos a)))
                                (+ (* PLASMA_SPEED (sin a)))
                                0.0)
                        10.0 #f))
      (set-space-objects! ownspace (cons f (space-objects ownspace))))
    
    
    (for ((i 10))
      (define m (message (next-id) (space-time ownspace) #f (format "message ~a" i)))
      (set-space-objects! ownspace (cons m (space-objects ownspace)))))
  
  
  (define my-stack #f)
  (define buttons #f)
  (define frames '())  ; list of last few frame times
  (define last-update-time #f)
  
  
  (define (click-button? buttons x y)
    (if (not buttons)
        #f
        (ormap (lambda (b)
                 (and (<= (button-x b) x (+ (button-x b) (button-width b)))
                      (<= (button-y b) y (+ (button-y b) (button-height b)))
                      (button-name b)))
               buttons)))
  
  
  (define (click canvas event)
    (define scale (canvas-scale canvas))
    (define-values (x y) (screen->canon canvas (send event get-x) (send event get-y)))
    (define button (click-button? buttons x y))
    ;(printf "click ~a ~a ~a\n" x y button)
    (define role (if my-stack (get-role my-stack) #f))
    (cond
      ((and button (equal? button "leave"))
       (set-box! viewing-sector? #f)
       (cond
         ((not role)
          (drop-connection "clicked leave")
          (exit 0))
         ((and (crewer? role) (not (hangar? role)))
          (define ships (get-ships my-stack))
          (cond (((length ships) . > . 1)
                 (define h (car (filter hangarpod? (ship-pods (cadr ships)))))
                 (send-commands (role-change me (ob-id role) (ob-id h) -1)))
                (else
                 (define pv (obj-posvel (car ships)))
                 (define dx (random-between -50 50))
                 (define dy (random-between -50 50))
                 (define ss (make-ship "space-suit"
                                       (player-name me)
                                       (ship-faction (car ships))
                                       #:x 0
                                       #:y 0
                                       #:dx (+ (posvel-dx pv) dx)
                                       #:dy (+ (posvel-dy pv) dy)))
                 (define t (atan0 dy dx))
                 (define r (+ 1 (hit-distance (car ships) ss)))
                 (set-posvel-x! (obj-posvel ss) (+ (posvel-x pv) (* r (cos t))))
                 (set-posvel-y! (obj-posvel ss) (+ (posvel-y pv) (* r (sin t))))
                 (add-player-to-multipod! me (car (ship-pods ss)) -1)
                 (define rc (role-change me (ob-id role) #f -1))
                 (send-commands (list rc (chadd ss #f))))))
         ((spacesuit? (get-ship my-stack))
          (send-commands (role-change me (ob-id role) #f -1)))
         (else
          (define crew (ship-crew (get-ship my-stack)))
          (send-commands (role-change me (ob-id role) (ob-id crew) -1)))))
      ((and button (equal? button "sectorview"))
       (set-box! viewing-sector? (not (unbox viewing-sector?))))
      ((unbox viewing-sector?)
       (printf "click dropped because viewing sector\n"))
      ((crewer? role)
       (send-commands (click-crewer x y button my-stack)))
      ((pilot? role)
       (send-commands (click-pilot x y button my-stack)))
      ((weapons? role)
       (send-commands (click-weapons x y button my-stack)))
      ((tactics? role)
       (send-commands (click-tactics x y button my-stack)))
      (button
       ; player is choosing starting role
       (when role (error "choosing a starting role but already in pod ~v\n" my-stack))
       (send-commands (role-change me #f button -1)))
      (else
       (printf "click hit ELSE clause\n"))))
  
  
  (define (draw-screen canvas dc)
;    (when (and serverspace ownspace)
;      (printf "serverspace time ~a\n   ownspace time ~a\n" (space-time serverspace) (space-time ownspace)))
    
    (send dc set-smoothing 'smoothed)
    (send dc set-background bgcolor)
    
    ;(send dc set-text-mode 'transparent)
    ;    (send dc set-font (send the-font-list find-or-create-font
    ;                          12 'default 'normal 'normal #f 'smoothed #f 'aligned))
    
    (keep-transform dc
      
      (send dc set-clipping-rect 0 0 (send canvas get-width) (send canvas get-height))
      (send dc clear)
      
      (send dc translate (/ (send canvas get-width) 2) (/ (send canvas get-height) 2))
      (define scale (min (/ (send canvas get-width) WIDTH) (/ (send canvas get-height) HEIGHT)))
      (send dc scale scale scale)
      ; transformation is (center of screen, y down, WIDTHxHEIGHT logical units, rotation clockwise)
      
      (send dc set-clipping-rect (- (/ WIDTH 2)) (- (/ HEIGHT 2)) (* 1 WIDTH) (* 1 HEIGHT))
      
      ; reset alpha in case a damage effect changed it last frame
      (send dc set-alpha 1.0)
      
      (define role (if my-stack (get-role my-stack) #f))
      
      (when my-stack (draw-dmgfx dc my-stack))
      
      (set! buttons
            (cond
              ((not ownspace)
               (draw-intro dc))
              ((not my-stack)
               (draw-sector dc ownspace #f))
              ((pilot? role)
               (draw-pilot dc ownspace my-stack))
              ((crewer? role)
               (draw-crewer canvas dc ownspace my-stack))
              ((observer? role)
               (define bs (draw-observer dc ownspace my-stack serverspace))
               ;(draw-pilot-fitness dc ownspace (get-ship my-stack))
               bs)
              ((weapons? role)
               (draw-weapons dc my-stack serverspace))
              ((tactics? role)
               (draw-tactics dc my-stack))
              (else
               (error "didn't know what to draw"))))
      
      (draw-buttons dc buttons)
      (draw-overlay dc ownspace my-stack)
      (draw-framerate dc frames)
      )
    )
  
  
  (define-values (left-inset top-inset) (get-display-left-top-inset))
  (define-values (screen-w screen-h) (get-display-size))
  
  (define frame (new frame%
                     (label "Warp")
                     (width (inexact->exact WIDTH))
                     (height (inexact->exact HEIGHT))
                     ;                     (x (- left-inset))
                     ;                     (y (- top-inset))
                     ;                     (width screen-w)
                     ;                     (height screen-h)
                     ;                     (style '(hide-menu-bar no-caption no-resize-border))
                     ))
  
  (define my-canvas%
    (class canvas%
      (super-new)
      (define/override (on-event event)
        (when (send event button-down? 'left)
          (click this event)))
      (define/override (on-char event)
        ;(displayln (~v (send event get-key-code)))
        (case (send event get-key-code)
          ((#\h)
           (printf "hello\n"))
          ((#\d)
           (define r (get-role my-stack))
           (define p (get-pod my-stack))
           (when (pilot? r)
             (define d (dmg-for-role r))
             (send-commands (adddmg (ob-id p) d))) 
           )))
      ))
  
  (define canvas
    (new my-canvas%
         (parent frame)
         (paint-callback draw-screen)
         (style '(no-autoclear))))
  
  
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
    
    (when (and (not testing) (not server-in-port))
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
    (while (and (not testing) server-in-port (byte-ready? server-in-port))
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
      (when testing (tick-space! ownspace))
      (when (not testing)
        (set! my-stack (find-stack ownspace (ob-id me)))
      
        ; physics prediction
        (define dt (calc-dt (current-milliseconds) start-time (space-time ownspace) start-space-time))
        (when (dt . > . TICK)
          ;(printf "client ticking forward for prediction ~a\n" dt)
          (tick-space! ownspace)
          (set! dt (calc-dt (current-milliseconds) start-time (space-time ownspace) start-space-time))))
      
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
       (if (and (not testing) ownspace)
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
