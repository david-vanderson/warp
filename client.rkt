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
         "effect.rkt")

(provide start-client)

(define serverspace #f)

(define (start-client ip port name new-eventspace? space)
  (when space
    (set! serverspace space))
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
       (cond
         ((not role)
          (drop-connection "clicked leave"))
         ((and (crewer? role) (not (hangar? role)))
          (define ships (get-ships my-stack))
          (cond (((length ships) . > . 1)
                 (define h (car (filter hangarpod? (ship-pods (cadr ships)))))
                 (send-command (role-change me (ob-id role) (ob-id h))))
                (else
                 (send-command (role-change me (ob-id role) #f)))))
         ((equal? "space-suit" (ship-type (get-ship my-stack)))
          (send-command (role-change me (ob-id role) #f)))
         (else
          (define crew (ship-crew (get-ship my-stack)))
          (send-command (role-change me (ob-id role) (ob-id crew))))))
      ((crewer? role)
       (send-command (click-crewer x y button my-stack)))
      ((pilot? role)
       (send-command (click-pilot x y button my-stack)))
      ((weapons? role)
       (send-command (click-weapons x y button my-stack)))
      ((tactics? role)
       (send-command (click-tactics x y button my-stack)))
      (button
       ; player is choosing starting role
       (when role (error "choosing a starting role but already in pod ~v\n" my-stack))
       (send-command (role-change me #f button)))
      (else
       (printf "click hit ELSE clause\n"))))
  
  
  (define (draw-screen canvas dc)
;    (when (and serverspace ownspace)
;      (printf "serverspace time ~a\n   ownspace time ~a\n" (space-time serverspace) (space-time ownspace)))
    (send dc set-smoothing 'smoothed)
    (send dc set-background bgcolor)
    (send dc set-text-foreground fgcolor)
    ;    (send dc set-font (send the-font-list find-or-create-font
    ;                          12 'default 'normal 'normal #f 'smoothed #f 'aligned))
    (keep-transform dc
      (send dc translate (/ (send canvas get-width) 2) (/ (send canvas get-height) 2))
      (define scale (min (/ (send canvas get-width) WIDTH) (/ (send canvas get-height) HEIGHT)))
      (send dc scale scale (- scale))
      (send dc set-clipping-rect (- (/ WIDTH 2)) (- (/ HEIGHT 2)) (* 1 WIDTH) (* 1 HEIGHT))
      
      ; transformation is (center of screen, y up, WIDTHxHEIGHT logical units, rotation clockwise)
      
      (send dc clear)
      
      (define role (if my-stack (get-role my-stack) #f))
      
      (set! buttons
            (cond
              ((not ownspace)
               (draw-intro dc))
              ((not my-stack)
               (draw-no-role dc ownspace))
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
      (draw-framerate dc frames)))
  
  
  (define-values (left-inset top-inset) (get-display-left-top-inset))
  (define-values (screen-w screen-h) (get-display-size))
  
  (define frame (new frame%
                     (label "Warp")
                     (width WIDTH)
                     (height HEIGHT)
                     ;                     (x (- left-inset))
                     ;                     (y (- top-inset))
                     ;                     (width screen-w)
                     ;                     (height screen-h)
                     ;                     (style '(hide-menu-bar no-caption no-resize-border))
                     ))
  
  (define my-canvas
    (class canvas%
      (define/override (on-event event)
        (when (send event button-down? 'left)
          (click this event)))
      ;      (define/override (on-char event)
      ;        ;(displayln (~v (send event get-key-code)))
      ;        (case (send event get-key-code)
      ;          ((#\f)
      ;           (define role (get-role my-stack))
      ;           (when (pilot? role)
      ;             (send-command (struct-copy pilot role (aft #t)))))
      ;          ((#\w)
      ;           (define role (get-role my-stack))
      ;           (when (pilot? role)
      ;             (send-command (struct-copy pilot role (fore (not (pilot-fore role)))))))
      ;          ))
      (super-new)))
  
  (define canvas
    (new my-canvas
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
    (for ((o (space-objects space)))
      (update-physics! space o (/ TICK 1000.0))
      (when (ship? o) (update-energy! (/ TICK 1000.0) o 0.0))
      (add-backeffects! space o TICK)))
  
  
  
  (define (drop-connection msg)
    (printf "drop server ~a\n" msg)
    (when server-in-port
      (close-input-port server-in-port)
      (close-output-port server-out-port))
    (set! server-in-port #f)
    (set! server-out-port #f)
    (set! me #f)
    (set! ownspace #f))
  
  
  (define (send-command cmd)
    ;(printf "send-command ~v\n" cmd)
    (when cmd
      (with-handlers ((exn:fail:network? (lambda (exn)
                                           (drop-connection "send-command"))))
        (write cmd server-out-port)
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
        (get-text-from-user "Player Name"
                            "Player Name"
                            #f
                            (or name "")))
      
      (when newname (set! name newname))
      
      ; ask the user for address
      (define newip
        (get-text-from-user "IP of server"
                            "IP address of the Server"
                            #f
                            (or ip "")))
      
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
             (for ((c (update-changes input)))
               ;(printf "client applying change ~v\n" c)
               (apply-change! ownspace c (update-time input) "client"))
             (for ((pvu (update-pvs input)))
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
    
    ;(printf "client got updates ~a\n" n)
    
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
    ;(printf "client render\n")
    (set! frames (add-frame-time (current-milliseconds) frames))
    (send canvas refresh-now)
          
    ; sleep so we don't hog the whole racket vm
    (define sleep-time
      (add1
       (if ownspace
           (- (calc-dt (current-milliseconds) start-time
                       (+ (space-time ownspace) TICK) start-space-time))
           (- (+ start-loop-time TICK) (current-milliseconds)))))
    
    (define sleep-secs (/ sleep-time 1000.0))
    (cond
      ((sleep-secs . > . 0)
       ;(printf "client sleeping ~a\n" sleep-time)
       (sleep/yield sleep-secs))
      (else
       ;(printf "client skipping sleep ~a\n" sleep-time)
       (sleep/yield .001)))
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
