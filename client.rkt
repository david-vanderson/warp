#lang racket/gui

(require "defs.rkt"
         "physics.rkt"
         "draw-utils.rkt"
         "draw.rkt"
         "draw-intro.rkt"
         "pilot.rkt"
         "crewer.rkt"
         "weapons.rkt"
         "tactics.rkt")

(provide start-client)

(define (start-client ip port name new-eventspace?)
  (when new-eventspace?
    (current-eventspace (make-eventspace)))
  
  (define ownspace #f)
  (define serverspace #f)
  (define my-stack #f)
  (define buttons #f)
  (define frames '())  ; list of last few frame times
  
  ; connect to server
  (define-values (server-in-port server-out-port)
    (tcp-connect ip port))
  
  (define (send-command cmd)
    (printf "send-command ~v\n" cmd)
    (when cmd
      (write cmd server-out-port)
      (flush-output server-out-port)))
  
  
  ; read a player struct that has our unique id
  (define me (read server-in-port))
  (set-player-name! me name)
  
  
  
  (define (click-button? buttons x y)
    (if (not buttons)
        #f
        (ormap (lambda (b)
                 (and (<= (button-x b) x (+ (button-x b) (button-width b)))
                      (<= (button-y b) y (+ (button-y b) (button-height b)))
                      (button-name b)))
               buttons)))
  
  
  (define (click canvas event)
    (define role (get-role my-stack))
    (define scale (canvas-scale canvas))
    (define-values (x y) (screen->canon canvas (send event get-x) (send event get-y)))
    (define button (click-button? buttons x y))
    ;(printf "click ~a ~a ~a\n" x y button)
    (cond
      ((and button (equal? button "leave"))
       (cond
         ((crewer? role)
          (send-command (role-change me (obj-id role) #f)))
         (else
          (define crew (ship-crew (get-ship my-stack)))
          (send-command (role-change me (obj-id role) (obj-id crew))))))
      ((crewer? role)
       (send-command (click-crewer x y button role ownspace me)))
      ((pilot? role)
       (send-command (click-pilot x y button my-stack)))
      ((weapons? role)
       (send-command (click-weapons x y button my-stack)))
      ((tactics? role)
       (send-command (click-tactics x y button my-stack)))
      (button
       ; player is choosing starting role
       (define mr (find-id ownspace button))
       (send-command (role-change me (if role (obj-id role) #f) (obj-id mr))))
      (else
       (printf "click hit ELSE clause\n"))))
  
  
  (define (draw-screen canvas dc)
    (send dc set-smoothing 'smoothed)
    (send dc set-background bgcolor)
    (send dc set-text-foreground fgcolor)
    ;    (send dc set-font (send the-font-list find-or-create-font
    ;                          12 'default 'normal 'normal #f 'smoothed #f 'aligned))
    (keep-transform dc
      (send dc translate (/ (send canvas get-width) 2) (/ (send canvas get-height) 2))
      (define scale (min (/ (send canvas get-width) WIDTH) (/ (send canvas get-height) HEIGHT)))
      (send dc scale scale (- scale))
      
      ; transformation is (center of screen, y up, WIDTHxHEIGHT logical units, rotation clockwise)
      
      (send dc clear)
      
      (define role (get-role my-stack))
      
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
               (draw-observer dc ownspace my-stack))
              ((weapons? role)
               (draw-weapons dc my-stack))
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
                     ;                     (x (- left-inset))
                     ;                     (y (- top-inset))
                     ;                     (width screen-w)
                     ;                     (height screen-h)
                     ;                     (style '(hide-menu-bar no-caption no-resize-border))
                     ))
  
  (define my-canvas
    (class canvas%
      (super-new)
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
      ))
  
  (define canvas
    (new my-canvas
         (parent frame)
         (min-width WIDTH)
         (min-height HEIGHT)
         (paint-callback draw-screen)
         (style '(no-autoclear))))
  
  
  (send frame show #t)
  
  (define start-space-time #f)
  (define start-time #f)
  
  (define (calc-dt curtime start curspace startspace)
    (- (- curtime start) (- curspace startspace)))
  
  (define (client-loop)
    (define current-time (current-milliseconds))
;    (printf "client current-time ~a\n" current-time)
    
    ; get new world
    (when (byte-ready? server-in-port)
      ;(printf "client byte-ready, ")
      (define update (read server-in-port))
      ;(printf "update: ~v\n" update)
      ;(printf "update ~a\n" (space-time update))
      (when (not start-space-time)
        (set! start-space-time (space-time update))
        (set! start-time current-time))
      
      ; If the first ownspace is delayed, then our own clock got started late
      (define dt (calc-dt current-time start-time (space-time update) start-space-time))
      (when (dt . < . 0)
        (printf "started too late ~a\n" dt)
        (set! start-time (- start-time 10)))
      
      (set! ownspace update)
      
      (set! serverspace (read (open-input-string (with-output-to-string (lambda () (write update))))))
      
      (set! my-stack (find-stack ownspace (obj-id me))))
    
    
    ; physics prediction
    (when ownspace
      (let loop ()
        (when (TICK . < . (calc-dt current-time start-time (space-time ownspace) start-space-time))
          (update-physics! ownspace (/ TICK 1000.0))
          (set-space-time! ownspace (+ (space-time ownspace) TICK))
          (update-effects! ownspace)
          (loop))))
    
    ;rendering
    (set! frames (add-frame-time current-time frames))
    (send canvas refresh-now)
          
    ; sleep so we don't hog the whole racket vm
    (define sleep-time (- (calc-dt
                           (current-milliseconds) start-time
                           (+ (space-time ownspace) TICK) start-space-time)))
    (if (sleep-time . > . 0)
        (new timer%
             (notify-callback (lambda () (queue-callback client-loop #f)))
             (interval (+ 1 (inexact->exact (floor sleep-time))))
             (just-once? #t))
        (begin
          (printf "client skipping sleep ~a\n" sleep-time)
          (queue-callback client-loop #f))))
  
  (queue-callback client-loop #f))

(module+ main
  (start-client "127.0.0.1" PORT "Dave" #f))
