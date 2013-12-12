#lang racket/gui

(require "defs.rkt"
         "physics.rkt"
         "helm.rkt"
         "draw.rkt"
         "draw-intro.rkt")

(provide start-client)

(define CLIENT_LOOP_DELAY .03)  ; don't loop more often than X secs

(define (start-client ip port name new-eventspace?)
  (when new-eventspace?
    (current-eventspace (make-eventspace)))
  
  (define ownspace #f)
  (define my-stack #f)
  (define buttons #f)
  (define frames '())  ; list of last few frame times
  
  ; connect to server
  (define-values (server-in-port server-out-port)
    (tcp-connect ip port))
  
  (define (send-command cmd)
    ;(printf "send-command ~v\n" cmd)
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
    (define scale (min (/ (send canvas get-width) WIDTH)
                       (/ (send canvas get-height) HEIGHT)))
    (define x (/ (- (send event get-x) (/ (send canvas get-width) 2)) scale))
    (define y (/ (- (/ (send canvas get-height) 2) (send event get-y)) scale))
    (define button (click-button? buttons x y))
    (cond
      ((and button (equal? button "leave"))
       (cond
         ((crewer? role)
          (send-command (role-change me (obj-id role) #f)))
         (else
          (define crew (ship-crew (get-ship my-stack)))
          (send-command (role-change me (obj-id role) (obj-id crew))))))
      ((helm? role)
       (send-command (click-helm x y button role)))
      (button
       ;(printf "~a: clicked button ~a\n" (player-name me) button)
       (define mr (find-id ownspace button))
       (send-command (role-change me (if role (obj-id role) #f) (obj-id mr))))))
  
  
  (define (draw-screen canvas dc)
    (send dc set-smoothing 'smoothed)
    (send dc set-brush "red" 'transparent)
    (keep-transform dc
      (send dc translate (/ (send canvas get-width) 2) (/ (send canvas get-height) 2))
      (define scale (min (/ (send canvas get-width) WIDTH) (/ (send canvas get-height) HEIGHT)))
      (send dc scale scale (- scale))
      
      ; transformation is (center of screen, y up, WIDTHxHEIGHT logical units, rotation clockwise)
      (send dc erase)
      
      (define role (get-role my-stack))
      
      (set! buttons
            (cond
              ((not ownspace)
               (draw-intro dc))
              ((not my-stack)
               (draw-no-role dc ownspace))
              ((helm? role)
               (draw-helm dc role ownspace my-stack))
              (else
               (append
                (draw-playing dc ownspace my-stack)
                (draw-overlay dc ownspace my-stack)))))
      
      (draw-buttons dc buttons)
      (draw-framerate dc frames)))
  
  
  (define-values (left-inset top-inset) (get-display-left-top-inset))
  (define-values (screen-w screen-h) (get-display-size))
  
  (define frame (new (class frame% (super-new))
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
      ;           (when (helm? role)
      ;             (send-command (struct-copy helm role (aft #t)))))
      ;          ((#\w)
      ;           (define role (get-role my-stack))
      ;           (when (helm? role)
      ;             (send-command (struct-copy helm role (fore (not (helm-fore role)))))))
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
    (- (/ (- curtime start) 1000)
       (- curspace startspace)))
  
  (define (client-loop)
    (define current-time (current-inexact-milliseconds))
    
    ; get new world
    (when (byte-ready? server-in-port)
      ;(printf "client byte-ready, ")
      (define update (read server-in-port))
      ;(printf "update: ~v\n" update)
      ;(printf "update ~a\n" (space-time update))
      (when (not start-space-time)
        (set! start-space-time (space-time update))
        (set! start-time (- current-time (* CLIENT_LOOP_DELAY 1000))))
      
      (define dt (calc-dt current-time start-time (space-time update) start-space-time))
      
      (define diff (/ CLIENT_LOOP_DELAY 3.0))
      (when (dt . < . 0)  ; started too late
        (printf "started too late ~a\n" dt)
        (set! start-time (- start-time (* diff 1000))))
      (when (dt . > . (* 10 CLIENT_LOOP_DELAY))  ; started too early
        (printf "catching up too much ~a\n" dt)
        ;(set! start-time (+ start-time (* diff 1000)))
        )
      
      (set! ownspace update)
      ; find my role
      (set! my-stack (find-stack ownspace (obj-id me)))
      ;(printf "update stack ~v\n" my-stack)
      )
    
    
    ; physics prediction
    (when ownspace
      (define dt (calc-dt current-time start-time (space-time ownspace) start-space-time))
      (set! dt (max dt 0))  ; don't go backwards
      ;      (printf "client physics ~a ~a ~a\n" (space-time ownspace) dt start-time)
      (update-physics! ownspace dt)
      (set-space-time! ownspace (+ (space-time ownspace) dt))
      (update-effects! ownspace)
      )
    
    ;rendering
    (set! frames (add-frame-time current-time frames))
    (send canvas refresh-now)
    
    ;sleep so we don't hog the whole racket vm
    (define loop-time (/ (- (current-inexact-milliseconds) current-time) 1000))
    (if (loop-time . < . CLIENT_LOOP_DELAY)
        (new timer%
             (notify-callback (lambda () (queue-callback client-loop #f)))
             (interval (inexact->exact (floor (* 1000 (- CLIENT_LOOP_DELAY loop-time)))))
             (just-once? #t))
        (begin
          (printf "CLIENT LOOP TOO LONG: ~a\n" loop-time)
          (queue-callback client-loop #f)))
    )
  
  (queue-callback client-loop #f))

(module+ main
  (start-client "127.0.0.1" PORT "Dave" #f))
