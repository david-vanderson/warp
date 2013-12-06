#lang racket/gui

(require "defs.rkt"
         "physics.rkt"
         "draw.rkt"
         "draw-intro.rkt")

(provide start-client)

(define CLIENT_LOOP_DELAY .03)  ; don't loop more often than X secs

(define (start-client ip port player-arg new-eventspace?)
  (when new-eventspace?
    (current-eventspace (make-eventspace)))
  
  ; connect to server
  (define-values (server-in-port server-out-port)
    (tcp-connect ip port))
  
  (define (send-command cmd)
    ;(printf "send-command\n")
    (write cmd server-out-port)
    (flush-output server-out-port))
  
  (send-command player-arg)
  
  (define frames '())  ; list of last few frame times
  
  (define ownspace #f)
  (define me player-arg)
  (define my-stack #f)
  
  (define (interpret-click canvas event)
    (when my-stack
      (define role (get-role my-stack))
      (cond ((helm? role)
             (when (send event button-down? 'left)
               (printf "~a: helm clicked\n" (player-name me))
               (define x (- (send event get-x) (/ (send canvas get-width) 2)))
               (define y (- (/ (send canvas get-height) 2) (send event get-y)))
               (define course (atan y x))
               (when (course . < . 0)
                 (set! course (+ course (* 2 pi))))
               (send-command (struct-copy helm role (course course))))))))
  
  (define first-frame #t)
  
  (define (draw-screen canvas dc)
    (when first-frame
      (set! first-frame #f)
      (send dc set-initial-matrix #(1 0 0 1 0 0))
      (send dc set-origin (/ (send canvas get-width) 2) (/ (send canvas get-height) 2))
      (send dc set-scale (/ (send canvas get-width) WIDTH 1.0) (/ (send canvas get-height) HEIGHT -1.0))
      (send dc set-rotation 0)
      (send dc set-smoothing 'smoothed)
      (send dc set-brush "red" 'transparent))
    
    ; transformation is (center of screen, y up, WIDTHxHEIGHT logical units, rotation clockwise)
    (define t (send dc get-transformation))
    (send dc erase)
    
    (cond
      ((not ownspace)
       (draw-intro dc))
      ((not my-stack)
       (draw-sector dc ownspace))
      (else
       (draw-playing dc ownspace my-stack)))
    
    (draw-framerate dc frames)
    
    (send dc set-transformation t))
  
  (define frame (new (class frame% (super-new))
                     (label "Warp")))
  
  (define my-canvas
    (class canvas%
      (super-new)
      (define/override (on-event event)
        (interpret-click this event))
      (define/override (on-char event)
        ;(displayln (~v (send event get-key-code)))
        (case (send event get-key-code)
          ((#\f)
           (define role (get-role my-stack))
           (when (helm? role)
             (send-command (struct-copy helm role (aft #t)))))
          ((#\w)
           (define role (get-role my-stack))
           (when (helm? role)
             (send-command (struct-copy helm role (fore (not (helm-fore role)))))))
          ))
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
      (set! my-stack (find-player ownspace (obj-id me))))
    
    
    ; physics prediction
    (when ownspace
      (define dt (calc-dt current-time start-time (space-time ownspace) start-space-time))
      (set! dt (max dt 0))  ; don't go backwards
      ;      (printf "client physics ~a ~a ~a\n" (space-time ownspace) dt start-time)
      (update-physics! ownspace dt)
      (update-effects! ownspace)
      (set-space-time! ownspace (+ (space-time ownspace) dt))
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
  (start-client "127.0.0.1" PORT (player 1 #f "Dave" #f) #f))
