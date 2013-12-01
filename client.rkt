#lang racket/gui

(require racket/async-channel
         racket/serialize)

(require "defs.rkt"
         "physics.rkt"
         "drawing.rkt")
(require "server.rkt")

(provide start-client)

(define CLIENT_LOOP_DELAY .03)  ; don't loop more often than X secs

(define (start-client player-arg)
  (current-eventspace (make-eventspace))
  
  (define update-channel (make-async-channel))
  
  ; connect to server
  (async-channel-put command-channel update-channel)
  
  (define server-channel (async-channel-get update-channel))
  
  (async-channel-put server-channel player-arg)
  
  (define frames '())  ; list of last few frame times
  
  (define ownspace #f)
  (define me player-arg)
  (define my-stack #f)
  
  (define (send-command cmd)
    (async-channel-put server-channel cmd))
  
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
  
  
  (define (draw-screen canvas dc)
    ; transformation is (center of screen, y up, WIDTHxHEIGHT logical units, rotation clockwise)
    (define t (send dc get-transformation))
    (send dc erase)
    
    (when (and frames ownspace my-stack)
      (draw-all canvas dc frames ownspace my-stack))
    
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
  
  (define dc (send canvas get-dc))
  (send dc set-initial-matrix #(1 0 0 1 0 0))
  (send dc set-origin (/ (send canvas get-width) 2) (/ (send canvas get-height) 2))
  (send dc set-scale (/ (send canvas get-width) WIDTH 1.0) (/ (send canvas get-height) HEIGHT -1.0))
  (send dc set-rotation 0)
  (send dc set-smoothing 'smoothed)
  (send dc set-brush "red" 'transparent)
  
  (define previous-loop-time (current-inexact-milliseconds))
  (define last-update-time (current-inexact-milliseconds))
  
  (define (client-loop)
    (define current-time (current-inexact-milliseconds))
    (define dt (/ (- current-time previous-loop-time) 1000))
    (set! previous-loop-time current-time)
    
    ; get new world
    (define update (async-channel-try-get update-channel))
    (when update
      ;(printf "got update\n")
      (set! last-update-time (current-inexact-milliseconds))
      ;(printf "update: ~v\n" update)
      (set! ownspace (deserialize update))
      ; find my role
      (set! my-stack (find-player ownspace (player-id me))))
    
    ; physics prediction
    (when ownspace
      (define dt (/ (- current-time last-update-time) 1000))
      (set! last-update-time current-time)
      
      ;(printf "client physics:\n")
      (update-physics! ownspace dt)
      (update-effects! ownspace)
      )
    
    ;rendering
    (set! frames (add-frame-time current-time frames))
    (send canvas refresh-now)
    
    ;sleep so we don't hog the whole racket vm
    (define loop-time (/ (- (current-inexact-milliseconds) current-time) 1000))
    (if (loop-time . < . CLIENT_LOOP_DELAY)
        (sleep (- CLIENT_LOOP_DELAY loop-time))
        (begin (printf "CLIENT LOOP TOO LONG: ~a\n" loop-time)))
    
    (queue-callback client-loop #f))
  
  (queue-callback client-loop #f))


