#lang racket/gui

(require racket/async-channel
         racket/serialize)

(require "defs.rkt"
         "physics.rkt"
         "drawing.rkt")
(require "server.rkt")

(provide start-client)

(define update-channel (make-async-channel))

(define CLIENT_LOOP_DELAY .03)  ; don't loop more often than X secs


(define last-drawn-role "helm")
(define ownspace #f)
(define me (player "Dave" 123))
(define my-role #f)


(define (send-command cmd)
  (async-channel-put command-channel cmd))

(define (interpret-click canvas event role)
  (cond ((helm? role)
         (when (send event button-down? 'left)
           (define x (- (send event get-x) (/ (send canvas get-width) 2)))
           (define y (- (/ (send canvas get-height) 2) (send event get-y)))
           (define course (atan y x))
           (when (course . < . 0)
             (set! course (+ course (* 2 pi))))
           (send-command (struct-copy helm my-role (course course)))))))


(define frame (new (class frame% (super-new))
                   (label "Warp")))

(define my-canvas
  (class canvas%
    (super-new)
    (define/override (on-event event)
      (interpret-click this event my-role))
        (define/override (on-char event)
          ;(displayln (~v (send event get-key-code)))
          (case (send event get-key-code)
            ((#\f)
             (define bolt (plasma 200 0 0 -50 0 0 "blue" 10.0 '()))
             (send-command bolt))
;            ((#\a) (set-object-dr! ownship (+ (object-dr ownship) .03)))
;            ((#\d) (set-object-dr! ownship (- (object-dr ownship) .03)))
;            ((#\w)
;             (set-object-dx! ownship (+ (object-dx ownship) (* 2 (cos (object-r ownship)))))
;             (set-object-dy! ownship (+ (object-dy ownship) (* 2 (sin (object-r ownship))))))
;            ((#\s)
;             (set-object-dx! ownship (- (object-dx ownship) (* 2 (cos (object-r ownship)))))
;             (set-object-dy! ownship (- (object-dy ownship) (* 2 (sin (object-r ownship))))))
            ))
    ))


(define (draw-screen canvas dc)
  ; transformation is (center of screen, y up, WIDTHxHEIGHT logical units, rotation clockwise)
  (define t (send dc get-transformation))
  (send dc erase)
  
  (when ownspace
    (define ownship (car (space-objects ownspace)))
    (draw-all canvas dc ownspace ownship))
  
  (draw-framerate dc)
  (send dc set-transformation t))


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
    (set! ownspace (deserialize update))
    ; find my role
    (set! my-role (ship-helm (car (space-objects ownspace)))))
  
  ; physics prediction
  (when ownspace
    (define dt (/ (- current-time last-update-time) 1000))
    (set! last-update-time current-time)
    
    ;(printf "client physics:\n")
;    (update-physics! ownspace dt)
;    (update-effects! ownspace)
    )
  
  ;rendering
  (add-frame-time current-time)
  (send canvas refresh-now)
  
  ;sleep so we don't hog the whole racket vm
  (define loop-time (/ (- (current-inexact-milliseconds) current-time) 1000))
  (if (loop-time . < . CLIENT_LOOP_DELAY)
    (sleep (- CLIENT_LOOP_DELAY loop-time))
    (begin (printf "CLIENT LOOP TOO LONG: ~a\n" loop-time)))
  
  (queue-callback client-loop #f))

(define (start-client)
  (async-channel-put command-channel update-channel)
  (queue-callback client-loop #f))


