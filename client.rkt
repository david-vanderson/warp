#lang racket/gui

(require racket/async-channel
         racket/serialize)

(require "defs.rkt")
(require "server.rkt")

(define CLIENT_LOOP_DELAY .01)  ; don't loop more often than X secs
(define show-framerate? #t)
(define frames '())  ; list of last few frame times
(define last-drawn-role "helm")
(define ownship #f)

(define me (player "Dave" 123))
(define my-role #f)

(define (recenter ownship x y)
  (if ownship
      (values (- x (object-x ownship)) (- y (object-y ownship)))
      (values x y)))

(define (send-command role)
  (async-channel-put command-channel role))

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
    ;    (define/override (on-char event)
    ;      ;(displayln (~v (send event get-key-code)))
    ;      (case (send event get-key-code)
    ;        ((#\a) (set-object-dr! ownship (+ (object-dr ownship) .03)))
    ;        ((#\d) (set-object-dr! ownship (- (object-dr ownship) .03)))
    ;        ((#\w)
    ;         (set-object-dx! ownship (+ (object-dx ownship) (* 2 (cos (object-r ownship)))))
    ;         (set-object-dy! ownship (+ (object-dy ownship) (* 2 (sin (object-r ownship))))))
    ;        ((#\s)
    ;         (set-object-dx! ownship (- (object-dx ownship) (* 2 (cos (object-r ownship)))))
    ;         (set-object-dy! ownship (- (object-dy ownship) (* 2 (sin (object-r ownship))))))))
    ))


(define (draw-background dc)
  (define t (send dc get-transformation))
  (define-values (x y) (recenter ownship 0 0))
  (send dc draw-rectangle (- x 250) (- y 250) 500 500)
  (send dc set-transformation t))

(define (draw-shield dc shield)
  (define t (send dc get-transformation))
  
  (define num (length (shield-sections shield)))
  (define arc-size (* 0.9 (/ (* 2 pi) num)))
  (define radius (shield-radius shield))
  
  (for ((section (shield-sections shield))
        (i (in-naturals)))
    (define r (/ (* 2 pi i) num))
    (send dc set-pen (shield-color shield) (* 3 (/ section (shield-max shield))) 'solid)
    (send dc draw-arc
          (- (/ radius 2)) (- (/ radius 2))
          radius radius
          (- r (* 0.5 arc-size)) (+ r (* 0.5 arc-size))))
  
  (send dc set-transformation t))

(define (draw-ship dc s)
  (define t (send dc get-transformation))
  (send dc rotate (- (object-r s)))
  (for ((shield (ship-shields s)))
    (draw-shield dc shield))
  
  (send dc set-pen "black" 1 'solid)
  (send dc draw-polygon '((10 . 10)
                          (20 . 0)
                          (10 . -10)
                          (-10 . -10)
                          (-10 . 10)))
  (send dc set-transformation t))


(define (draw-framerate dc)
  (when (and show-framerate? (not (empty? frames)))
    (define t (send dc get-transformation))
    (send dc translate (- (/ WIDTH 2)) (/ HEIGHT 2))
    (send dc scale 1 -1)
    (define start (list-ref frames (- (length frames) 1)))
    (define end (first frames))
    (define span (/ (- end start) 1000))
    (send dc draw-text (~a (truncate (/ (- (length frames) 1) span))) 0 0)
    (send dc set-transformation t)))


(define (draw-screen canvas dc)
  ; transformation is (center of screen, y up, WIDTHxHEIGHT logical units, rotation clockwise)
  (define t (send dc get-transformation))
  (send dc erase)
  (draw-background dc)
  (draw-framerate dc)
  (when ownship (draw-ship dc ownship))
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
  (define new-ownship (async-channel-try-get update-channel))
  (when new-ownship
    (set! last-update-time (current-inexact-milliseconds))
    (set! ownship (deserialize new-ownship))
    ; find my role
    (set! my-role (ship-helm ownship)))
  
  ; physics prediction
;  (when ownship
;    (define dt (/ (- current-time last-update-time) 1000))
;    (update-physics ownship dt)
;    (set! last-update-time current-time))
  
  ;rendering
  (when show-framerate?
    (set! frames (cons current-time (take frames (min 10 (length frames))))))
  (send canvas refresh-now)
  
  ;sleep so we don't hog the whole racket vm
  (when (dt . < . CLIENT_LOOP_DELAY)
    (sleep (- CLIENT_LOOP_DELAY dt)))
  
  (queue-callback client-loop #f))

(queue-callback client-loop #f)

(thread server-loop)
