#lang racket/gui

(require "defs.rkt")

(define frames '())  ; list of last few frame times

(define ownship (ship 0 0 (* 0.5 3 pi) 0 0 0
                      (list (shield 100 "red" 100
                                    '(100 50 25 0 20 20 20 20 20 20 20 20 20 20 20 20)))))
(define helm (* 0.5 3 pi))

(define (recenter ownship x y)
  (values (- x (object-x ownship)) (- y (object-y ownship))))

(define frame (new (class frame% (super-new))
                   (label "Warp")))

(define my-canvas
  (class canvas%
    (super-new)
    (define/override (on-event event)
      (when (send event button-down? 'left)
        (define x (- (send event get-x) (/ (send this get-width) 2)))
        (define y (- (/ (send this get-height) 2) (send event get-y)))
        ;(displayln (~a x " " y))
        (set! helm (atan y x))
        (when (helm . < . 0)
          (set! helm (+ helm (* 2 pi))))))
    (define/override (on-char event)
      ;(displayln (~v (send event get-key-code)))
      (case (send event get-key-code)
        ((#\a) (set-object-dr! ownship (+ (object-dr ownship) .03)))
        ((#\d) (set-object-dr! ownship (- (object-dr ownship) .03)))
        ((#\w)
         (set-object-dx! ownship (+ (object-dx ownship) (* 2 (cos (object-r ownship)))))
         (set-object-dy! ownship (+ (object-dy ownship) (* 2 (sin (object-r ownship))))))
        ((#\s)
         (set-object-dx! ownship (- (object-dx ownship) (* 2 (cos (object-r ownship)))))
         (set-object-dy! ownship (- (object-dy ownship) (* 2 (sin (object-r ownship))))))))))


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
  (draw-ship dc ownship)
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

(define (drag dv dt coef)
  (define newv (* dv (expt (1 . - . coef) dt)))
  (if ((abs newv) . < . EPSILON) 0 newv))

(define previous-time (current-inexact-milliseconds))

(define (game-loop)
  (define current-time (current-inexact-milliseconds))
  (define dt (/ (- current-time previous-time) 1000))
  (set! previous-time current-time)
  
  ;physics
  (set-object-x! ownship (+ (object-x ownship) (* dt (object-dx ownship))))
  (set-object-y! ownship (+ (object-y ownship) (* dt (object-dy ownship))))
  (set-object-r! ownship (+ (object-r ownship) (* dt (object-dr ownship))))
  (set-object-dx! ownship (drag (object-dx ownship) dt DRAG_COEF))
  (set-object-dy! ownship (drag (object-dy ownship) dt DRAG_COEF))
  (set-object-dr! ownship (drag (object-dr ownship) dt R_DRAG_COEF))
;  (match-define (ship x y r dx dy dr) ownship)
;  (set! ownship (ship (+ x (* dx dt))
;                      (+ y (* dy dt))
;                      (+ r (* dr dt))
;                      (drag dx dt DRAG_COEF)
;                      (drag dy dt DRAG_COEF)
;                      (drag dr dt R_DRAG_COEF)))
  (when ((object-r ownship) . > . (* 2 pi))
    (set! ownship (struct-copy ship ownship (r #:parent object (- (object-r ownship) (* 2 pi))))))
  (when ((object-r ownship) . < . 0)
    (set! ownship (struct-copy ship ownship (r #:parent object (+ (object-r ownship) (* 2 pi))))))
  
  ;steer the ship
  (cond ((and
          ((abs (- helm (object-r ownship))) . < . EPSILON)
          ((abs (object-dr ownship)) . < . EPSILON))
         (set-object-r! ownship helm)
         (set-object-dr! ownship 0))
        (else
         (define simple-diff (- helm (object-r ownship)))
         (define diff (if ((abs simple-diff) . <= . pi)
                          simple-diff
                          (- simple-diff)))
         ;(displayln (~a "helm " helm ", r " (object-r ownship) ", diff " diff))
         (set-object-dr! ownship ((if (positive? diff) + -) (object-dr ownship) (* RACC dt)))))
  
  ;rendering
  (when show-framerate?
    (set! frames (cons current-time (take frames (min 10 (length frames))))))
  
  (send canvas refresh-now)
  
  ;sleep so we don't hog the whole racket vm
  (when (dt . < . .01)
    (sleep (- 0.01 dt)))
  
  (queue-callback game-loop #f))

(queue-callback game-loop #f)

