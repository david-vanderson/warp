#lang racket/base

(require racket/class
         racket/math
         racket/list)

(require "defs.rkt")

(provide (all-defined-out))


(define show-framerate? #t)
(define frames '())  ; list of last few frame times

(define (add-frame-time current-time)
  (when show-framerate?
    (set! frames (cons current-time (take frames (min 10 (length frames)))))))

(define (draw-framerate dc)
  (when (and show-framerate? (not (empty? frames)))
    (define t (send dc get-transformation))
    (send dc translate (- (/ WIDTH 2)) (/ HEIGHT 2))
    (send dc scale 1 -1)
    (define start (list-ref frames (- (length frames) 1)))
    (define end (first frames))
    (define span (/ (- end start) 1000))
    (send dc draw-text (format "~a" (truncate (/ (- (length frames) 1) span))) 0 0)
    (send dc set-transformation t)))


(define (recenter thing x y)
  (values (- x (thing-x thing)) (- y (thing-y thing))))


(define (draw-background dc ownspace center)
  (define t (send dc get-transformation))
  (define-values (x y) (recenter center 0 0))
  (send dc draw-rectangle (- x 250) (- y 250) 500 500)
  (send dc set-transformation t))


(define (draw-shield dc shield)
  (define t (send dc get-transformation))
  
  (define num (vector-length (shield-sections shield)))
  (define radius (shield-radius shield))
  
  (for ((section (shield-sections shield))
        (i (in-naturals))
        #:when (section . > . 0))
    (define linear-strength (/ section (shield-max shield)))
    (define log-strength (/ (log (add1 section)) (log (shield-max shield))))
    (send dc set-pen (shield-color shield) (* 3 (* 0.5 (+ linear-strength log-strength))) 'solid)
    (define r (- 2pi (/ (* 2pi i) num)))
    (define arc-size (* log-strength (/ 2pi num)))
    (send dc draw-arc
          (- radius) (- radius)
          (* 2 radius) (* 2 radius)
          (- r (* 0.5 arc-size)) (+ r (* 0.5 arc-size))))
  
  (send dc set-transformation t))


(define (draw-ship dc s center)
  (define-values (x y) (recenter center (thing-x s) (thing-y s)))
  (define t (send dc get-transformation))
  (send dc translate x y)
  (send dc rotate (- (thing-r s)))
  (for ((shield (ship-shields s)))
    (draw-shield dc shield))
  
  (send dc set-pen "black" 1 'solid)
  (send dc draw-polygon '((10 . 10)
                          (20 . 0)
                          (10 . -10)
                          (-10 . -10)
                          (-10 . 10)))
  (send dc set-transformation t))


(define (draw-plasma dc p center)
  (define-values (x y) (recenter center (thing-x p) (thing-y p)))
  (send dc set-pen (plasma-color p) 1 'solid)
  (define rad (plasma-energy p))
  (send dc draw-ellipse (- x (/ rad 2)) (- y (/ rad 2)) rad rad))


(define (draw-all canvas dc ownspace stack)
  (define center (get-center stack))
  (draw-background dc ownspace center)
  (for ((o (space-objects ownspace)))
    (cond
      ((ship? o)
       (draw-ship dc o center))
      ((plasma? o)
       (draw-plasma dc o center))))
  
  (draw-framerate dc))
