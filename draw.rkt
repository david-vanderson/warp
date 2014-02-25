#lang racket/base

(require racket/class
         racket/list
         racket/draw)

(require "defs.rkt"
         "utils.rkt"
         "draw-utils.rkt"
         "plasma.rkt"
         "shield.rkt"
         "effect.rkt"
         "ships.rkt")

(provide (all-defined-out))

(define stars1-bitmap (make-bitmap 1 1))
(send stars1-bitmap load-file "images/stars.png" 'png/alpha)

(define background-bitmap (make-bitmap 1 1))
(send background-bitmap load-file "images/background.jpg")


(define (add-frame-time current-time frames)
  (cons current-time (take frames (min 10 (length frames)))))

(define (draw-framerate dc frames)
  (when ((length frames) . > . 1)
    (keep-transform dc
      (send dc translate (- (/ WIDTH 2)) (/ HEIGHT 2))
      (send dc scale 1 -1)
      (define start (list-ref frames (- (length frames) 1)))
      (define end (first frames))
      (define span (/ (- end start) 1000))
      (send dc draw-text (format "~a" (truncate (/ (- (length frames) 1) span))) 0 0))))


(define (draw-background dc space center bitmap scale parallax)
  (define repeatx (* scale (send bitmap get-width)))
  (define repeaty (* scale (send bitmap get-height)))
  (define x (remain (+ (* parallax (posvel-x (obj-posvel center)))
                       (/ (space-width space) 2)) repeatx))
  (define y (remain (+ (* parallax (posvel-y (obj-posvel center)))
                       (/ (space-height space) 2)) repeaty))
  (for* ((i (in-range (- (ceiling (/ (- (/ WIDTH 2) x) repeatx)))
                      (ceiling (/ (+ (/ WIDTH 2) x) repeatx))))
         (k (in-range (- (ceiling (/ (- (/ HEIGHT 2) y) repeaty)))
                      (ceiling (/ (+ (/ HEIGHT 2) y) repeaty)))))
    
    (define xstart (- (* i repeatx) x))
    (define ystart (- (* k repeaty) y))
    (keep-transform dc
      (send dc translate xstart ystart)
      (send dc scale scale scale)
      (send dc draw-bitmap bitmap 0 0))))


(define (draw-object dc o center space)
  (cond
    ((ship? o)
     (draw-ship dc o center))
    ((plasma? o)
     (draw-plasma dc o center space))
    ((shield? o)
     (draw-shield dc space center o))
    ((effect? o)
     (draw-effect dc space center o))))


(define (draw-view dc center space)
  (draw-background dc space center background-bitmap 3 0.5)
  (draw-background dc space center stars1-bitmap 8 1)
  (define objects (space-objects space))
  (define ships (filter ship? objects))
  (define effects (filter effect? objects))
  (define other (remove* (append ships effects) objects))
  
  (define backeffects (filter backeffect? effects))
  (set! effects (remove* backeffects effects))
  
  (for ((o (append backeffects ships other effects)))
    (draw-object dc o center space)))


(define (draw-ship dc s center)
  (keep-transform dc
    (define-values (x y) (recenter center s))
    (send dc translate x y)
    
    (keep-transform dc
      (send dc rotate (- (posvel-r (obj-posvel s))))
      (send dc set-pen fgcolor 1 'solid)
      (send dc set-brush nocolor 'transparent)
      (define ship-bitmap (get-ship-bitmap s))
      (send dc draw-bitmap
              ship-bitmap
              (- (/ (send ship-bitmap get-width) 2))
              (- (/ (send ship-bitmap get-height) 2)))
      
;      (for ((pod (ship-pods s)))
;        (draw-pod dc pod)))
    
;    (define scale 0.5)
;    (send dc scale scale (- scale))
;    (define text (~r (ship-containment s) #:precision 0))
;    (define-values (w h b v) (send dc get-text-extent text #f #t))
;    (send dc translate (* -0.5 w) (* -0.5 h))
;    (send dc set-pen nocolor 1 'transparent)
;    (send dc set-brush fgcolor 'solid)
;    (let ((p (new dc-path%)))
;      (send p text-outline (send dc get-font) text 0 0)
;      (send dc draw-path p 0 0))
    )))


; assuming dc is already centered on middle of ship and rotated for the ship
;(define (draw-pod dc pod)
;  (keep-transform dc
;    (send dc set-pen fgcolor 1 'solid)
;    (send dc rotate (- (pod-angle pod)))
;    (send dc translate (pod-dist pod) 0)
;    (send dc draw-ellipse -5 -5 10 10)))


(define (draw-no-role dc space)
  (keep-transform dc
    (define max-x (space-width space))
    (define max-y (space-height space))
    (define scale (min (/ WIDTH max-x) (/ HEIGHT max-y)))
    (send dc scale scale scale)
    (define center (obj #f #f (posvel 0 0 0 0 0 0 0)))
    (draw-background dc space center background-bitmap 3 0.5)
    (draw-background dc space center stars1-bitmap 8 1)
    (for ((o (space-objects space)))
      (draw-object dc o center space)))
  
  (define start-stacks
    (search space (lambda (o) (and (multipod? o)
                                   (multipod-start? o))) #t))
  
  (set! start-stacks (filter (lambda (s) (obj-posvel (get-ship s))) start-stacks))
  
  (for/list ((s start-stacks)
              (i (in-naturals)))
     (define mp (car s))
     (button (+ LEFT 100 (* i 200)) (+ BOTTOM 60) 150 30 5 5 (ob-id mp)
             (format "~a on ~a" (role-name (pod-role mp))
                     (ship-name (get-ship s))))))


(define (draw-observer dc space stack)
  (draw-view dc (get-center stack) space)
  (draw-hud dc (get-ship stack) #f)
  (for ((p (ship-pods (get-ship stack)))
        (i (in-naturals)))
    (define e (inexact->exact (round (pod-energy p))))
    (draw-hud-status-text dc (+ 10 i) (format "~a ~a" (role-name (pod-role p)) e)))
  (list leave-button))


(define (draw-overlay dc ownspace stack)
  (when stack
    (define role (get-role stack))
    (define str (format "~a" (role-name role)))
    (for ((s (get-ships stack)))
      (set! str (format "~a on ~a" str (ship-name s))))
    (keep-transform dc
      (send dc translate 0 (/ HEIGHT 2))
      (send dc scale 1 -1)
      (send dc draw-text str 0 0))))


(define (draw-hud dc ship pod)
  (when (ship-flying? ship)
    (define con (inexact->exact (round (ship-containment ship))))
    (draw-hud-status-text dc 1 (format "Con ~a" con)))
  (when pod
    (define e (inexact->exact (round (pod-energy pod))))
    (draw-hud-status-text dc 3 (format "Bat ~a" e))))


(define (draw-buttons dc buttons)
  (for ((b buttons))
    (keep-transform dc
      (define-values (x y w h) (values (button-x b) (button-y b)
                                       (button-width b) (button-height b)))
      (send dc set-brush "darkgray" 'solid)
      (send dc set-pen fgcolor 1 'solid)
      (send dc draw-rectangle x y w h)
      (send dc translate x (+ y h))
      (send dc scale 1 -1)
      (send dc draw-text (button-label b) (button-left-inset b) (button-top-inset b)))))


