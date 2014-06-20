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


(define (draw-background dc space center bitmap scale parallax width height)
  (define repeatx (* scale (send bitmap get-width)))
  (define repeaty (* scale (send bitmap get-height)))
  (define x (remain (* parallax (posvel-x (obj-posvel center))) repeatx))
  (define y (remain (* parallax (posvel-y (obj-posvel center))) repeaty))
  
;  (set! width (/ width 2))
;  (set! height (/ height 2))
  
  (define istart (- (ceiling (- (/ (- (/ width 2) x) repeatx) 0.5))))
  (define iend (add1 (ceiling (- (/ (+ (/ width 2) x) repeatx) 0.5))))
  (define kstart (- (ceiling (- (/ (- (/ height 2) y) repeaty) 0.5))))
  (define kend (add1 (ceiling (- (/ (+ (/ height 2) y) repeaty) 0.5))))
;  (printf "i from ~a to ~a\n" istart iend)
;  (printf "k from ~a to ~a\n" kstart kend)
  
  (for* ((i (in-range istart iend))
         (k (in-range kstart kend)))
    (define xx (- x (* i repeatx)))
    (define yy (- y (* k repeaty)))
    (keep-transform dc
      (send dc translate (- xx) (- yy))
      (send dc scale scale scale)
      (send dc translate (- (/ (send bitmap get-width) 2)) (/ (send bitmap get-height) 2))
      (send dc scale 1 -1)
      (send dc draw-bitmap bitmap 0 0)))
  
;  (send dc set-pen "white" 10 'solid)
;  (send dc set-brush nocolor 'transparent)
;  (send dc draw-rectangle (- (/ width 2)) (- (/ height 2)) width height)
  
  )


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


(define (draw-server-objects dc center space)
  (send dc set-pen "hotpink" 1 'solid)
  (send dc set-brush nocolor 'transparent)
  (for ((o (space-objects space)))
    (keep-transform dc
      (define-values (x y) (recenter center o))
      (send dc translate x y)
      (send dc draw-ellipse -2 -2 4 4)
      (when (ship? o)
        (define r (ship-radius o))
        (send dc draw-ellipse (- r) (- r) (* 2 r) (* 2 r))))))


(define (draw-view dc center space)
  (draw-background dc space center background-bitmap 4 1 WIDTH HEIGHT)
  ;(draw-background dc space center stars1-bitmap 8 2 WIDTH HEIGHT)
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
      (send dc scale 1 -1)
      (send dc draw-bitmap
              ship-bitmap
              (- (/ (send ship-bitmap get-width) 2))
              (- (/ (send ship-bitmap get-height) 2)))
      
;      (for ((pod (ship-pods s)))
;        (draw-pod dc pod)))
    
;    (define scale 0.5)
;    (send dc scale scale (- scale))
;    (define text (~r (ship-con s) #:precision 0))
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
    (draw-background dc space center background-bitmap 4 1 max-x max-y)
    ;(draw-background dc space center stars1-bitmap 8 2 max-x max-y)
    (for ((o (space-objects space)))
      (draw-object dc o center space)))
  
  (define buttons (list leave-button))
  
  (define start-stacks
    (search space (lambda (o) (and (multipod? o)
                                   (multipod-start? o))) #t))
  
  (set! start-stacks (filter (lambda (s) (obj-posvel (get-ship s))) start-stacks))
  
  (for ((s start-stacks)
        (i (in-naturals)))
    (define mp (car s))
    (define b (button (+ LEFT 100 (* i 250)) (+ BOTTOM 60) 200 30 5 5 (ob-id mp)
                      (format "~a on ~a" (role-name (pod-role mp))
                              (ship-name (get-ship s)))))
    (set! buttons (append buttons (list b))))
  buttons)


(define (draw-observer dc space stack serverspace)
  (draw-view dc (get-center stack) space)
  (when serverspace (draw-server-objects dc (get-center stack) serverspace))
  (draw-hud dc (get-ship stack) #f)
  (for ((p (ship-pods (get-ship stack)))
        (i (in-naturals)))
    (define e (inexact->exact (round (pod-energy p))))
    (draw-hud-status-text dc (+ 10 i) (format "~a ~a" (role-name (pod-role p)) e)))
  (list leave-button))


(define (draw-overlay dc space stack)
  (when stack
    (define role (get-role stack))
    (define str (format "~a" (role-name role)))
    (for ((s (get-ships stack)))
      (set! str (format "~a on ~a" str (ship-name s))))
    (keep-transform dc
      (send dc translate 0 (/ HEIGHT 2))
      (send dc scale 1 -1)
      (send dc draw-text str 0 0)))
  
  (when space
    (define max 8)
    (define msgs (filter message? (space-objects space)))
    (set! msgs (take (reverse msgs) (min 8 (length msgs))))
    (for ((m msgs) (i max))
      (keep-transform dc
        (send dc translate (- (/ WIDTH 2)) (- (/ HEIGHT 2)))
        (send dc translate 0 (* (+ max 6) 20))
        (send dc translate 0 (* i -20))
        (send dc scale 1 -1)
        (define cc "white")
        (when ((obj-age space m) . > . (/ MSG_FADE_TIME 2))
          (define z (min 1.0 (/ (- (obj-age space m) (/ MSG_FADE_TIME 2)) (/ MSG_FADE_TIME 2))))
          (set! cc (linear-color "white" "white" z (- 1.0 z))))
        (send dc set-text-foreground cc)
        (send dc draw-text (message-msg m) 0 0)))
    (send dc set-text-foreground "white")))


(define (draw-hud dc ship pod)
  (define con (inexact->exact (round (ship-con ship))))
  (draw-hud-status-text dc 1 (format "Ship Hull ~a" con))
  (when pod
    (define e (inexact->exact (round (pod-energy pod))))
    (draw-hud-status-text dc 4 (format "Pod Bat ~a" e))))


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


