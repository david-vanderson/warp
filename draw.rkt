#lang racket/base

(require racket/class
         racket/list
         racket/format
         racket/math
         racket/draw)

(require "defs.rkt")

(provide (all-defined-out))

(define-syntax-rule (keep-transform dc e ...)
  (begin
    (define t (send dc get-transformation))
    (define a (let () e ...))
    (send dc set-transformation t)
    a))

(define (dc-scale dc)
  (vector-ref (send dc get-initial-matrix) 0))

(define (canvas-scale canvas)
  (min (/ (send canvas get-width) WIDTH)
       (/ (send canvas get-height) HEIGHT)))

(define (screen->canon canvas x y)
  (define scale (canvas-scale canvas))
  (define cw (send canvas get-width))
  (define ch (send canvas get-height))
  (values (/ (- x (/ cw 2)) scale)
          (/ (- (/ ch 2) y) scale)))

(define (dc->canon canvas dc x y)
  (define m (send dc get-initial-matrix))
  (define sx (+ (* x (vector-ref m 0)) (* y (vector-ref m 1)) (vector-ref m 4)))
  (define sy (+ (* x (vector-ref m 2)) (* y (vector-ref m 3)) (vector-ref m 5)))
  (screen->canon canvas sx sy))

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


(define (draw-background dc ownspace center)
  (define x (- (posvel-x (obj-posvel center))))
  (define y (- (posvel-y (obj-posvel center))))
  (send dc set-brush nocolor 'transparent)
  (send dc draw-rectangle (- x 250) (- y 250) 500 500))


(define ship-external
  '((10 . 10)
    (0 . 20)
    (-10 . 10)
    (-10 . -10)
    (10 . -10)))

(define ship-internal
  '((10 . 10)
    (0 . 20)
    (-10 . 10)
    (-10 . -10)
    (10 . -10)))


(define (draw-object dc o center space)
  (cond
    ((ship? o)
     (draw-ship dc o center))
    ((plasma? o)
     (draw-plasma dc o center space))
    ((shield? o)
     (draw-shield dc space center o))))


(define (draw-ship dc s center)
  (keep-transform dc
    (define-values (x y) (recenter center s))
    (send dc translate x y)
    
    (keep-transform dc
      (send dc rotate (- (posvel-r (obj-posvel s))))
      ;      (for ((pod (ship-pods s)))
      ;        (draw-pod dc pod))
      (keep-transform dc
        (send dc rotate (/ pi 2))
        (send dc set-pen fgcolor 1 'solid)
        (send dc set-brush nocolor 'transparent)
        (send dc draw-polygon ship-external)))
    
    (define scale 0.5)
    (send dc scale scale (- scale))
    (define text (~r (ship-containment s) #:precision 0))
    (define-values (w h b v) (send dc get-text-extent text #f #t))
    (send dc translate (* -0.5 w) (* -0.5 h))
    (send dc set-brush fgcolor 'solid)
    (send dc set-pen nocolor 1 'transparent)
    (let ((p (new dc-path%)))
      (send p text-outline (send dc get-font) text 0 0)
      (send dc draw-path p 0 0))))


; assuming dc is already centered on middle of ship and rotated for the ship
(define (draw-pod dc pod)
  (when ((pod-dist pod) . > . 0)
    (keep-transform dc
      (send dc set-pen fgcolor 1 'solid)
      (send dc rotate (- (pod-angle pod)))
      (send dc draw-line 0 0 (pod-dist pod) 0)
      (send dc translate (pod-dist pod) 0)
      (send dc draw-ellipse -5 -5 10 10))))


(define (draw-plasma dc p center space)
  (define-values (x y) (recenter center p))
  (send dc set-pen "red" 1 'solid)
  (define rad (plasma-energy p))
  (define t (modulo (- (space-time space) (obj-start-time p)) 1000))
  (define rot (* pi (/ t 1000.0)))
  (send dc draw-line
        (- x (* rad (cos rot))) (- y (* rad (sin rot)))
        (+ x (* rad (cos rot))) (+ y (* rad (sin rot))))
  (set! rot (+ rot (/ pi 2)))
  (send dc draw-line
        (- x (* rad (cos rot))) (- y (* rad (sin rot)))
        (+ x (* rad (cos rot))) (+ y (* rad (sin rot))))
  (send dc draw-ellipse (- x (/ rad 2)) (- y (/ rad 2)) rad rad))


(define (shield-color s)
  (define b (send the-color-database find-color "blue"))
  (make-color (send b red)
              (send b green) 
              (send b blue)
              (/ (shield-energy s) 20.0)))


(define (draw-shield dc space center s)
  (define-values (x y) (recenter center s))
  (send dc set-pen (shield-color s) 3 'solid)
  (define len (shield-length s))
  (keep-transform dc
    (send dc translate x y)
    (send dc rotate (- (posvel-r (obj-posvel s))))
    (send dc draw-line 0 (* -0.5 len) 0 (* 0.5 len))))


(define (draw-no-role dc ownspace)
  (keep-transform dc
    (define max-x (space-sizex ownspace))
    (define max-y (space-sizey ownspace))
    (define scale (min (/ WIDTH max-x) (/ HEIGHT max-y)))
    (send dc scale scale scale)
    (define center (obj #f #f (posvel 0 0 0 0 0 0)))
    (draw-background dc ownspace center)
    (for ((o (space-objects ownspace)))
      (cond
        ((ship? o)
         (draw-ship dc o center))
        ((plasma? o)
         (draw-plasma dc o center ownspace)))))
  
  (define start-stacks
    (search ownspace (lambda (o) (and (multirole? o)
                                      (multirole-start? o))) #t))
  (cons
   leave-button
   (for/list ((s start-stacks)
              (i (in-naturals)))
     (define mr (car s))
     (button (+ LEFT 100 (* i 200)) (+ BOTTOM 60) 150 30 5 5 (obj-id mr)
             (format "~a on ~a" (role-name (multirole-role mr))
                     (ship-name (get-ship s)))))))


(define (draw-overlay dc ownspace stack)
  (when stack
    (define role (get-role stack))
    (keep-transform dc
      (send dc translate 0 (/ HEIGHT 2))
      (send dc scale 1 -1)
      (send dc draw-text (role-name role) 0 0))))


(define (draw-observer dc ownspace stack)
  (define center (get-center stack))
  (draw-background dc ownspace center)
  (for ((o (space-objects ownspace)))
    (cond
      ((ship? o)
       (draw-ship dc o center))
      ((plasma? o)
       (draw-plasma dc o center ownspace))))
  (list leave-button))


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


