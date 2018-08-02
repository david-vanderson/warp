#lang racket/base

(require racket/class
         racket/match
         mode-lambda
         (submod mode-lambda/text/runtime private)
         racket/fixnum
         racket/flonum
         racket/draw)

(require "defs.rkt"
         "utils.rkt")

(provide (all-defined-out))

(define mapcol (make-color 0 0 200 1.0))  ; sector lines
(define zoomcol (make-color 130 50 10 1.0))  ; zoom meter

(define canon-width 800.0)
(define canon-height 600.0)
(define (set-canon-width! w)
  (set! canon-width w))
(define (set-canon-height! h)
  (set! canon-height h))
(define (left) (- (/ canon-width 2)))
(define (right) (/ canon-width 2))
(define (top) (- (/ canon-height 2)))
(define (bottom) (/ canon-height 2))

(define (xy->screen x y center scale)
  (values (* (- x (obj-x center)) scale)
          (* (- (obj-y center) y) scale)))

(define (space->canon center zoom x y)
  (values (* zoom (- x (obj-x center)))
          (* zoom (- (obj-y center) y))))

(define (canon->space center zoom x y)
  (values (+ (obj-x center) (/ x zoom))
          (- (obj-y center) (/ y zoom))))

(define (obj->screen o center scale)
  (xy->screen (obj-x o) (obj-y o) center scale))

(define (ship-w s scale)
  ; multiply ship width by 0.7, which is roughly sqrt(2)/2
  ; to make sure the corners and hp bar are above a rotated square ship
  (max 8.0 (* scale (* (max 32 (ship-sprite-size s)) 0.7))))

(define (sprite-size csd sym)
  (define w (sprite-width csd (sprite-idx csd sym)))
  (define h (sprite-height csd (sprite-idx csd sym)))
  (max w h))

(define (xy-sprite x y csd scale layer sprsym size a r color)
  (sprite x y (sprite-idx csd sprsym)
          #:layer layer #:a (exact->inexact a) #:theta (exact->inexact (- r))
          #:m (exact->inexact (* scale size))
          #:r (send color red) #:g (send color green) #:b (send color blue)))

(define (obj-sprite o csd center scale layer sprsym size a r color)
  (define-values (x y) (obj->screen o center scale))
  (xy-sprite x y csd scale layer sprsym size a r color))

(define (make-text-aligned-sizer f csd)
  (match-define (*ml-font char->char-id) f)
  (λ (text #:mx [mx 1.0]
           #:my [my 1.0])
    (define idxs
      (for/list ([c (in-string text)])
        (define ci (hash-ref char->char-id c))
        (define idx (sprite-idx csd ci))
        (unless idx
          (local-require mode-lambda/core)
          (error 'make-text-aligned-sizer "Cannot find sprite ~v" ci))
        idx))
    (define-values (width height)
      (for/fold ([w 0.0] [h 0.0]) ([i (in-list idxs)])
        (values (fl+   w (ceiling (fl* mx (fx->fl (sprite-width csd i)))))
                (flmax h (ceiling (fl* my (fx->fl (sprite-height csd i))))))))
    (values width height)))

(define (make-text-aligned-renderer f csd)
  (match-define (*ml-font char->char-id) f)
  (λ (text tx ty
           #:layer [layer 0]
           #:mx [mx 1.0]
           #:my [my 1.0]
           #:r [r 0]
           #:g [g 0]
           #:b [b 0]
           #:a [a 1.0])
    (define idxs
      (for/list ([c (in-string text)])
        (define ci (hash-ref char->char-id c))
        (define idx (sprite-idx csd ci))
        (unless idx
          (local-require mode-lambda/core)
          (error 'make-text-renderer "Cannot find sprite ~v" ci))
        idx))
    (define-values (width height)
      (for/fold ([w 0.0] [h 0.0]) ([i (in-list idxs)])
        (values (fl+   w (ceiling (fl* mx (fx->fl (sprite-width csd i)))))
                (flmax h (ceiling (fl* my (fx->fl (sprite-height csd i))))))))
    (define sx (round (fl- tx (fl/ width 2.0))))
    (define  y (if (even? height)
                   (round ty)
                   (- (round (+ ty 0.5)) 0.5)))
    (define-values (lx st)
      (for/fold ([sx sx] [st #f])
                ([i (in-list idxs)])
        (define w (ceiling (fl* mx (fx->fl (sprite-width csd i)))))
        (define x (fl+ sx (fl/ w 2.0)))
        ;(printf "w x,y ~a ~a,~a\n" w x y)
        (values (fl+ sx w)
                (cons (sprite x y i
                              #:layer layer
                              #:mx mx #:my my
                              #:r r #:g g #:b b #:a a)
                      st))))
    st))

(define (text-sprite textr textsr txt x y layer (a 1.0) (color "white"))
  (when (string? color)
    (set! color (send the-color-database find-color color)))
  (define-values (width height) (textsr txt))
  (textr txt (exact->inexact (+ x (* 0.5 width))) (exact->inexact (+ y (* 0.5 height))) #:layer layer
         #:r (send color red) #:g (send color green) #:b (send color blue) #:a a))

(define (get-alpha x y fowlist)
  (define a 0.0)
  (for ((f (in-list fowlist)))
    (define dx (- x (car f)))
    (define dy (- y (cadr f)))
    (define r (caddr f))
    (define d (sqrt (+ (* dx dx) (* dy dy))))
    (define fa (linear-fade d r (* r 1.1)))
    (set! a (max a fa)))
  a)

(define (get-red space ship)
  (define hpfrac (max 0.0 (/ (ship-con ship) (ship-maxcon ship))))
  (cond
    ((hpfrac . < . 0.5)
     (define cycletime 2500.0)
     (define z (cycletri (obj-age space ship) cycletime))
     (define alpha (* z (- 0.8 hpfrac)))
     (inexact->exact (round (* alpha 255))))
    (else 0)))


(define (rect-outline csd x y w h thick #:layer [layer LAYER_UI]
                      #:r [r 255] #:g [g 255] #:b [b 255] #:a [a 1.0])
  (define-values (idx-w mx-w)
    (if (w . > . 100.0)
        (values (sprite-idx csd '1000x10) (/ (+ w thick) 1000.0))
        (values (sprite-idx csd '100x10) (/ (+ w thick) 100.0))))
  (define-values (idx-h mx-h)
    (if (h . > . 100.0)
        (values (sprite-idx csd '1000x10) (/ (+ h thick) 1000.0))
        (values (sprite-idx csd '100x10) (/ (+ h thick) 100.0))))
  (define xoffs (list (/ w 2) (- (/ w 2))))
  (define yoffs (list (/ h 2) (- (/ h 2))))
  (append
   (for/list ((xoff xoffs)) 
     (sprite (+ x xoff) y idx-h
             #:layer layer
             #:mx mx-h #:my (/ thick 10.0) #:theta pi/2
             #:r r #:g g #:b b #:a a))
   (for/list ((yoff yoffs))
     (sprite x (+ y yoff) idx-w
             #:layer layer
             #:mx mx-w #:my (/ thick 10.0)
             #:r r #:g g #:b b #:a a))))

(define (rect-filled csd x y w h #:layer [layer LAYER_UI]
                     #:r [r 255] #:g [g 255] #:b [b 255] #:a [a 1.0])
  (define-values (idx mw mh)
    (cond
      ((and (w . > . 100.0)
            (h . > . 100.0))
       (values (sprite-idx csd '1000x1000) (/ w 1000.0) (/ h 1000.0)))
      ((w . > . 100.0)
       (values (sprite-idx csd '1000x100) (/ w 1000.0) (/ h 100.0)))
      ((h . > . 100.0)
       (values (sprite-idx csd '100x1000) (/ w 100.0) (/ h 1000.0)))
      (else
       (values (sprite-idx csd '100x100) (/ w 100.0) (/ h 100.0)))))
  (sprite x y idx
          #:layer layer
          #:mx mw #:my mh
          #:r r #:g g #:b b #:a a))


(define (canvas-scale canvas)
  (min (/ (send canvas get-width) canon-width)
       (/ (send canvas get-height) canon-height)))


(define (screen->canon canvas x y)
  (define scale (canvas-scale canvas))
  (define cw (send canvas get-width))
  (define ch (send canvas get-height))
  (values (/ (- x (/ cw 2)) scale)
          (/ (- y (/ ch 2)) scale)))


(define (linear-color color1 color2 z alpha)
  (define a (send the-color-database find-color color1))
  (define b (send the-color-database find-color color2))
  (define nz (- 1.0 z))
  (make-color (inexact->exact (floor (+ (* nz (send a red))   (* z (send b red)))))
              (inexact->exact (floor (+ (* nz (send a green)) (* z (send b green)))))
              (inexact->exact (floor (+ (* nz (send a blue))  (* z (send b blue)))))
              alpha))


(define (add-offline-button! tool b send-commands (dmgstr "offline"))
  (define offline (findf (lambda (d) (equal? dmgstr (dmg-type d))) (tool-dmgs tool)))
  (cond
    (offline
     (set-button-draw! b 'dmg)
     (dmgbutton 'normal #f #f
                (button-x b) (- (button-y b) (button-height b))
                (button-width b) (button-height b)
     "Offline"
     (lambda (x y) (void))
     (/ (dmg-energy offline) (dmg-size offline)) (dmg-fixing? offline)))
    (else
     #f)))

