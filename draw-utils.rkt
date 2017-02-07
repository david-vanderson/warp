#lang racket/base

(require racket/class
         mode-lambda
         racket/draw)

(require "defs.rkt"
         "utils.rkt")

(provide (all-defined-out))

(define txtsize (box 5))

(define (xy->screen x y center scale)
  (values (* (- x (obj-x center)) scale)
          (* (- (obj-y center) y) scale)))

(define (space->canon center zoom x y)
  (values (* zoom (- x (obj-x center)))
          (* zoom (- (obj-y center) y))))

(define (obj->screen o center scale)
  (xy->screen (obj-x o) (obj-y o) center scale))

(define (xy-sprite x y csd scale layer sprsym size a r color)
  (when (string? color)
    (set! color (send the-color-database find-color color)))
  
  (define sprite-size (max (sprite-width csd (sprite-idx csd sprsym))
                           (sprite-height csd (sprite-idx csd sprsym))))
  (sprite x y (sprite-idx csd sprsym)
          #:layer layer #:a (exact->inexact a) #:theta (exact->inexact (- r))
          #:m (exact->inexact (/ (* size scale) sprite-size))
          #:r (send color red) #:g (send color green) #:b (send color blue)))

(define (obj-sprite o csd center scale layer sprsym size a r color)
  (define-values (x y) (obj->screen o center scale))
  (xy-sprite x y csd scale layer sprsym size a r color))

(define (text-sprite textr txt x y layer (a 1.0) (color "white"))
  (when (string? color)
    (set! color (send the-color-database find-color color)))
  (textr txt (exact->inexact (+ x (* 0.5 (unbox txtsize) (string-length txt)))) (exact->inexact y) #:layer layer
         #:r (send color red) #:g (send color green) #:b (send color blue) #:a a))

(define (get-alpha x y fowlist)
  (define a 0.0)
  (for ((f fowlist))
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
     (define cycletime (+ 200 (* 5000 hpfrac)))
     (define z (cycletri (obj-age space ship) cycletime))
     (define alpha (+ 0.0 (* 1.0 z (max 0.1 (- 0.8 hpfrac)))))
     (inexact->exact (round (* alpha 255))))
    (else 0)))

(define-syntax-rule (keep-transform dc e ...)
  (begin
    (define t (send dc get-transformation))
    (define a (let () e ...))
    (send dc set-transformation t)
    a))


(define (center-on dc o (rotate #t))
  (send dc translate (obj-x o) (obj-y o))
  (when rotate
    ; rotate such that positive x axis points in the obj-r direction
    (send dc rotate (- (obj-r o)))))


(define (canvas-scale canvas)
  (min (/ (send canvas get-width) WIDTH)
       (/ (send canvas get-height) HEIGHT)))


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
     (dmgbutton 'normal #f
                (button-x b) (- (button-y b) (button-height b))
                (button-width b) (button-height b)
     "Offline"
     (lambda (x y) (send-commands (command (ob-id offline)
                                           (not (dmg-fixing? offline)))))
     (/ (dmg-energy offline) (dmg-size offline)) (dmg-fixing? offline)))
    (else
     #f)))

