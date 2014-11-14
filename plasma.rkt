#lang racket/base

(require racket/class
         racket/draw)

(require "defs.rkt"
         "utils.rkt"
         "draw-utils.rkt")

(provide (all-defined-out))


(define PLASMA_LIFE 3000)  ; ms after which plasma starts fading
(define PLASMA_DEATH 5000)  ; ms after which plasma is dead

(define plasma-bitmap (make-bitmap 1 1))
(send plasma-bitmap load-file (string-append "images/" "plasma" ".png") 'png/alpha)



(define (plasma-energy space p)
  (* (linear-fade (obj-age space p) PLASMA_LIFE PLASMA_DEATH)
     (plasma-e p)))


(define (plasma-radius space p)
  (/ (plasma-energy space p) 2))


(define (plasma-dead? space p)
  (or ((obj-age space p) . > . PLASMA_DEATH)
      ((plasma-energy space p) . < . 1)))


(define (reduce-plasma! space p damage)
  (set-plasma-e! p (- (plasma-e p) (/ damage (linear-fade (obj-age space p) PLASMA_LIFE PLASMA_DEATH))))
  (when (plasma-dead? space p)
    (set-space-objects! space (remove p (space-objects space)))))


(define (draw-plasma dc p center space)
  (define-values (x y) (recenter center p))
  (keep-transform dc
    (send dc translate x (- y))
    (send dc scale
          (/ (plasma-radius space p) (send plasma-bitmap get-width) 0.5)
          (/ (plasma-radius space p) (send plasma-bitmap get-height) 0.5))
    (define cycle 1000.0)
    (define t (modulo (obj-age space p) cycle))
    (define rot (* 2pi (/ t cycle)))
    (send dc rotate rot)
    (send dc draw-bitmap
          plasma-bitmap
          (- (/ (send plasma-bitmap get-width) 2))
          (- (/ (send plasma-bitmap get-height) 2)))))
  
;  (send dc set-pen "red" 3 'solid)
;  (define rad (plasma-radius space p))
;  (define cycle 2000.0)
;  (define t (modulo (obj-age space p) cycle))
;  (define rot (* 2pi (/ t cycle)))
;  (define num 5)
;  (for ((i num))
;    (define r (+ rot (/ (* i 2pi) num)))
;    (send dc draw-line x y
;          (+ x (* rad (cos r))) (+ y (* rad (sin r))))))

