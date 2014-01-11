#lang racket/base

(require racket/class)

(require "defs.rkt"
         "utils.rkt")

(provide (all-defined-out))


(define PLASMA_LIFE 5000)  ; ms after which plasma starts fading
(define PLASMA_DEATH 8000)  ; ms after which plasma is dead


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
  (send dc set-pen "red" 3 'solid)
  (define rad (plasma-radius space p))
  (define cycle 2000.0)
  (define t (modulo (- (space-time space) (obj-start-time p)) cycle))
  (define rot (* 2pi (/ t cycle)))
  (define num 5)
  (for ((i num))
    (define r (+ rot (/ (* i 2pi) num)))
    (send dc draw-line x y
          (+ x (* rad (cos r))) (+ y (* rad (sin r))))))

