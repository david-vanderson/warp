#lang racket/base

(require racket/class)

(require "defs.rkt"
         "utils.rkt")

(provide (all-defined-out))


(define PLASMA_LIFE 5000)  ; ms after which plasma starts fading
(define PLASMA_DEATH 8000)  ; ms after which plasma is dead


(define (plasma-radius space p)
  (define age (obj-age space p))
  (define fade (cond ((age . < . PLASMA_LIFE) 1.0)
                     ((age . > . PLASMA_DEATH) 0.0)
                     (else (/ (- PLASMA_DEATH age)
                              (- PLASMA_DEATH PLASMA_LIFE)))))
  (* fade (/ (plasma-energy p) 2)))


(define (plasma-dead? space p)
  (or ((obj-age space p) . > . PLASMA_DEATH)
      ((plasma-energy p) . < . 1)))


(define (reduce-plasma! space p damage)
  (set-plasma-energy! p (- (plasma-energy p) damage))
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

