#lang racket/base

(require racket/class)

(require "defs.rkt"
         "draw-utils.rkt")

(provide (all-defined-out))


(define (plasma-radius p)
  (/ (plasma-energy p) 2))


(define (plasma-dead? p)
  ((plasma-energy p) . < . 1))


(define (reduce-plasma! space p damage)
  (set-plasma-energy! p (- (plasma-energy p) damage))
  (when (plasma-dead? p)
    (set-space-objects! space (remove p (space-objects space)))))


(define (draw-plasma dc p center space)
  (define-values (x y) (recenter center p))
  (send dc set-pen "red" 3 'solid)
  (define rad (plasma-radius p))
  (define cycle 2000.0)
  (define t (modulo (- (space-time space) (obj-start-time p)) cycle))
  (define rot (* 2pi (/ t cycle)))
  (define num 5)
  (for ((i num))
    (define r (+ rot (/ (* i 2pi) num)))
    (send dc draw-line x y
          (+ x (* rad (cos r))) (+ y (* rad (sin r))))))

