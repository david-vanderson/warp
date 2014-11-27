#lang racket/base

(require racket/class
         racket/draw)

(require "defs.rkt"
         "utils.rkt"
         "draw-utils.rkt")

(provide (all-defined-out))


(define UPGRADE_LIFE 60000)  ; ms after which upgrade dies


(define (upgrade-radius space u)
  5)

(define (upgrade-dead? space u)
  ((obj-age space u) . > . UPGRADE_LIFE))


(define (draw-upgrade dc space center u)
  (define-values (x y) (recenter center u))
  (define ucolor
    (case (upgrade-type u)
      (("power") "red")
      (("thrust") "yellow")
      (("bat") "blue")
      (("con") "green")))
  (send dc set-brush ucolor 'solid)
  (send dc set-pen nocolor 1 'transparent)
  (send dc draw-ellipse (- x 5) (- (- y) 5) 10 10))

(define types '("power" "thrust" "bat" "con"))

(define (random-upgrade ownspace pv)
  (upgrade (next-id) (space-time ownspace) pv (list-ref types (random 4))))