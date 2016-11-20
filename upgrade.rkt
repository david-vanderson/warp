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

(define (upgrade-color u)
  (case (upgrade-type u)
    (("power") "red")
    (("thrust") "yellow")
    (("bat") "blue")
    (("con") "green")
    (("radar") "cyan")))

(define (draw-upgrade dc space u)
  (send dc set-brush (upgrade-color u) 'solid)
  (send dc set-pen nocolor 1 'transparent)
  (send dc draw-ellipse (- (obj-x u) 5) (- (obj-y u) 5) 10 10))

(define types '("power" "thrust" "bat" "con" "radar"))

(define (random-upgrade ownspace pv)
  (upgrade (next-id) (space-time ownspace) pv (list-ref types (random (length types)))))