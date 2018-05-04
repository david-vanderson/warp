#lang racket/base

(require racket/class
         racket/draw)

(require "defs.rkt"
         "utils.rkt"
         "draw-utils.rkt")

(provide (all-defined-out))


(define (upgrade-radius space u)
  5)

(define (upgrade-alive? space u)
  (or (not (upgrade-life u))
      ((obj-age space u) . <= . (upgrade-life u))))

(define (upgrade-color u)
  (case (upgrade-type u)
    (("engines") "red")
    (("turning") "yellow")
    (("hull") "green")
    (("radar") "blue")
    (("parts") "orange")
    (else #f)
    ))

(define (draw-upgrade csd center scale space u fowa)
  (obj-sprite u csd center scale LAYER_SHIPS 'circle (* 2.0 (upgrade-radius space u)) fowa 0.0 (upgrade-color u)))

(define types '("engines" "turning" "hull" "radar"))

(define (random-upgrade ownspace pv)
  (upgrade (next-id) (space-time ownspace) #t pv (list-ref types (random (length types))) 60000))