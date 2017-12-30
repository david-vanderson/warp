#lang racket/base

(require racket/class
         racket/math)

(require "defs.rkt"
         "utils.rkt"
         "draw-utils.rkt"
         "draw.rkt")

(provide (all-defined-out))

;; server

#;(define (shbolt-ai! space stack)
  (define changes '())
  (define ownship (get-ship stack))
  (define pod (get-pod stack))
  (define sb (findf shbolt? (pod-tools pod)))
  (when (and (ship-flying? ownship)
             (tool-online? sb)
             ((pod-energy pod) . > . (shbolt-shield-size sb)))
    
    (define ne (nearest-incoming-plasma space ownship))
  
    (when ne
      (define me (pod-obj pod ownship))
      (define t (target-angle me me ne ne SHIELD_SPEED))
      (when t
        (define podangle (angle-add (obj-r ownship) (pod-facing pod)))
        (define offset (angle-frto podangle t))
        (when ((abs offset) . < . (/ (pod-spread pod) 2))
          (define chance-per-sec (/ (pod-energy pod) (pod-maxe pod)))
          (set! chance-per-sec (expt chance-per-sec 0.7))
          (when ((random) . < . chance-per-sec)
            (set! changes (list (command (ob-id sb) t))))))))
  changes)


