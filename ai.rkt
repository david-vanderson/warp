#lang racket/base

(require racket/math)

(require "defs.rkt"
         "utils.rkt"
         "physics.rkt"
         "tactics.rkt"
         "pilot.rkt")

(provide (all-defined-out))


(define (ai-tactics! space ships ownship)
  (for ((tstack (search ownship tactics? #t)))
    (cond
      (((random) . > . 0.9)
       (define pod (get-pod tstack))
       (define ps (obj-posvel ownship))
       (define podangle (+ (posvel-r ps) (pod-angle pod)))
       (update-tactics (struct-copy tactics (car tstack) (shield podangle))
        space tstack))))
  #f)
