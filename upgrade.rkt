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


(define (draw-upgrade csd center scale space u fowa layer-ships)
  (obj-sprite u csd center scale layer-ships 'circle
              (/ (* 2.0 (upgrade-radius space u)) 100) fowa 0.0
              (send the-color-database find-color (upgrade-color u))))


; return a list of changes
(define (upgrade-ship-random space ship [alltypes '(engines turning hull radar)])
  (define changes '())
  (let loop ((types alltypes))
    (cond
      ((null? types)
       (printf "upgrade-ship-random ran out of types ~v for ship ~v\n" alltypes ship))
      (else
       (define t (list-ref types (random (length types))))
       (case t
         ((engines)
          (define t (ship-tool ship 'engine))
          (when t
            (append! changes
                     (chstat (ob-id ship) 'toolval
                             (list 'engine (* 1.1 (tool-val t))))
                     (make-message space "engines"))))
         ((turning)
          (for ((tname '(turnleft turnright steer)))
            (define t (ship-tool ship tname))
            (when t
              (append! changes
                       (chstat (ob-id ship) 'toolval (list tname (* 1.1 (tool-val t)))))))
          (when (not (null? changes))
            (append! changes
                     (make-message space "turning"))))
         ((hull)
          (append! changes
                   (chstat (ob-id ship) 'hull (* 1.1 (ship-maxcon ship)))
                   (make-message space "hull")))
         ((radar)
          (append! changes
                   (chstat (ob-id ship) 'radar (* 1.1 (ship-radar ship)))
                   (make-message space "radar"))))
       (when (null? changes)
         (loop (filter (lambda (x) (not (equal? x t)))
                       types))))))
  changes)