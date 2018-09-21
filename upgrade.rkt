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

(define (ship-msg space ship msg)
  (define w (ship-w ship 1.0))
  (define y (if ((obj-dy ship) . < . 0)
                (+ (obj-y ship) w 16)
                (- (obj-y ship) w 10)))
  (make-ann-text (obj-x ship) y #:pos 'space
                 (space-time space) 1000 2000 msg))
  
; return a list of changes
(define (upgrade-ship-random space ship
                             [alltypes '(engine warp
                                         turning hull radar
                                         pbolt missile cannon regen)]
                             #:amount [amount 1.1])
  (define amtstr (number->string (inexact->exact (round (* 100.0 amount)))))
  (define changes '())
  (let loop ((types alltypes))
    (cond
      ((null? types)
       (printf "upgrade-ship-random ran out of types ~v for ship ~v\n" alltypes ship))
      (else
       (define t (list-ref types (random (length types))))
       (case t
         ((warp)
          (define t (ship-tool ship 'warp))
          (when t
            (append! changes
                     (chstat (ob-id ship) 'toolval
                             (list 'warp (list (* amount (car (tool-val t)))  ; speed
                                               (* (/ amount) (cadr (tool-val t)))  ; threshold
                                               0.0)))
                     (chadd (ship-msg space ship (string-append "warp " amtstr "%")) #f))))
         ((regen)
          (define t (ship-tool ship 'regen))
          (when t
            (append! changes
                     (chstat (ob-id ship) 'toolval
                             (list 'regen (* amount (tool-val t))))
                     (chadd (ship-msg space ship (string-append "regen " amtstr "%")) #f))))
         ((cannon)
          (define t (ship-tool ship 'cannon))
          (when t
            (append! changes
                     (chstat (ob-id ship) 'toolval
                             (list 'cannon (* amount (tool-val t))))
                     (chadd (ship-msg space ship (string-append "cannon " amtstr "%")) #f))))
         ((missile)
          (define t (ship-tool ship 'missile))
          (when t
            (append! changes
                     (chstat (ob-id ship) 'toolval
                             (list 'missile (list (car (tool-val t)) (* amount (cadr (tool-val t))))))
                     (chadd (ship-msg space ship (string-append "missile " amtstr "%")) #f))))
         ((pbolt)
          (define t (ship-tool ship 'pbolt))
          (when t
            (append! changes
                     (chstat (ob-id ship) 'toolval
                             (list 'pbolt (* amount (tool-val t))))
                     (chadd (ship-msg space ship (string-append "plasma " amtstr "%")) #f))))
         ((engine)
          (define t (ship-tool ship 'engine))
          (when t
            (append! changes
                     (chstat (ob-id ship) 'toolval
                             (list 'engine (* amount (tool-val t))))
                     (chadd (ship-msg space ship (string-append "engine " amtstr "%")) #f))))
         ((turning)
          (for ((tname '(turnleft turnright steer)))
            (define t (ship-tool ship tname))
            (when t
              (append! changes
                       (chstat (ob-id ship) 'toolval (list tname (* amount (tool-val t)))))))
          (when (not (null? changes))
            (append! changes
                     (chadd (ship-msg space ship (string-append "turning " amtstr "%")) #f))))
         ((hull)
          (append! changes
                   (chstat (ob-id ship) 'hull (* amount (ship-maxcon ship)))
                   (chadd (ship-msg space ship (string-append "hull " amtstr "%")) #f)))
         ((radar)
          (append! changes
                   (chstat (ob-id ship) 'radar (* amount (ship-radar ship)))
                   (chadd (ship-msg space ship (string-append "radar " amtstr "%")) #f))))
       (when (null? changes)
         (loop (filter (lambda (x) (not (equal? x t)))
                       types))))))
  changes)