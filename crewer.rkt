#lang racket/base

(require racket/class
         racket/math)

(require "defs.rkt"
         "utils.rkt"
         "draw-utils.rkt"
         "draw.rkt")

(provide (all-defined-out))


; client
(define (click-crewer x y button role ownspace me)
  (cond (button
         (define mr (find-id ownspace button))
         (role-change me (if role (obj-id role) #f) (obj-id mr)))
        (else #f)))
  

; client
(define (draw-crewer canvas dc ownspace stack)
  (define orig (send dc get-transformation))
  (define ship (get-ship stack))
  (define buttons (list leave-button))
  
  (send dc set-pen fgcolor 1.0 'solid)
  (send dc set-brush nocolor 'transparent)
  
  ; reversed because we are rotating by pi/2
  (define scale (* 0.9 (min (/ HEIGHT (send ship-bitmap get-width))
                            (/ WIDTH (send ship-bitmap get-height)))))
  (keep-transform dc
    (send dc scale scale scale)
    (send dc rotate (- (/ pi 2)))
    (send dc draw-bitmap
          ship-bitmap
          (- (/ (send ship-bitmap get-width) 2))
          (- (/ (send ship-bitmap get-height) 2))))
    
  (for ((p (ship-pods ship)))
    (keep-transform dc
      (define r (angle-add (/ pi 2) (pod-angle p)))
      (send dc translate (* scale (pod-dist p) (cos r)) (* scale (pod-dist p) (sin r)))
      (send dc draw-ellipse -5 -5 10 10)
      
      (cond
        ((multipod? p)
         (define mr (multipod-multirole p))
         (send dc scale 1 -1)
         (send dc draw-text (format "~a" (role-name (multirole-role mr))) 0 10)
         (define-values (x y) (dc->canon canvas dc 0 65))
         (set! buttons (cons (button x y 65 30 5 5 (obj-id mr) "Deploy") buttons))
         (for ((r (multirole-roles mr))
               (i (in-naturals)))
           (send dc draw-text (player-name (role-player r)) 0 (+ 70 (* i 20)))))
        (else
         (define role (pod-role p))
         (send dc scale 1 -1)
         (send dc draw-text (format "~a" (role-name role)) 0 10)
         
         (cond
           ((role-player role)
            (send dc draw-text (player-name (role-player role)) 0 35))
           (else
            (define-values (x y) (dc->canon canvas dc 0 65))
            (set! buttons (cons (button x y 65 30 5 5 (obj-id role) "Deploy") buttons)))))))) 
  buttons)

