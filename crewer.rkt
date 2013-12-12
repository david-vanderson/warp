#lang racket/base

(require racket/class)

(require "defs.rkt"
         "draw.rkt")

(provide (all-defined-out))


(define (click-crewer x y button role ownspace me)
  (when button
    (define mr (find-id ownspace button))
    (role-change me (if role (obj-id role) #f) (obj-id mr))))


(define (draw-crewer canvas dc ownspace stack)
  (define ship (get-ship stack))
  (keep-transform dc
    (send dc scale 18 18)
    (send dc set-pen "black" (/ 1.0 (dc-scale dc)) 'solid)
    (send dc draw-polygon ship-internal))
  
  (define hangar-buttons
    (for/list ((o (ship-pods ship))
               (i (in-naturals)))
      (keep-transform dc
        (send dc translate -100 (- 100 (* i 50)))
        (cond
          ((weapon-pod? o)
           (send dc set-pen "black" 1.0 'solid)
           (send dc scale 1 -1)
           (send dc draw-text (format "Weapon Pod ~a" (add1 i)) 65 -15)
           (send dc scale 1 -1)
           ;(send dc draw-ellipse -20 -20 40 40)
           (define-values (x y) (dc->canon canvas dc -10 -10))
           (button x y 65 30 5 5 (obj-id (weapon-pod-role o)) "Deploy")
           )
          ))))
  
  (define helm-button
    (button -50 200 100 30 5 5 (obj-id (ship-helm ship)) "Helm"))
  
  (define obs-button
    (button -50 150 100 30 5 5 (obj-id (ship-observers ship)) "Observe"))
  
  (append
   (list leave-button helm-button obs-button)
   hangar-buttons))

