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
           (send dc draw-text "Weapon Pod" 0 0)
           (send dc scale 1 -1)
           
           (cond ((role-player (weapon-pod-role o))
                  (send dc scale 1 -1)
                  (send dc draw-text (player-name (role-player (weapon-pod-role o))) 120 0)
                  #f)
                 (else
                  (define-values (x y) (dc->canon canvas dc 120 -25))
                  (button x y 65 30 5 5 (obj-id (weapon-pod-role o)) "Deploy")
                  )))))))
  
  (define helm-button
    (cond ((role-player (ship-helm ship))
           (send dc scale 1 -1)
           (send dc draw-text (format "Helm ~a" (player-name (role-player (ship-helm ship))))
                 -50 -225)
           (send dc scale 1 -1)
           #f)
          (else
           (button -50 200 100 30 5 5 (obj-id (ship-helm ship)) "Helm"))))
  
  (define obs-button
    (button -50 150 100 30 5 5 (obj-id (ship-observers ship)) "Observe"))
  
  (filter values (append
                  (list leave-button helm-button obs-button)
                  hangar-buttons)))

