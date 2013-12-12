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
    (for/list ((o (ship-hangar ship))
               (i (in-naturals)))
      (keep-transform dc
        (define angle (* i (/ 2pi (length (ship-hangar ship)))))
        (send dc rotate angle)
        (send dc translate 0 100)
        (send dc rotate (- angle))
        
        (cond
          ((weapon-pod? o)
           (send dc set-pen "black" 1.0 'solid)
           (send dc draw-ellipse -20 -20 40 40)
           (define-values (x y) (dc->canon canvas dc -10 -10))
           ;(printf "x,y ~a,~a\n" x y)
           
           (button x y 20 20 2 2 (obj-id o) "W"))
          ))))
  
  (define ship-roles
    (find-all (get-ship stack)
              (lambda (o) (or (multirole? o)
                              (and (role? o) (not (role-player o)))))))
  (append
   (list leave-button)
   hangar-buttons
   (for/list ((r ship-roles)
              (i (in-naturals)))
     (cond
       ((role? r)
        (button (+ (/ (- WIDTH) 2) 100 (* i 100)) (+ (/ (- HEIGHT) 2) 60) 100 30 5 5 (obj-id r)
                (format "~a" (role-name r))))
       ((multirole? r)
        (define role (multirole-role r))
        (button (+ (/ (- WIDTH) 2) 100 (* i 100)) (+ (/ (- HEIGHT) 2) 60) 100 30 5 5 (obj-id r)
                (format "~a" (role-name role))))))))

