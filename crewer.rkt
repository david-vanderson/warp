#lang racket/base

(require racket/class
         racket/math
         racket/draw)

(require "defs.rkt"
         "utils.rkt"
         "draw-utils.rkt"
         "draw.rkt"
         "ships.rkt")

(provide (all-defined-out))


; client
(define (click-crewer x y button stack)
  (cond (button
         (role-change (car stack) (ob-id (get-role stack)) button -1))
        (else #f)))
  

; client
(define (draw-crewer canvas dc ownspace stack)
  (define role (get-role stack))
  (define ship (get-ship stack))
  (define buttons (list leave-button))
  
  (define ship-bitmap (get-ship-bitmap ship))
  
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
  
  (cond
    ((hangar? role)
     ; draw hangar background
     (send dc set-pen fgcolor 1.0 'solid)
     (send dc set-brush (make-color 0 0 0 .9) 'solid)
     (define size (* scale (min (send ship-bitmap get-width) (send ship-bitmap get-height))))
     (send dc draw-rectangle (* -0.5 size) (* -0.5 size) size size)
     
     ; draw all the ships in the hangar
     (define shipmax 54)
     (define pod (get-pod stack))
     (for ((s (hangarpod-ships pod))
           (i (in-naturals)))
       (keep-transform dc
         (send dc translate (+ (* -0.5 size) 10 (/ shipmax 2)) (+ (* 0.5 size) -10 (* i -100) (/ shipmax -2)))
         (keep-transform dc
           (send dc rotate (- (/ pi 2)))
           (define ship-bitmap (get-ship-bitmap s))
           (send dc draw-bitmap
                 ship-bitmap
                 (- (/ (send ship-bitmap get-width) 2))
                 (- (/ (send ship-bitmap get-height) 2))))
         
         (send dc translate (+ 10 (/ shipmax 2)) (/ shipmax 2))
         (send dc scale 1 -1)
         (send dc draw-text (format "~a" (ship-name s)) 0 5)
         (define-values (x y) (dc->canon canvas dc 0 60))
         (set! buttons (cons (button x y 65 30 5 5 (ob-id (ship-crew s)) "Board") buttons))
         (for ((p (find-all s player?))
               (i (in-naturals)))
           (send dc draw-text (player-name p) 0 (+ 70 (* i 20))))
         )))
    (else
     ; draw all the pods on the ship
     (send dc set-pen fgcolor 1.0 'solid)
     (send dc set-brush nocolor 'transparent)
     
     (for ((p (ship-pods ship)))
       (keep-transform dc
         (define r (angle-add (/ pi 2) (pod-angle p)))
         (send dc translate (* scale (pod-dist p) (cos r)) (* scale (pod-dist p) (sin r)))
         (send dc draw-ellipse -5 -5 10 10)
         
         (send dc scale 1 -1)
         (send dc draw-text (format "~a" (role-name (pod-role p))) 0 10)
         (define-values (x y) (dc->canon canvas dc 0 65))
         (define deploy (button x y 65 30 5 5 (ob-id (if (multipod? p) p (pod-role p))) "Deploy"))
         
         (cond
           ((multipod? p)
            (set! buttons (cons deploy buttons))
            (for ((r (multipod-roles p))
                  (i (in-naturals)))
              (send dc draw-text (player-name (role-player r)) 0 (+ 70 (* i 20)))))
           (else
            (define r (pod-role p))
            (cond
              ((role-player r)
               (send dc draw-text (player-name (role-player r)) 0 35))
              (else
               (set! buttons (cons deploy buttons))))))))))
  buttons)

