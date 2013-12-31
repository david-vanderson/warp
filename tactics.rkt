#lang racket/base

(require racket/class
         racket/math
         racket/list)

(require "defs.rkt"
         "draw.rkt")

(provide (all-defined-out))


; client
(define (click-tactics x y button stack)
  (define role (get-role stack))
  ; we are firing, need the angle
  (define fangle (angle-norm (atan y x)))
  (struct-copy tactics role (shield fangle)))


; server
(define (command-tactics cmd space stack)
  (define role (get-role stack))
  (define pod (get-pod stack))
  (define ship (get-ship stack))
  (cond
    ((tactics-shield cmd)
     ; we are firing
     (define ps (obj-posvel ship))
     (define podangle (+ (posvel-r ps) (pod-angle pod)))
     (define px (+ (posvel-x ps) (* (pod-dist pod) (cos podangle))))
     (define py (+ (posvel-y ps) (* (pod-dist pod) (sin podangle))))
     (define a (tactics-shield cmd))
     (define x (+ px (* 5 (cos a))))
     (define y (+ py (* 5 (sin a))))
     
     ; add rotational velocity of pod
     (define rvx (* -1 (* (pod-dist pod) (posvel-dr ps)) (sin podangle)))
     (define rvy (* 1 (* (pod-dist pod) (posvel-dr ps)) (cos podangle)))
     
     (define s (shield (next-id) (space-time space)
                       (posvel x y a
                               (+ (* 40 (cos a)) (posvel-dx ps) rvx)
                               (+ (* 40 (sin a)) (posvel-dy ps) rvy)
                               0)
                       20.0 15.0))
     (set-space-objects! space (cons s (space-objects space)))
     )
    (else
     (error "command-tactics hit ELSE clause"))))


; client
(define (draw-tactics dc stack)
  (define role (get-role stack))
  (define pod (get-pod stack))
  (define ship (get-ship stack))
  (define spv (obj-posvel ship))
  (define space (get-space stack))
  (define center (get-center stack))
  (define t (get-pod stack))
  
  
  (draw-background dc space center)
  ; draw other ships/objects
  (for ((o (space-objects space))
        #:when (not (= (obj-id o) (obj-id ship))))
    (draw-object dc o center space))
  
  (keep-transform dc
    (define-values (x y) (recenter center ship))
    (send dc translate x y)
    (send dc rotate (- (posvel-r spv)))
    (send dc set-pen fgcolor 1 'solid)
    (send dc set-brush nocolor 'transparent)
    
;    ; draw other pods on my ship
;    (for ((p (ship-pods ship))
;          #:when (not (= (obj-id p) (obj-id t))))
;      (draw-pod dc p))
;    
;    ; draw my pod
;    (draw-pod dc t)
    
    ; draw my ship and shields
    (keep-transform dc
      (send dc rotate (/ pi 2))
      (send dc set-pen fgcolor 1 'solid)
      (send dc set-brush nocolor 'transparent)
      (send dc draw-polygon ship-external)))
  
  (define buttons (list leave-button))
  
  ; draw my hud
  (keep-transform dc
    (send dc rotate (- (posvel-r spv)))
    (define line-size 50)
    (send dc set-pen "red" 1 'solid)
    (for ((a (list (+ (pod-facing t) (/ (pod-spread t) 2))
                   (- (pod-facing t) (/ (pod-spread t) 2)))))
      (send dc draw-line 0 0 (* line-size (cos a)) (* line-size (sin a)))))
  
  
  buttons)
