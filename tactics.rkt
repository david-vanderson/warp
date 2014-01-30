#lang racket/base

(require racket/class
         racket/math
         racket/list)

(require "defs.rkt"
         "utils.rkt"
         "draw-utils.rkt"
         "draw.rkt")

(provide (all-defined-out))

;; client/server

(define (change-tactics c space stack)
  (define role (get-role stack))
  (define pod (get-pod stack))
  (define ship (get-ship stack))
  (cond
    ((tactics-shield c)
     ; we are firing
     (define ps (obj-posvel ship))
     (define podangle (+ (posvel-r ps) (pod-angle pod)))
     (define px (+ (posvel-x ps) (* (pod-dist pod) (cos podangle))))
     (define py (+ (posvel-y ps) (* (pod-dist pod) (sin podangle))))
     (define a (tactics-shield c))
     (define x (+ px (* 5 (cos a))))
     (define y (+ py (* 5 (sin a))))
     
     ; add rotational velocity of pod
     (define rvx (* -1 (* (pod-dist pod) (posvel-dr ps)) (sin podangle)))
     (define rvy (* 1 (* (pod-dist pod) (posvel-dr ps)) (cos podangle)))
     
     (define s (shield (next-id) (space-time space)
                       (posvel (space-time space) x y a
                               (+ (* 40 (cos a)) (posvel-dx ps) rvx)
                               (+ (* 40 (sin a)) (posvel-dy ps) rvy)
                               0)
                       20.0 15.0))
     (list (chadd s)))
    (else
     (error "update-tactics hit ELSE clause"))))


;; client

(define (click-tactics x y button stack)
  (cond
    ((ship-flying? (get-ship stack))
     (define role (get-role stack))
     ; we are firing, need the angle
     (define fangle (angle-norm (atan y x)))
     (struct-copy tactics role (shield fangle)))
    (else #f)))


(define (draw-tactics dc stack)
  (define ship (get-ship stack))
  (define spv (obj-posvel ship))
  (define space (get-space stack))
  (define center (get-center stack))
  (define t (get-pod stack))
  
  (draw-view dc center space)
  
  ; draw my hud
  (when (ship-flying? ship)
    (keep-transform dc
      (send dc rotate (- (posvel-r spv)))
      (define line-size 50)
      (send dc set-pen "red" 1 'solid)
      (for ((a (list (+ (pod-facing t) (/ (pod-spread t) 2))
                     (- (pod-facing t) (/ (pod-spread t) 2)))))
        (send dc draw-line 0 0 (* line-size (cos a)) (* line-size (sin a))))))
  
  
  (list leave-button))
