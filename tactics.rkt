#lang racket/base

(require racket/class
         racket/math
         racket/list)

(require "defs.rkt"
         "utils.rkt"
         "draw-utils.rkt"
         "draw.rkt")

(provide (all-defined-out))

(define SHIELD_COST 10.0)

;; server

; return a list of changes
(define (tactics-ai! space dt stack)
  (define changes '())
  (define ownship (get-ship stack))
  (when (and (ship-flying? ownship) ((pod-energy (get-pod stack)) . > . SHIELD_COST))
    (define role (get-role stack))
    (define newrole (copy role))
    (define np (nearest-incoming-plasma space ownship))
    (when np
      (define me (pod-obj (get-pod stack) ownship))
      (define t (target-angle me me np np SHIELD_SPEED))
      
      (when t
        (define pod (get-pod stack))
        (define podangle (angle-add (posvel-r (obj-posvel ownship)) (pod-facing pod)))
        (define offset (angle-diff podangle t))
        (when ((abs offset) . < . (/ (pod-spread pod) 2))
          (define chance-per-sec (/ (pod-energy pod) MAX_POD_ENERGY))
          (set! chance-per-sec (expt chance-per-sec 0.7))
          (define chance-per-tick (- 1.0 (expt (- 1.0 chance-per-sec) dt)))
          (when ((random) . < . chance-per-tick)
            (set-tactics-shield! newrole t)
            (set! changes (list newrole)))))))
  changes)

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
                               (+ (* SHIELD_SPEED (cos a)) (posvel-dx ps) rvx)
                               (+ (* SHIELD_SPEED (sin a)) (posvel-dy ps) rvy)
                               0)
                       20.0 15.0))
     (list (chadd s) (cherg (ob-id pod) (- SHIELD_COST))))
    (else
     (error "update-tactics hit ELSE clause"))))


;; client

(define (click-tactics x y button stack)
  (cond
    ((and (ship-flying? (get-ship stack))
          ((pod-energy (get-pod stack)) . > . SHIELD_COST))
     ; we are firing
     (define fangle (angle-norm (atan0 y x)))
     
     (define ship (get-ship stack))
     (define pod (get-pod stack))
     (define podangle (angle-add (posvel-r (obj-posvel ship)) (pod-facing pod)))
     (define offset (angle-diff podangle fangle))
     (if ((abs offset) . < . (/ (pod-spread pod) 2))
       (struct-copy tactics (get-role stack) (shield fangle))
       #f))
    (else #f)))


(define (draw-tactics dc stack)
  (define ship (get-ship stack))
  (define spv (obj-posvel ship))
  (define space (get-space stack))
  (define t (get-pod stack))
  
  (draw-view dc (get-center stack) space)
  (draw-hud dc ship t)
  
  (when (ship-flying? ship)
    (keep-transform dc
      (send dc rotate (- (posvel-r spv)))
      (define line-size 50)
      (send dc set-pen "red" 1 'solid)
      (for ((a (list (+ (pod-facing t) (/ (pod-spread t) 2))
                     (- (pod-facing t) (/ (pod-spread t) 2)))))
        (send dc draw-line 0 0 (* line-size (cos a)) (* line-size (sin a))))))
  
  
  (list leave-button))
