#lang racket/base

(require racket/class
         racket/math
         racket/list)

(require "defs.rkt"
         "utils.rkt"
         "draw-utils.rkt"
         "draw.rkt")

(provide (all-defined-out))


;; server

; return a list of changes
(define (tactics-ai! space stack)
  (define changes '())
  (define ownship (get-ship stack))
  (when (ship-flying? ownship)
    (define role (get-role stack))
    (define newrole (copy-role role))
    (define np (nearest-incoming-plasma space ownship))
    (when np
      (define me (pod-obj (get-pod stack) ownship))
      (define t (target-angle me me np np SHIELD_SPEED))
      
      ;(printf "incoming plasma at t ~a\n" t)
      
      (when (and t ((random) . > . 0.96))
        (set-tactics-shield! newrole t)
        (set! changes (list newrole)))))
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
     (list (chadd s) (cherg (ob-id pod) -10.0)))
    (else
     (error "update-tactics hit ELSE clause"))))


;; client

(define (click-tactics x y button stack)
  (cond
    ((and (ship-flying? (get-ship stack))
          ((pod-energy (get-pod stack)) . > . 10.0))
     ; we are firing
     (define fangle (angle-norm (atan y x)))
     
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
