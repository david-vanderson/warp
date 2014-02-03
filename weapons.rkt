#lang racket/base

(require racket/class
         racket/math)

(require "defs.rkt"
         "utils.rkt"
         "draw-utils.rkt"
         "draw.rkt")

(provide (all-defined-out))


;; server

; return a list of changes
(define (weapons-ai! space stack)
  (define changes '())
  (define ownship (get-ship stack))
  (when (ship-flying? ownship)
    (define w (get-role stack))
    (define neww (copy-role w))
    (define ne (nearest-enemy space ownship))
  
    (when ne
      (define me (pod-obj (get-pod stack) ownship))
      (define t (target-angle me me ne ne PLASMA_SPEED))
      
      (when (and t ((random) . > . 0.97))
        (set-weapons-fire! neww t)
        (set! changes (list neww)))))
  changes)


;; client/server

(define (change-weapons cmd space stack)
  (define role (get-role stack))
  (define pod (get-pod stack))
  (define ship (get-ship stack))
  (cond
    ((weapons-fire cmd)
     ; we are firing
     (define ps (obj-posvel ship))
     (define-values (px py podangle) (pod-xyr pod ship))
     (define a (weapons-fire cmd))
     (define x (+ px (* 5 (cos a))))
     (define y (+ py (* 5 (sin a))))
     
     ; add rotational velocity of pod
     (define rvx (* -1 (* (pod-dist pod) (posvel-dr ps)) (sin podangle)))
     (define rvy (* 1 (* (pod-dist pod) (posvel-dr ps)) (cos podangle)))
     
     (define p (plasma (next-id) (space-time space)
                       (posvel (space-time space) x y 0
                               (+ (* PLASMA_SPEED (cos a)) (posvel-dx ps) rvx)
                               (+ (* PLASMA_SPEED (sin a)) (posvel-dy ps) rvy)
                               0)
                       10.0 (ob-id ship)))
     (list (chadd p)))
    (else
     (error "command-weapons hit ELSE clause"))))


;; client

(define (click-weapons x y button stack)
  (cond
    ((ship-flying? (get-ship stack))
     (define role (get-role stack))
     ; we are firing, need the angle
     (define fangle (angle-norm (atan y x)))
     (struct-copy weapons role (fire fangle)))
    (else #f)))
        

(define (draw-weapons dc stack)
  (define ship (get-ship stack))
  (define spv (obj-posvel ship))
  (define space (get-space stack))
  (define center (get-center stack))
  (define w (get-pod stack))
  
  (draw-view dc center space)
  
  ; draw my hud
  (when (ship-flying? ship)
    (keep-transform dc
      (send dc rotate (- (posvel-r spv)))
      (define line-size 50)
      (send dc set-pen "red" 1 'solid)
      (for ((a (list (+ (pod-facing w) (/ (pod-spread w) 2))
                     (- (pod-facing w) (/ (pod-spread w) 2)))))
        (send dc draw-line 0 0 (* line-size (cos a)) (* line-size (sin a))))))
  
  (list leave-button))
