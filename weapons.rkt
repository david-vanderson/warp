#lang racket/base

(require racket/class
         racket/math)

(require "defs.rkt"
         "utils.rkt"
         "draw-utils.rkt"
         "draw.rkt")

(provide (all-defined-out))

;; client/server

(define (update-weapons cmd space stack)
  (define role (get-role stack))
  (define pod (get-pod stack))
  (define ship (get-ship stack))
  (cond
    ((weapons-fire cmd)
     ; we are firing
     (define ps (obj-posvel ship))
     (define podangle (+ (posvel-r ps) (pod-angle pod)))
     (define px (+ (posvel-x ps) (* (pod-dist pod) (cos podangle))))
     (define py (+ (posvel-y ps) (* (pod-dist pod) (sin podangle))))
     (define a (weapons-fire cmd))
     (define x (+ px (* 5 (cos a))))
     (define y (+ py (* 5 (sin a))))
     
     ; add rotational velocity of pod
     (define rvx (* -1 (* (pod-dist pod) (posvel-dr ps)) (sin podangle)))
     (define rvy (* 1 (* (pod-dist pod) (posvel-dr ps)) (cos podangle)))
     
     (define p (plasma (next-id) (space-time space)
                       (posvel (space-time space) x y 0
                               (+ (* 60 (cos a)) (posvel-dx ps) rvx)
                               (+ (* 60 (sin a)) (posvel-dy ps) rvy)
                               0)
                       10.0 (obj-id ship)))
     (set-space-objects! space (cons p (space-objects space)))
     (list (chadd p)))
    (else
     (error "command-weapons hit ELSE clause"))))


;; client

(define (click-weapons x y button stack)
  (define role (get-role stack))
  ; we are firing, need the angle
  (define fangle (angle-norm (atan y x)))
  (struct-copy weapons role (fire fangle)))


(define (draw-weapons dc stack)
  (define ship (get-ship stack))
  (define spv (obj-posvel ship))
  (define space (get-space stack))
  (define center (get-center stack))
  (define w (get-pod stack))
  
  (draw-view dc center space)
  
  ; draw my hud
  (keep-transform dc
    (send dc rotate (- (posvel-r spv)))
    (define line-size 50)
    (send dc set-pen "red" 1 'solid)
    (for ((a (list (+ (pod-facing w) (/ (pod-spread w) 2))
                   (- (pod-facing w) (/ (pod-spread w) 2)))))
      (send dc draw-line 0 0 (* line-size (cos a)) (* line-size (sin a)))))
  
  (list leave-button))
