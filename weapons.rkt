#lang racket/base

(require "defs.rkt"
         "draw.rkt")

(provide (all-defined-out))


; client
(define (click-weapons x y button stack)
  (define role (get-role stack))
  (define pod (get-pod stack))
  (define ship (get-ship stack))
  (cond
    ((not (pod-deploying? pod))
     ; we are selecting a new angle to deploy to
     (printf "ship ~a, atan ~a\n" (posvel-r (obj-posvel ship)) (atan y x))
     (define angle (- (atan y x) (posvel-r (obj-posvel ship))))
     (when (angle . < . 0)
       (set! angle (+ angle 2pi)))
     (define cmd (struct-copy weapons role))
     (set-podrole-angle! cmd angle)
     cmd)
    (else
     (error "click-weapons hit ELSE clause"))))


; server
(define (command-weapons cmd space stack)
  (define role (get-role stack))
  (define pod (get-pod stack))
  (set-podrole-angle! role (podrole-angle cmd)))
;  (when (
;    (when (helm-aft cmd)
;      (set-helm-aft! cmd #f)
;      
;      (define ownship (cadr stack))
;      (define pv (obj-posvel ownship))
;      (define rx (cos (posvel-r pv)))
;      (define ry (sin (posvel-r pv)))
;      (define x (+ (posvel-x pv) (* 20 rx)))
;      (define y (+ (posvel-y pv) (* 20 ry)))
;      (define p (plasma (next-id) (space-time ownspace)
;                        (posvel x y 0
;                                (+ (posvel-dx pv) (* 30 rx))
;                                (+ (posvel-dy pv) (* 30 ry))
;                                0)
;                        "blue" 10.0 (obj-id ownship) '()))
;      (set-space-objects! ownspace (list* p (space-objects ownspace)))))


; client
(define (draw-weapons dc ownspace stack)
  (draw-observer dc ownspace stack)
  (list leave-button))
