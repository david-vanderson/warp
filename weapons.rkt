#lang racket/base

(require racket/class
         racket/math)

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
    ((pod-deployed? pod)
     ; we are firing, need the angle
     (define fangle (atan y x))
     (when (fangle . < . 0)
       (set! fangle (+ fangle 2pi)))
     (struct-copy weapons role (fire fangle)))
    (else
     (error "click-weapons hit ELSE clause"))))


; server
(define (command-weapons cmd space stack)
  (define role (get-role stack))
  (define pod (get-pod stack))
  (define ship (get-ship stack))
  (cond
    ((not (pod-deploying? pod))
     ; we are selecting a new angle to deploy to
     (set-podrole-angle! role (podrole-angle cmd)))
    ((pod-deployed? pod)
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
                       (posvel x y 0
                               (+ (* 60 (cos a)) (posvel-dx ps) rvx)
                               (+ (* 60 (sin a)) (posvel-dy ps) rvy)
                               0)
                       "blue" 10.0 #f #;(obj-id ship) '()))
     (set-space-objects! space (cons p (space-objects space))))
    (else
     (error "click-weapons hit ELSE clause"))))


; client
(define (draw-weapons dc stack)
  (define role (get-role stack))
  (define pod (get-pod stack))
  (define ship (get-ship stack))
  (define space (get-space stack))
  
  (define center (obj #f #f (posvel
                             (+ (posvel-x (obj-posvel ship))
                                (* (pod-dist pod) (cos (+ (posvel-r (obj-posvel ship))
                                                          (pod-angle pod)))))
                             (+ (posvel-y (obj-posvel ship))
                                (* (pod-dist pod) (sin (+ (posvel-r (obj-posvel ship))
                                                          (pod-angle pod)))))
                             0 0 0 0)))
                                    
  
  (draw-background dc space center)
  ; draw other ships/objects
  (for ((o (space-objects space))
        #:when (not (= (obj-id o) (obj-id ship))))
    (draw-object dc o center space))
  
  (keep-transform dc
    (define-values (x y) (recenter center (posvel-x (obj-posvel ship)) (posvel-y (obj-posvel ship))))
    (send dc translate x y)
    (send dc rotate (- (posvel-r (obj-posvel ship))))
    
    ; draw other pods on my ship
    (for ((p (ship-pods ship))
          #:when (not (= (obj-id p) (obj-id pod))))
      (draw-pod dc p))
    
    ; draw my pod
    (draw-pod dc pod)
    
    ; draw my ship and shields
    (keep-transform dc
      (send dc rotate (/ pi 2))
      (send dc set-pen fgcolor 1 'solid)
      (send dc draw-polygon ship-external))
    (for ((shield (ship-shields ship)))
      (draw-shield dc shield)))
  
  (list leave-button))
