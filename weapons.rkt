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
(define (weapons-ai! space dt stack)
  (define changes '())
  (define ownship (get-ship stack))
  (when (and (ship-flying? ownship)
             ((pod-energy (get-pod stack)) . > . (weapon-plasma-size (get-pod stack))))
    (define w (get-role stack))
    (define neww (copy w))
    (define ne (nearest-enemy space ownship))
  
    (when (and ne ((distance ownship ne) . < . 500))
      (define me (pod-obj (get-pod stack) ownship))
      (define t (target-angle me me ne ne PLASMA_SPEED))
      (when t
        (define pod (get-pod stack))
        (define podangle (angle-add (posvel-r (obj-posvel ownship)) (pod-facing pod)))
        (define offset (angle-diff podangle t))
        (when ((abs offset) . < . (/ (pod-spread pod) 2))
          (define chance-per-sec (/ (pod-energy pod) MAX_POD_ENERGY))
          (set! chance-per-sec (expt chance-per-sec 0.7))
          (define chance-per-tick (- 1.0 (expt (- 1.0 chance-per-sec) dt)))
          (when ((random) . < . chance-per-tick)
            (set-weapons-fire! neww t)
            (set! changes (list neww)))))))
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
                       (weapon-plasma-size pod) (ob-id ship)))
     (values #f (list (chadd p) (cherg (ob-id pod) (- (weapon-plasma-size pod))))))
    (else
     (error "command-weapons hit ELSE clause"))))


;; client

(define (click-weapons x y button stack)
  (cond
    ((and (ship-flying? (get-ship stack))
          ((pod-energy (get-pod stack)) . > . (weapon-plasma-size (get-pod stack))))
     ; we are firing
     (define fangle (angle-norm (atan0 y x)))
     
     (define ship (get-ship stack))
     (define pod (get-pod stack))
     (define podangle (angle-add (posvel-r (obj-posvel ship)) (pod-facing pod)))
     (define offset (angle-diff podangle fangle))
     (if ((abs offset) . < . (/ (pod-spread pod) 2))
       (struct-copy weapons (get-role stack) (fire fangle))
       '()))
    (else '())))
        

(define (draw-weapons dc stack serverspace)
  (define ship (get-ship stack))
  (define spv (obj-posvel ship))
  (define space (get-space stack))
  (define w (get-pod stack))
  
  (draw-view dc (get-center stack) space)
  (when serverspace (draw-server-objects dc (get-center stack) serverspace))
  (draw-hud dc ship w)
  
  (when (ship-flying? ship)
    (keep-transform dc
      (send dc rotate (- (posvel-r spv)))
      (define line-size 50)
      (send dc set-pen "red" 1 'solid)
      (for ((a (list (+ (pod-facing w) (/ (pod-spread w) 2))
                     (- (pod-facing w) (/ (pod-spread w) 2)))))
        (send dc draw-line 0 0 (* line-size (cos a)) (* line-size (sin a))))))
  
  (list leave-button))
