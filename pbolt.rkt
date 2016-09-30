#lang racket/base

(require "defs.rkt"
         "utils.rkt"
         "draw-utils.rkt")

(provide (all-defined-out))


(define (dmg-pbolt tool)
  (cond
    ((null? (tool-dmgs tool))
     (list (chadd (dmg -1 "offline" 10 0 #f) (ob-id tool))))
    (else
     #f)))


;; client/server

(define (change-pbolt! cmd space stack who)
  (define tool (car stack))
  (define a cmd)  ; cmd is angle to fire
  (define ship (get-ship stack))
  (define pod (get-pod stack))
  (cond
    ((not (ship-flying? ship))
     (printf "~a discarding message (not flying) ~v\n" who cmd)
     (values #f '()))
    ((not ((pod-energy pod) . > . (pbolt-plasma-size tool)))
     (printf "~a discarding message (not enough energy) ~v\n" who cmd)
     (values #f '()))
    (else
     (define po (pod-obj pod ship))
     
     (define plas (plasma -1 (space-time space)
                          (posvel (space-time space) (obj-x po) (obj-y po) (obj-r po)
                                  (+ (* PLASMA_SPEED (cos a)) (posvel-dx (obj-posvel po)))
                                  (+ (* PLASMA_SPEED (sin a)) (posvel-dy (obj-posvel po)))
                                  0)
                          (pbolt-plasma-size tool) (ob-id ship)))
     
     (values #f (list (chadd plas #f) (cherg (ob-id pod) (- (pbolt-plasma-size tool))))))))


; return list of buttons
(define (draw-pbolt-ui! dc t stack send-commands)
  (define buttons '())
  (define ship (get-ship stack))
  (define pod (get-pod stack))
  (define b (button 'disabled #\space (+ LEFT 40) (+ BOTTOM 10) 50 50 "Fire [_]" #f))
  (when (and (ship-flying? ship) ((pod-energy pod) . > . (pbolt-plasma-size t)))
    (define a (+ (obj-r ship) (pod-facing (get-pod stack))))
    (set-button-f! b (lambda (x y) (send-commands (command (ob-id t) a))))
    (set-button-draw! b 'normal))
  (append! buttons (list b))
  (define ob (add-offline-button! t b send-commands))
  (when ob (append! buttons (list ob)))
  buttons)


; server

; return a list of changes
(define (pbolt-ai! space stack)
  (define changes '())
  (define ownship (get-ship stack))
  (define pod (get-pod stack))
  (define pb (findf pbolt? (pod-tools pod)))
  (when (and (ship-flying? ownship)
             ((pod-energy pod) . > . (pbolt-plasma-size pb)))
    
    (define ne (nearest-enemy space ownship))
  
    (when (and ne ((distance ownship ne) . < . 500))
      (define me (pod-obj pod ownship))
      (define t (target-angle me me ne ne PLASMA_SPEED))
      (when t
        (define podangle (angle-add (obj-r ownship) (pod-facing pod)))
        (define offset (angle-diff podangle t))
        (when ((abs offset) . < . (/ (pod-spread pod) 2))
          (define chance-per-sec (/ (pod-energy pod) (pod-maxe pod)))
          ;(printf "t = ~a\n" chance-per-sec)
          (when ((random) . < . chance-per-sec)
            (set! changes (list (command (ob-id pb) t))))))))
  changes)