#lang racket/base

(require "defs.rkt"
         "utils.rkt")

(provide (all-defined-out))


(define (pbolt-dmg! tool)
  (list (chadd (dmg -1 "offline" 10 0 #t) (ob-id tool))))


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
     
     (define plas (plasma (next-id) (space-time space)
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
  (define offline (findf (lambda (d) (equal? "offline" (dmg-type d))) (tool-dmgs t)))
  (when offline
    (define ob (dmgbutton 'dmg #f (+ LEFT 90) (+ BOTTOM 10) 50 50 "Offline"
                          (lambda (x y) (send-commands (command (ob-id offline)
                                                                (not (dmg-fixing? offline)))))
                          (/ (dmg-energy offline) (dmg-size offline)) (dmg-fixing? offline)))
    (append! buttons (list ob)))
    
  (define b (button 'normal #\space (+ LEFT 40) (+ BOTTOM 10) 50 50 "Fire [_]" #f))
  (cond
    ((and (ship-flying? ship)
          ((pod-energy pod) . > . (pbolt-plasma-size t))
          (not offline))
     (define a (+ (obj-r ship) (pod-facing (get-pod stack))))
     (set-button-f! b (lambda (x y) (send-commands (command (ob-id t) a)))))
    (else
     (set-button-draw! b 'disabled)))
  (append! buttons (list b))
  
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
          (when ((random) . < . chance-per-sec)
            (set! changes (list (command (ob-id pb) t))))))))
  changes)