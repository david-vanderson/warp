#lang racket/base

(require "defs.rkt"
         "utils.rkt"
         "draw-utils.rkt")

(provide (all-defined-out))


(define (dmg-pbolt tool)
  (cond
    ((null? (tool-dmgs tool))
     (list (chadd (dmg (next-id) "offline" DMG_SIZE 0 DMG_FIX?) (ob-id tool))))
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
     
     (define plas (plasma (next-id) (space-time space)
                          (posvel (space-time space) (obj-x po) (obj-y po) (obj-r po)
                                  (+ (* PLASMA_SPEED (cos a)) (posvel-dx (obj-posvel po)))
                                  (+ (* PLASMA_SPEED (sin a)) (posvel-dy (obj-posvel po)))
                                  0)
                          (pbolt-plasma-size tool) (ob-id ship)))
     
     (values #f (list (chadd plas #f) (cherg (ob-id pod) (- (pbolt-plasma-size tool))))))))


; return list of buttons
(define (draw-pbolt-ui! t stack send-commands)
  (define buttons '())
  (define ship (get-ship stack))
  (define pod (get-pod stack))
  (define b (button 'disabled #\space (+ LEFT 65) (- BOTTOM 35) 50 50 "Fire [_]" #f))
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
  (define chance-per-sec (/ (pod-e pod) (pod-maxe pod) 2.0))
  (when (and (ship-flying? ownship)
             (tool-online? pb)
             ((pod-e pod) . > . (pbolt-plasma-size pb))
             ((random) . < . chance-per-sec))

    (define me (pod-obj pod ownship))
    (define podangle (angle-add (obj-r ownship) (pod-facing pod)))

    (for/first ((o (in-list (space-objects space)))
                #:when (and (or (and (spaceship? o)
                                     ((ship-con o) . > . 0)
                                     (not (equal? (ship-faction o) (ship-faction ownship))))
                                (and (missile? o)
                                     (not (equal? (missile-faction o) (ship-faction ownship)))))
                            ((distance ownship o) . < . 400)))

      
      (define t (target-angle me me o o PLASMA_SPEED))
      #:break (not t)
      (define offset (angle-frto podangle t))
      #:break (not ((abs offset) . < . (/ (pod-spread pod) 2)))
      (append! changes (list (command (ob-id pb) t)))))
  changes)