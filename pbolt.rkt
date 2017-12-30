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

(define (change-pbolt! cmd space stack a who)
  (define ship (get-ship stack))
  (define tool (ship-tool ship 'pbolt))
  (cond
    ((not (ship-flying? ship))
     (printf "~a discarding message (not flying) ~v\n" who cmd)
     (values #f '()))
    (else
     (define plas
       (plasma (next-id) (space-time space)
               (posvel (space-time space)
                       (+ (obj-x ship) (* (ship-radius ship) (cos a)))
                       (+ (obj-y ship) (* (ship-radius ship) (sin a)))
                       (obj-r ship)
                       (+ (* PLASMA_SPEED (cos a)) (obj-dx ship))
                       (+ (* PLASMA_SPEED (sin a)) (obj-dy ship))
                       0)
               (tool-val tool) (ob-id ship)))
     
     (values #f (list (chadd plas #f))))))


; server

; return a list of changes
(define (pbolt-ai! space stack)
  (define changes '())
  (define ownship (get-ship stack))
  (define pb (ship-tool ownship 'pbolt))
  (when (and (ship-flying? ownship)
             (tool-online? pb))

    (for/first ((o (in-list (space-objects space)))
                #:when (and (or (spaceship? o) (missile? o) (probe? o))
                            ((faction-check (ship-faction ownship) (ship-faction o)) . < . 0)
                            ((distance ownship o) . < . 400)))
      
      (define t (target-angle ownship ownship o o PLASMA_SPEED))
      #:break (not t)
      (append! changes (list (command (ob-id ownship) 'pbolt t)))))
  changes)