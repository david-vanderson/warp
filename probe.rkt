#lang racket/base

(require racket/class
         racket/math
         mode-lambda
         racket/draw)

(require "defs.rkt"
         "utils.rkt"
         "ships.rkt"
         "draw-utils.rkt")

(provide (all-defined-out))


(define (dmg-ptube tool)
  (cond
    ((null? (tool-dmgs tool))
     (if ((random) . < . 0.5)
         (list (chadd (dmg (next-id) "offline" DMG_SIZE 0 DMG_FIX?) (ob-id tool)))
         (list (chadd (dmg (next-id) "nofire" DMG_SIZE 0 DMG_FIX?) (ob-id tool)))))
    (((length (tool-dmgs tool)) . < . 2)
     (if (equal? "offline" (dmg-type (car (tool-dmgs tool))))
         (list (chadd (dmg (next-id) "nofire" DMG_SIZE 0 DMG_FIX?) (ob-id tool)))
         (list (chadd (dmg (next-id) "offline" DMG_SIZE 0 DMG_FIX?) (ob-id tool)))))
    (else
     #f)))


;; client/server

(define (launch-probe! cmd space stack who)
  (define ship (get-ship stack))
  (define tool (ship-tool ship 'probe))
  (cond
    ((not (ship-flying? ship))
     (printf "~a discarding message (not flying) ~v\n" who cmd)
     (values #f '()))
    ((not tool)
     (printf "~a discarding message (no probe tool) ~v\n" who cmd)
     (values #f '()))
    (else
     (define changes '())
     (when (server?)
       (define a (angle-add (obj-r ship) pi))
       (define p (make-ship "probe" "Probe" (ship-faction ship)
                            #:start-time (space-time space)
                            #:life (tool-val tool)))
       (define d (+ (ship-radius ship) (* 2.0 (ship-radius p))
                    (random-between 0 1)))  ; random so things don't end up exactly aligned
       (set-obj-posvel! p (posvel (space-time space)
                                  (+ (obj-x ship) (* d (cos a)))
                                  (+ (obj-y ship) (* d (sin a)))
                                  a
                                  (obj-dx ship)
                                  (obj-dy ship)
                                  0))
       
       (append! changes (chadd p #f) (chrc (ob-id (car stack)) (ob-id p))))
     
     (values #f changes))))
     