#lang racket/base

(require "defs.rkt"
         "utils.rkt"
         "draw-utils.rkt"
         "plasma.rkt")

(provide (all-defined-out))


(define (dmg-pbolt tool)
  (cond
    ((null? (tool-dmgs tool))
     (list (chadd (dmg (next-id) "offline" DMG_SIZE 0 DMG_FIX?) (ob-id tool))))
    (else
     #f)))


(define (pbolt-frac last-time now)
  (define a (- 1.0 (linear-fade (- now last-time) 0 1000)))
  (* a a))


;; client/server

(define (change-pbolt! cmd space stack args who)
  (define ship (get-ship stack))
  (define tool (ship-tool ship 'pbolt))
  (cond
    ((not (ship-flying? ship))
     (printf "~a discarding message (not flying) ~v\n" who cmd)
     (values #f '()))
    (else
     (define ship-a (car args))
     (define a (cadr args))
     (define frac (caddr args))
     (define e (clamp 1.0 (tool-val tool) (* frac (tool-val tool))))
     (define d (+ (ship-radius ship) (plasma-energy->radius e) 0.1))
     (define plas
       (plasma (next-id) (space-time space) #t 1.0
               (posvel (space-time space)
                       (+ (obj-x ship) (* d (cos ship-a)))
                       (+ (obj-y ship) (* d (sin ship-a)))
                       (obj-r ship)
                       (+ (* PLASMA_SPEED (cos a)) (obj-dx ship))
                       (+ (* PLASMA_SPEED (sin a)) (obj-dy ship))
                       0)
               e))
     
     (values #f (list (chadd plas #f))))))


; server

(define (filterf? o)
  (or (spaceship? o) (missile? o) (probe? o) (cannonball? o) (mine? o)))

; return a list of changes
(define (pbolt-ai! qt stack)
  (define changes '())
  (define ownship (get-ship stack))
  (define pb (ship-tool ownship 'pbolt))
  (define d (+ (ship-radius ownship) (plasma-energy->radius (tool-val pb))))
  (when (and (ship-flying? ownship)
             (tool-online? pb))
    (define ne (nearest-enemy qt ownship filterf?))
    (when (and ne ((distance ownship ne) . < . 300.0))
      (define r (theta ownship ne))
      (define xy (pvobj (+ (obj-x ownship) (* d (cos r)))
                        (+ (obj-y ownship) (* d (sin r)))))
      (define t (target-angle xy ownship ne ne
                              PLASMA_SPEED (+ (/ PLASMA_LIFE 1000.0)
                                              (/ (tool-val pb) PLASMA_FADE))))
      (when t
        (append! changes (list (command (ob-id ownship) #f 'pbolt (list r t 1.0)))))))
  changes)