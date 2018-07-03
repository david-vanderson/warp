#lang racket/base

(require racket/math)

(require "defs.rkt"
         "utils.rkt"
         "quadtree.rkt"
         "ships.rkt")

(provide (all-defined-out))

(define CANNON_SPEED 75.0)


; return list of changes
(define (reduce-cannonball! space b damage)
  (define changes '())

  (set-stats-con! (ship-stats b) (- (ship-con b) damage))
  
  (when ((ship-con b) . <= . 0)
    (set-obj-alive?! b #f)
    
    ; explode
    (when (server?)
      (define e (make-explosion space (obj-x b) (obj-y b) 10.0 50.0 100.0 100.0))
      (append! changes (chadd e #f))))
  
  changes)


(define (dmg-cannon tool)
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

(define (fire-cannon! cmd space stack who)
  (define ship (get-ship stack))
  (define tool (ship-tool ship 'cannon))
  (cond
    ((not (ship-flying? ship))
     (printf "~a discarding message (not flying) ~v\n" who cmd)
     (values #f '()))
    ((not tool)
     (printf "~a discarding message (no cannon tool) ~v\n" who cmd)
     (values #f '()))
    (else
     (define changes '())
     (when (server?)
       (define a (command-arg cmd))
       (define b (make-ship "cannonball" "Cannonball" (ship-faction ship)
                            #:ai? (not (player? (car stack)))
                            #:r a
                            #:radar (ship-radar ship)
                            #:start-time (space-time space)
                            #:con (tool-val tool)))
       (define d (+ (ship-radius ship) (ship-radius b) 0.1))
       (define speed CANNON_SPEED)
       (set-obj-posvel! b (posvel (space-time space)
                                  (+ (obj-x ship) (* d (cos a)))
                                  (+ (obj-y ship) (* d (sin a)))
                                  a
                                  (+ (obj-dx ship) (* speed (cos a)))
                                  (+ (obj-dy ship) (* speed (sin a)))
                                  2.0))

       (append! changes (chadd b #f))
       (when (player? (car stack))
         (append! changes (chrc (ob-id (car stack)) (ob-id b)))))
     
     (values #f changes))))


; return a list of changes
(define (cannon-ai! qt stack)
  (define changes '())
  (define ownship (get-ship stack))
  (define c (ship-tool ownship 'cannon))
  (when (and (ship-flying? ownship)
             (tool-online? c))
    (let/ec done
      (for ((o (in-list (qt-retrieve qt (obj-x ownship) (obj-y ownship) (ship-radar ownship))))
            #:when (and (or (spaceship? o) (missile? o) (probe? o) (cannonball? o))
                        ((distance ownship o) . <= . (ship-radar ownship))
                        ((faction-check (ship-faction ownship) (ship-faction o)) . < . 0)))
        (define t (target-angle ownship ownship o o CANNON_SPEED 30.0))
        (define spread (atan (/ (ship-radius o)
                                (distance ownship o))))
        (when (and t ((abs (angle-frto (obj-r ownship) t)) . < . spread))
          (append! changes (list (command (ob-id ownship) #f 'cannon (obj-r ownship))))
          (done)))))
  changes)


; return a list of changes
(define (cannonball-ai! qt stack)
  (define changes '())
  (define ownship (get-ship stack))
  (when (and (ship-flying? ownship))
    (define any-closer?
      (for/or ((o (in-list (qt-retrieve qt (obj-x ownship) (obj-y ownship) (ship-radar ownship))))
               #:when (and (or (spaceship? o) (missile? o) (probe? o))
                           ((distance ownship o) . <= . (ship-radar ownship))
                           ((faction-check (ship-faction ownship) (ship-faction o)) . < . 0)))
        ; get our relative motion to target
        (define vx (- (obj-dx ownship) (obj-dx o)))
        (define vy (- (obj-dy ownship) (obj-dy o)))
        (define t (theta ownship o))
        (define t2 (atan0 vy vx))
        
        ((abs (angle-frto t2 t)) . < . (* pi 0.2))))
    
    (when (not any-closer?)
      (append! changes (list (chdam (ob-id ownship) (ship-maxcon ownship) #f)))))
  
  changes)
