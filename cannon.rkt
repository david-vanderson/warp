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

(define CANNON_MAX_SPEED 75.0)


; return list of changes
(define (reduce-cannonball! space b damage)
  (define changes '())

  ; -1 damage means exploding
  ; -2 damage means dying silently because we hit a ship
  
  (if (damage . > . 0)
      (set-stats-con! (ship-stats b) (- (ship-con b) damage))
      (set-stats-con! (ship-stats b) damage))
  
  (when ((ship-con b) . <= . 0)
    (set-space-objects! space (remove-id (ob-id b) (space-objects space)))
    (when (and (server?) (not (equal? -2 damage)))
      ; explode
      (define num 16)
      (for ((i (in-range num)))
        (define r (* i (/ 2pi num)))
        (define a (angle-add (obj-r b) r))
        (define p (plasma (next-id) (space-time space)
                          (posvel (space-time space)
                                  (obj-x b) (obj-y b) (obj-r b)
                                  (+ (* PLASMA_SPEED 0.7 (cos a)))
                                  (+ (* PLASMA_SPEED 0.7 (sin a)))
                                  0)
                          (/ (* 5.0 (ship-maxcon b)) num) #f))
        (append! changes (chadd p #f)))))
  
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
                            #:start-time (space-time space)
                            #:life (tool-val tool)))
       (define d (+ (ship-radius ship) (* 1.1 (ship-radius b))))
       (define speed CANNON_MAX_SPEED)
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
(define (cannon-ai! space stack)
  (define changes '())
  (define ownship (get-ship stack))
  (define c (ship-tool ownship 'cannon))
  (when (and (ship-flying? ownship)
             (tool-online? c))
    (let/ec done
      (for ((o (in-list (space-objects space)))
            #:when (and (or (spaceship? o) (missile? o) (probe? o) (cannonball? o))
                        ((faction-check (ship-faction ownship) (ship-faction o)) . < . 0)
                        ((distance ownship o) . < . 600)))
        
        (define t (target-angle ownship ownship o o CANNON_MAX_SPEED))
        (when (and t ((abs (angle-frto (obj-r ownship) t)) . < . 0.1))
          (append! changes (list (command (ob-id ownship) 'cannon t)))
          (done)))))
  changes)


; return a list of changes
(define (cannonball-ai! space stack)
  (define changes '())
  (define ownship (get-ship stack))
  (when (and (ship-flying? ownship))
    (define any-closer?
      (for/or ((o (in-list (space-objects space)))
               #:when (and (or (spaceship? o) (missile? o) (probe? o))
                           ((faction-check (ship-faction ownship) (ship-faction o)) . < . 0)
                           ((distance ownship o) . < . 650)))
        ; get our relative motion to target
        (define vx (- (obj-dx ownship) (obj-dx o)))
        (define vy (- (obj-dy ownship) (obj-dy o)))
        (define t (theta ownship o))
        (define t2 (atan0 vy vx))
        
        ((abs (angle-frto t2 t)) . < . (* pi 0.2))))
    
    (when (not any-closer?)
      (append! changes (list (chdam (ob-id ownship) -1)))))
  
  changes)


(define (cannonball-fitness space m)
  (define f 0.0)
  
  (for ((o (in-list (space-objects space)))
        #:when (spaceship? o))
    (define d (distance o m))
    (define hd (+ (ship-radius o) (ship-radius m)))
    (define foe? ((faction-check (ship-faction m) (ship-faction o)) . < . 0))
    (cond ((d . < . hd)
           (set! f (+ f (if foe? 1000.0 -1000.0))))
          (else
           (set! f (+ f (* (if foe? 1000.0 -1000.0) (- 1.0 (sigmoid d 100))))))))
  f)