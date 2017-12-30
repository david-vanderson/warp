#lang racket/base

(require racket/class
         racket/math
         mode-lambda
         racket/draw)

(require "defs.rkt"
         "utils.rkt"
         "physics.rkt"
         "ships.rkt"
         "draw-utils.rkt")

(provide (all-defined-out))


; return list of changes
(define (reduce-missile! space m damage)
  (define changes '())
  ; missiles die instantly
  (set-stats-con! (ship-stats m) 0)  ; mark as dead
  (set-space-objects! space (remove-id (ob-id m) (space-objects space)))

  (when (and (server?) (= -1 damage))
    ; missile is detonating
    (define n 5)
    (define da (degrees->radians 40.0))
    (define starta (- (* (- n 1) (/ da 2.0))))
    (for ((i n))
      (define a (angle-add (obj-r m) (+ starta (* i da))))
      (define p (plasma (next-id) (space-time space)
                        (posvel (space-time space) (obj-x m) (obj-y m) (obj-r m)
                                (+ (* (/ PLASMA_SPEED 3.0) (cos a)) (obj-dx m))
                                (+ (* (/ PLASMA_SPEED 3.0) (sin a)) (obj-dy m))
                                0)
                        5.0 #f))
      (append! changes (chadd p #f))))
  changes)



(define (draw-missile csd center scale m space fowa)
  (obj-sprite m csd center scale LAYER_SHIPS 'missile 10.0 fowa (obj-r m) "black"))



(define (dmg-mtube tool)
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

(define (launch-missile! cmd space stack who)
  (define ship (get-ship stack))
  (define tool (ship-tool ship 'missile))
  (cond
    ((not (ship-flying? ship))
     (printf "~a discarding message (not flying) ~v\n" who cmd)
     (values #f '()))
    ((not tool)
     (printf "~a discarding message (no missile tool) ~v\n" who cmd)
     (values #f '()))
    (else
     (define changes '())
     (when (server?)
       (define a (angle-add (obj-r ship) (if ((random) . > . 0.5) pi/2 (- pi/2))))
       (define m (make-ship "missile" "Missile" (ship-faction ship)
                            #:ai? (not (player? (car stack)))
                            #:r a
                            #:start-time (space-time space)
                            #:life (tool-val tool)))
       (define d (+ (ship-radius ship) (* 2.0 (ship-radius m))))
       (define speed 10.0)
       (set-obj-posvel! m (posvel (space-time space)
                                  (+ (obj-x ship) (* d (cos a)))
                                  (+ (obj-y ship) (* d (sin a)))
                                  a
                                  (+ (obj-dx ship) (* speed (cos a)))
                                  (+ (obj-dy ship) (* speed (sin a)))
                                  0))

       (append! changes (chadd m #f))
       (when (player? (car stack))
         (append! changes (chrc (ob-id (car stack)) (ob-id m)))))
     
     (values #f changes))))


; return a list of changes
(define (missile-ai! space stack)
  (define changes '())
  (define ownship (get-ship stack))
  (define t (ship-tool ownship 'missile))
  (define last-time (tool-rc t))
  (when (and (or (not last-time)
                 ((- (space-time space) last-time) . > . 5000.0))
             (ship-flying? ownship)
             (tool-online? t))
    (for/first ((o (in-list (space-objects space)))
                #:when (and (spaceship? o)
                            ((faction-check (ship-faction ownship) (ship-faction o)) . < . 0)
                            ((distance ownship o) . < . (ship-radar ownship))))
      ;(printf "firing\n")
      (set-tool-rc! t (space-time space))
      (append! changes (command (ob-id ownship) 'missile #t))
      ))
  changes)


(define (missile-fitness space m)
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