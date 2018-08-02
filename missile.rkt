#lang racket/base

(require "defs.rkt"
         "utils.rkt"
         "quadtree.rkt"
         "ships.rkt")

(provide (all-defined-out))


; return list of changes
(define (reduce-missile! space m damage)
  (define changes '())

  (set-stats-con! (ship-stats m) (- (ship-con m) damage))

  (when ((ship-con m) . <= . 0)
    (set-obj-alive?! m #f)
    
    (when (server?)
      ; missile is detonating
      (define e (make-explosion space (obj-x m) (obj-y m) 2.0 20.0 50.0 50.0))
      (append! changes (chadd e #f))))
  changes)


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
       (define a (angle-add (obj-r ship) (if (equal? 'left (command-arg cmd)) pi/2 (- pi/2))))
       (define m (make-ship "missile" "Missile" (ship-faction ship)
                            #:ai? (not (player? (car stack)))
                            #:r a
                            #:radar (ship-radar ship)
                            #:start-time (space-time space)
                            #:life (car (tool-val tool))
                            #:con (cadr (tool-val tool))))
       (define d (+ (ship-radius ship) (ship-radius m) 0.1))
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


(define (missile-target? o)
  (or (spaceship? o) (probe? o)))

; return a list of changes
(define (missile-ai! space qt stack)
  (define changes '())
  (define ownship (get-ship stack))
  (define t (ship-tool ownship 'missile))
  (define last-time (tool-rc t))
  (when (and (or (not last-time)
                 ((- (space-time space) last-time) . > . 5000.0))
             (ship-flying? ownship)
             (tool-online? t))
    (define ne (nearest-enemy qt ownship missile-target?))
    (when ne
      ;(printf "firing\n")
      (set-tool-rc! t (space-time space))
      (define a (angle-frto (posvel-r (obj-posvel ownship))
                            (theta ownship ne)))
      (define side (if (a . > . 0) 'left 'right))
      (append! changes (command (ob-id ownship) #f 'missile side))))
  changes)


(define (missile-fitness space qt m)
  (define f 0.0)
  (define live? #t)

  (for ((o (in-list (qt-retrieve qt (obj-x m) (obj-y m) (ship-radar m))))
        #:when (missile-target? o))
    (define d (distance o m))
    (when (d . <= . (ship-radar m))
      (define foe? ((faction-check (ship-faction m) (ship-faction o)) . < . 0))
      (when foe?
        ; linearly incentivize flying towards enemies in general
        (set! f (- f d)))

      (define hd (hit-distance o m))
      (define maxd (+ hd AI_HIT_CLOSE))
      (when (d . < . maxd)
        (define z (- maxd d))  ; meters inside maxd
        (set! f (+ f (* z z (if foe? 1.0 -1.0))))
        (when (d . < . (+ hd (/ AI_HIT_CLOSE 2)))
          (set! live? #f)))))
  (values f live?))