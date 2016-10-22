#lang racket/base

(require "defs.rkt"
         "utils.rkt"
         "change.rkt"
         "order.rkt"
         "base-defense.rkt"
         "ships.rkt")

(provide (all-defined-out))


;; Scenario function
; oldspace old-on-tick old-on-message -> (values newspace on-tick on-message)
; on-tick: space change-scenario! -> list of commands/changes
; on-message: space command-message change-scenario! -> list of commands/changes


;; Scenario Picking

(define (sc-pick oldspace old-on-tick old-on-message)
  (define newspace
    (space 0 1000 1000
           (if oldspace (space-players oldspace) '())
           '()
           `(
             ,(ann-button (next-id) 0 (posvel 0 -200 200 0 0 0 0) 200 100 "Pilot Training" "pilot-training")
             ,(ann-button (next-id) 0 (posvel 0 -200 0 0 0 0 0) 200 100 "Base Defense" "base-defense")
             )))
  (define (on-tick space change-scenario!)
    '())
  (define (on-message space cmd change-scenario!)
    (define o (find-id space (command-id cmd)))
    (when (and o (ann-button? o))
      (case (ann-button-msg o)
        (("base-defense") (change-scenario! base-defense-scenario))
        (("pilot-training") (change-scenario! pilot-training-scenario))))
    '())
  (values newspace on-tick on-message))


(define next-faction
  (let ((i 0))
    (lambda ()
      (set! i (add1 i))
      (format "Training~a" i))))

(define (new-trainer faction)
  (define s (make-ship "red-fighter" "Pilot Trainer" faction #:npc? #f
                       #:x (random-between -100 100) #:y (random-between -100 100)))
  (set-ship-stats! s (stats (next-id) (ship-type s) (ship-name s) (ship-faction s)
                            ;power bat maxbat con maxcon radius mass thrust rthrust radar start?
                            1.0 100.0 100.0 20.0 20.0 6.0 20.0 50.0 1.5 300.0 #t))
  s)

(define (pilot-training-scenario oldspace old-on-tick old-on-message)
  (define players (if oldspace (space-players oldspace) '()))
  (for ((p players))
    (set-player-faction! p #f))
  
  (define newspace
    (space 0 10000 10000
           players
           '()
           '()))

  (define orders
    (ordercomb #f "Scout" 'seq
          (list (ordercomb #f "First Half" 'seq
                           (list (scout-waypoint "Scout A" 500    0 50)
                                 (scout-waypoint "Scout B" 500 -500 50)))
                (ordercomb #f "Second Half" 'seq
                           (list (scout-waypoint "Scout C"   0 -500 50)))
                (scout-waypoint "Scout D"   0    0 50))))
  
  (define (on-tick space change-scenario!)
    (define changes '())
    (for ((p (space-players space)))
      (cond
        ((not (player-faction p))
         (define f (next-faction))
         (set-space-orders-for! space f orders)
         (append! changes (chorders f (scrub orders)) (chfaction (ob-id p) f) (chadd (new-trainer f) #f)))
        ((not (find-id space (lambda (o) (and (ship? o) (equal? (ship-faction o) (player-faction p))))))
         (append! changes (chadd (new-trainer (player-faction p)) #f)))))
    
    (for ((fo (space-orders space)))
      (define faction (car fo))
      (define ot (cadr fo))
      (define old (scrub ot))
      (define d (check space faction ot))

      ; check could rewrite space-orders, so find it again
      (define newfo (findf (lambda (fo) (equal? faction (car fo))) (space-orders space)))
      (define new (scrub (cadr newfo)))
      (when (not (equal? old new))
        (append! changes (chorders faction new))))
    changes)
  
  (define (on-message space cmd change-scenario!)
    '())
  (values newspace on-tick on-message))
  