#lang racket/base

(require "defs.rkt"
         "utils.rkt"
         "change.rkt"
         "order.rkt"
         "base-defense.rkt"
         racket/format
         racket/string
         "ships.rkt")

(provide (all-defined-out))


;; Scenario function
; oldspace old-on-tick old-on-message -> (values newspace on-tick on-message)
; on-tick: space change-scenario! -> list of commands/changes
; on-message: space command-message change-scenario! -> list of commands/changes


;; Scenario Picking

(define (sc-pick oldspace old-on-tick old-on-message)
  (define players (if oldspace (space-players oldspace) '()))
  (for ((p players)) (set-player-faction! p "players"))
  (define newspace
    (space 0 1000 1000
           players
           '()
           `(
             ,(ann-button (next-id) 0 (posvel 0 -200 -200 0 200 100 0) #f "Pilot Training" "pilot-training")
             ,(ann-button (next-id) 0 (posvel 0 -200 0 0 200 100 0) #f "Base Defense" "base-defense")
             )))
  (define (on-tick space change-scenario!)
    (define changes '())
    (for ((p (space-players space)))
      (when (not (player-faction p))
        ; new player
        (append! changes (chfaction (ob-id p) "players"))))
    changes)
  (define (on-message space cmd change-scenario!)
    (define o (find-id space (command-id cmd)))
    (when (and o (ann-button? o))
      (case (ann-button-msg o)
        (("base-defense") (change-scenario! base-defense-scenario))
        (("pilot-training") (change-scenario! pilot-training-scenario))))
    '())
  (values newspace on-tick on-message))




(define (pilot-training-scenario oldspace old-on-tick old-on-message)
  (define players (if oldspace (space-players oldspace) '()))
  (for ((p players))
    (set-player-faction! p #f))
  
  (define newspace
    (space 0 10000 10000
           players
           '()
           `(
             ,(ann-button (next-id) 0 (posvel 0 (+ LEFT 60) (+ TOP 110) 0 100 50 0) #t "Quit Scenario" "quit-scenario")
             ,(ann-text (next-id) 0 (posvel 0 -200 -100 0 0 0 0) #f
                        "Scout your waypoints before time is up!" 5000)
             )))

  (define time-limit (* 1000 60 1))  ; 1 minute for the whole scenario
  (define inside-time-limit? #t)

  (define (new-orders)
    (timeout "Within ~a" 0 time-limit
             (ordercomb #f "Scout" 'seq
                        (list (scout-waypoint "Scout A"  300 -300 50)
                              (scout-waypoint "Scout B"  300  300 50)
                              (scout-waypoint "Scout C" -300 -300 50)
                              (scout-waypoint "Scout D" -300  300 50)))))

  (define real-orders (space 0 0 0 '() '() '()))  ; only care about orders

  (define next-faction
    (let ((i 0))
      (lambda ()
        (set! i (add1 i))
        (format "Trainer ~a" i))))
  
  (define (new-trainer faction)
    (define s (make-ship "red-fighter" faction faction #:npc? #f
                         #:x (random-between -100 100) #:y (random-between -100 100)))
    (set-ship-stats! s (stats (next-id) (ship-type s) (ship-name s) (ship-faction s)
                              ;power bat maxbat con maxcon radius mass thrust rthrust radar start?
                              1.0 100.0 100.0 20.0 20.0 6.0 20.0 50.0 1.5 300.0 #t))
    s)
  
  (define (on-tick space change-scenario!)
    (define changes '())
    (for ((p (space-players space)))
      (cond
        ((not (player-faction p))
         ; new player
         (define f (next-faction))
         (define o (new-orders))
         (set-space-orders-for! real-orders f o)
         (append! changes (chfaction (ob-id p) f) (chadd (new-trainer f) #f)))
        ((not (find-id space (lambda (o) (and (ship? o) (equal? (ship-faction o) (player-faction p))))))
         ; player has no ship left
         (define o (new-orders))
         (set-space-orders-for! real-orders (player-faction p) o)
         (append! changes (chadd (new-trainer (player-faction p)) #f)))))

    (for ((fo (space-orders real-orders)))
      (check space (car fo) (cadr fo)))

    (when (and inside-time-limit? ((space-time space) . > . time-limit))
      ; scenario end
      (set! inside-time-limit? #f)
      ; add text annotation that says who finished or didn't
      (define txt '("Pilot Training Over"))
      (for ((p (space-players space)))
        (define ot (get-space-orders-for real-orders (player-faction p)))
        ;(printf "ot ~v\n" ot)
        (append! txt (~a (player-name p)
                         (if (check space (player-faction p) ot)
                             " succeeded"
                             " failed"))))
      (append! changes (chadd (ann-text (next-id) (space-time space) (posvel 0 -200 -100 0 0 0 0) #f
                                        (string-join txt "\n") #f) #f))
      ; add end scenario button
      (append! changes (chadd (ann-button (next-id) (space-time space) (posvel 0 200 100 0 100 50 0) #f "Quit Scenario" "quit-scenario") #f))
      )
    
    (append! changes (order-changes space real-orders))
    
    changes)
  
  (define (on-message space cmd change-scenario!)
    (define o (find-id space (command-id cmd)))
    (when (and o (ann-button? o))
      (case (ann-button-msg o)
        (("quit-scenario") (change-scenario! sc-pick))))
    '())
  (values newspace on-tick on-message))
  