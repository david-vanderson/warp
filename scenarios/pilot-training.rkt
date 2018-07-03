#lang racket/base

(require racket/math
         racket/string)

(require "../defs.rkt"
         "../utils.rkt"
         "../ships.rkt"
         "../order.rkt")

(provide (all-defined-out))

(define (pilot-training-scenario oldspace old-on-tick old-on-message old-on-player-restart)
  (define players (if oldspace (space-players oldspace) '()))
  (for ((p players))
    (set-player-faction! p #f))
  
  (define newspace
    (space (next-id) 0 2000 2000
           players
           '()
           `(
             ,(standard-quit-scenario-button #t)
             ,(ann-text (next-id) 0 #t (posvel 0 -200 -100 0 0 0 0) #f
                        "Scout your waypoints before time is up!" 5000)
             ,@(for/list ((i 30))
                 (define t (random-between 0 2pi))
                 (define d (random-between 500.0 1000.0))
                 (define x (* d (cos t)))
                 (define y (* d (sin t)))
                 (define dx (random-between -100.0 100.0))
                 (define dy (random-between -100.0 100.0))
                 (define dr (random-between -1.0 1.0))
                 (make-ship "asteroid_43" "Asteroid" "_neutral"
                            #:x x #:y y #:r 0
                            #:hp-bar? #f #:con 10000.0
                            #:dx dx #:dy dy #:dr dr))
             )))

  (define time-limit (* 1000 60 5))  ; 90 seconds for the whole scenario
  (define inside-time-limit? #t)

  (define (new-orders)
    (timeout "Within ~a" 0 time-limit
             (ordercomb #f "Scout" 'seq
                        (list (scout-waypoint "Scout A"  300 -300 50)
                              (scout-waypoint "Scout B"  300  300 50)
                              (scout-waypoint "Scout C" -300 -300 50)
                              (scout-waypoint "Scout D" -300  300 50)))))

  (define real-orders (space 0 0 0 0 '() '() '()))  ; only care about orders

  (define next-faction
    (let ((i 0))
      (lambda ()
        (set! i (add1 i))
        (format "Trainer ~a" i))))
  
  (define (new-trainer faction)
    (define s (make-ship "red-fighter" faction faction
                         #:x (random-between -200 200) #:y (random-between -200 200)))
    (set-ship-stats! s (stats (next-id) (ship-type s) (ship-name s) (ship-faction s)
                              ;con maxcon mass drag radar start?
                              100.0 100.0 20.0 300.0 0.4 #t))
    (set-ship-tools! s (append (list (tool-regen 1.0)) (ship-tools s)))
    s)

  (define (on-player-restart space pid)
    (define changes '())
    (define p (findfid pid (space-players space)))
    ; this happens during the processing of the client's dying message
    ; so the player is still in their spacesuit because it hasn't taken effect
    (define s (find-id space space (lambda (o) (and (ship? o) (equal? (ship-faction o) (player-faction p))))))
    (cond
      (s
       ; player has a ship but is not on any ship
       (append! changes (chmov (ob-id p) (ob-id s) #f)))
      (else
       ; player has no ship left
       (define o (new-orders))
       (set-space-orders-for! real-orders (player-faction p) o)
       (define nt (new-trainer (player-faction p)))
       (append! changes (chadd nt #f) (chmov (ob-id p) (ob-id nt) #f))))
    changes)
  
  (define (on-tick space qt change-scenario!)
    (define changes '())

    (for ((p (in-list (space-players space)))
          #:when (not (player-faction p)))
      ; new player
      (define f (next-faction))
      (define o (new-orders))
      (set-space-orders-for! real-orders f o)
      (define nt (new-trainer f))
      (append! changes (chfaction (ob-id p) f) (chadd nt #f) (chmov (ob-id p) (ob-id nt) #f)))

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
        (append! txt (string-append (player-name p)
                                    (if (check space (player-faction p) ot)
                                        " succeeded"
                                        " failed"))))
      (append! changes (chadd (ann-text (next-id) (space-time space) #t (posvel 0 -200 -100 0 0 0 0) #f
                                        (string-join txt "\n") #f) #f))
      ; add end scenario button
      (append! changes (chadd (standard-quit-scenario-button) #f))
      )

    (append! changes (order-changes space real-orders))
    
    changes)
  
  (define (on-message space cmd change-scenario!)
    (define o (find-id space space (anncmd-id cmd)))
    (when (and o (ann-button? o))
      (case (ann-button-msg o)
        (("quit-scenario") (change-scenario!))))
    '())
  (values newspace on-tick on-message on-player-restart))
