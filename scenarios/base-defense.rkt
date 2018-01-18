#lang racket/base

(require racket/math)

(require "../defs.rkt"
         "../utils.rkt"
         "../ships.rkt"
         "../order.rkt"
         "../upgrade.rkt")

(provide (all-defined-out))

(define (base-defense-scenario oldspace oldtick oldmessage)
  (define ai? #t)
  (define players (if oldspace (space-players oldspace) '()))
  (for ((p players)) (set-player-faction! p "Rebel"))

  (define ownspace (space 0 5000 2000 players '()
                          `(
                            ,(standard-quit-scenario-tab-button)
                            ,(ann-text (next-id) 0 (posvel 0 -200 -100 0 0 0 0) #f
                                       (string-append
                                        "Defend your base from the incoming destroyer.\n"
                                        "Use your cruiser to attack, dock on the station to repair.\n"
                                        "Enemy frigates drop upgrades when killed.")
                                       10000)
                            )))
  
  (define (new-blue-fighter)
    (define s (make-ship "blue-fighter" "a" "a" #:ai? #t))
    (set-ship-stats! s (stats (next-id) "blue-fighter" "Rebel Fighter" "Rebel"
                              ;con maxcon radius mass radar drag start-ship?
                              50.0 50.0 6.0 20.0 300.0 0.4 #f))
    (set-ship-tools!
     s (list (tool (next-id) 'engine 50.0 #f '())
             (tool (next-id) 'turnleft 1.5 #f '())
             (tool (next-id) 'turnright 1.5 #f '())
             (tool (next-id) 'steer 1.5 #f '())
             (tool (next-id) 'pbolt 5.0 #f '())
             (tool (next-id) 'dock #f #t '())
             ))
    (set-obj-posvel! s #f)
    s)
  
  (define (new-red-fighter)
    (define s (make-ship "red-fighter" "a" "a" #:ai? #t))
    (set-ship-stats! s (stats (next-id) "red-fighter" "Empire Fighter" "Empire"
                              ;con maxcon radius mass radar drag start
                              20.0 20.0 6.0 20.0 300.0 0.4 #f))
    (set-ship-tools!
     s (list (tool (next-id) 'engine 50.0 #f '())
             (tool (next-id) 'turnleft 1.5 #f '())
             (tool (next-id) 'turnright 1.5 #f '())
             (tool (next-id) 'steer 1.5 #f '())
             (tool (next-id) 'pbolt 5.0 #f '())
             (tool (next-id) 'dock #f #t '())
             ))
    (set-obj-posvel! s #f)
    s)
  
  
  (define cruiser (make-ship "blue-cruiser" "z" "z" #:x -1800 #:y -50 #:ai? #t
                             #:hangar (list (new-blue-fighter))))
  (set-ship-stats! cruiser (stats (next-id) "blue-cruiser" "Rebel Cruiser" "Rebel"
                                  ;con maxcon radius mass radar drag start?
                                  150.0 150.0 15.0 100.0 500.0 0.4 #t))
  (set-ship-tools!
   cruiser (list (tool (next-id) 'engine 30.0 #f '())
                 (tool (next-id) 'turnleft 1.0 #f '())
                 (tool (next-id) 'turnright 1.0 #f '())
                 (tool (next-id) 'pbolt 5.0 #f '())
                 (tool (next-id) 'dock #f #t '())
                 (tool (next-id) 'probe 10.0 #f '())
                 (tool (next-id) 'missile 5.0 #f '())
                 (tool (next-id) 'warp '(150.0 100.0 0.0) #f '())
                 ))
  
  
  (define base (make-ship "blue-station" "a" "a" #:x -2000 #:y -100 #:ai? #t #:hangar '()))
  (set-ship-stats! base (stats (next-id) "blue-station" "Rebel Outpost" "Rebel"
                               ;con maxcon radius mass radar drag start-ship?
                               1000.0 1000.0 26.0 1000.0 1000.0 0.4 #t))
  (set-ship-tools!
   base (list (tool (next-id) 'pbolt 5.0 #f '())
              (tool (next-id) 'probe 30.0 #f '())
              (tool (next-id) 'missile 5.0 #f '())
          ))
  
  
  (define destroyer (make-ship "red-destroyer" "b" "b" #:x 2400 #:y 100 #:r pi #:ai? #t
                               #:hangar '()))
  (set-ship-stats! destroyer (stats (next-id)
                                    ;type name faction
                                    "red-destroyer" "Empire Destroyer" "Empire"
                                    ;con maxcon radius mass radar drag start?
                                    1000.0 1000.0 23.0 500.0 1000.0 0.4 #f))
  (set-ship-tools!
   destroyer `(,(tool (next-id) 'engine 6.0 #f '())
               ,(tool (next-id) 'turnleft 0.1 #f '())
               ,(tool (next-id) 'turnright 0.1 #f '())
               ,(tool (next-id) 'steer 0.1 #f '())
               ,(tool (next-id) 'pbolt 5.0 #f '())
               ,(tool (next-id) 'missile 5.0 #f '())
               ))
  
  (set-ship-ai-strategy! destroyer
                         (list (strategy (space-time ownspace) "attack-only" (ob-id base))))
  
  
  ;(define special (new-blue-fighter))
  ;(set-obj-posvel! special (posvel 0 -2100.0 -100.0 0.0 0.0 0.0 0.0))
  ;(set-ship-cargo! special (list (random-upgrade ownspace #f) (random-upgrade ownspace #f)))
  ;(define special2 (new-red-fighter))
  ;(set-obj-posvel! special2 (posvel 0 1000.0 0.0 0.0 0.0 0.0 0.0))
  
  
  (set-space-objects! ownspace (append (space-objects ownspace) (list cruiser base destroyer #;special)))

  (define rebel-orders
    (ordercomb #f "Do All:" 'and
               (list (alive "Keep Base Alive" (ob-id base))
                     (alive "Keep Cruiser Alive" (ob-id cruiser))
                     (kill "Kill Enemy Destroyer" (ob-id destroyer)))))
  
  (define real-orders (space 0 0 0 '() '() '()))  ; only care about orders
  (set-space-orders-for! real-orders "Rebel" rebel-orders)

  
  ;(for ((i 10) (t (in-cycle '("power" "thrust" "bat" "con"))))
  ;  (define u (upgrade (next-id) (space-time ownspace)
  ;                     (posvel (space-time ownspace) -1700 (+ -250 (* i 50)) 0 0 0 0)
  ;                     t))      
  ;  (set-space-objects! ownspace (cons u (space-objects ownspace))))
  
  
  (define next-enemy-count 0)
  (define last-base-con 10000)
  (define last-enemy-base-con 10000)
  (define playing? #t)
  
  ; return a list of changes
  (define (on-tick ownspace change-scenario!)
    (define changes '())

    (for ((p (space-players ownspace)))
      (when (not (player-faction p))
        ; new player
        (append! changes (chfaction (ob-id p) "Rebel"))))

    (for ((fo (space-orders real-orders)))
      (check ownspace (car fo) (cadr fo)))
    
    (define hb (find-id ownspace (ob-id base)))
    (define eb (find-id ownspace (ob-id destroyer)))
    (define c (find-id ownspace (ob-id cruiser)))

    (when (and playing? (or (not hb) (not eb) (not c)))
      (set! playing? #f)
      (define txt
        (cond ((not hb)
               "Base Destroyed, Rebels Defeated, You Lose")
              ((not c)
               "Cruiser Destroyed, Rebels Defeated, You Lose")
              (else
               "Enemy Defeated, You Win")))
      
      (append! changes (chadd (ann-text (next-id) (space-time ownspace)
                                        (posvel 0 -200 -100 0 0 0 0) #f
                                        txt #f) #f))
      ; add end scenario button
      (append! changes (chadd (ann-button (next-id) (space-time ownspace)
                                          (posvel 0 -150 -160 0 120 50 0) #f
                                          "Quit Scenario" "quit-scenario") #f))
      )

    (when (and playing? hb eb)
      (when (time-for (space-time ownspace) 55000 0000)
        (define m (message (next-id) (space-time ownspace) #f "New Fighter at Outpost"))
        (define f (new-blue-fighter))
        (append! changes (chadd f (ob-id base)) m))
      
      (when (time-for (space-time ownspace) 65000 20000)
        (define f (new-red-fighter))
        (append! changes (chadd f (ob-id destroyer))))
      
      (when (time-for (space-time ownspace) 90000 10000)
        (define m (message (next-id) (space-time ownspace) #f "Empire Frigate Incoming"))
        (define x (+ (/ (space-width ownspace) 2) 100))
        (define y (random-between (- (/ (space-height ownspace) 2)) (/ (space-height ownspace) 2)))
        (define fighters (for/list ((i (random 3)))
                           (make-ship "red-fighter" "Empire Fighter" "Empire")))
        (define f (make-ship "red-frigate" "Empire Frigate" "Empire" #:x x #:y y #:r pi #:ai? #t
                             #:hangar fighters #:cargo (list (random-upgrade ownspace #f)
                                                                (random-upgrade ownspace #f))))
        (set-ship-ai-strategy! f (list (strategy (space-time ownspace) "attack*" (ob-id base))))
        (append! changes (chadd f #f) m))
      
      
      
      (when ((ship-con hb) . < . (- last-base-con 100))
        (define m (message (next-id) (space-time ownspace) #f (format "Outpost Health: ~a" (inexact->exact (round (ship-con hb))))))
        (append! changes m)
        (set! last-base-con (ship-con hb)))
      
      
      (when (< (ship-con eb) (- last-enemy-base-con 50))
        (define m (message (next-id) (space-time ownspace) #f (format "Empire Destroyer Health: ~a" (inexact->exact (round (ship-con eb))))))
        (append! changes m)
        (set! last-enemy-base-con (ship-con eb))))

    (append! changes (order-changes ownspace real-orders))
    
    changes)
  
  (define (on-message space cmd change-scenario!)
    (define o (find-id space (command-id cmd)))
    (when (and o (ann-button? o))
      (case (ann-button-msg o)
        (("quit-scenario") (change-scenario!))))
    '())
  
  (values ownspace on-tick on-message))


