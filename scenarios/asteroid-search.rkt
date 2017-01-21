#lang racket/base

(require racket/math)

(require "../defs.rkt"
         "../utils.rkt"
         "../ships.rkt"
         "../order.rkt"
         "../upgrade.rkt")

(provide (all-defined-out))

(define (asteroid-search-scenario oldspace oldtick oldmessage)
  (define ai? #f)
  (define players (if oldspace (space-players oldspace) '()))
  (for ((p players)) (set-player-faction! p "Empire"))

  (define hidden-base #f)

  (define ownspace
    (space 0 6000 4000 players '()
           `(
             ,(standard-quit-scenario-tab-button)
             ,(ann-text (next-id) 0 (posvel 0 -200 -100 0 0 0 0) #f
                        (string-append
                         "On mission to destroy a rebel outpost your engines have failed.\n"
                         "Use your fighters to search the asteroid field for a hidden cache.\n"
                         "Bring replacement parts back to the ship.\n"
                         "Once mobile again, destroy the outpost on the far side of the field.\n")
                        10000)
             ,@(for/list ((i 30))
                 (define x (+ -1500.0 (* i 100.0)))
                 (define y (random-between -2000 2000))
                 (define dx 0.0)
                 (define dy (random-between -150.0 150.0))
                 (define dr (random-between -1.0 1.0))
                 (define s (make-ship (if ((random) . > . 0.5) "asteroid_43" "asteroid_87")
                                      "Asteroid" "_neutral" #:x x #:y y
                                      #:dx dx #:dy dy #:dr dr))
                 (set-ship-cargo! s (list (upgrade (next-id) 0 #f "unscouted")))
                 (when (not hidden-base)
                   (set! hidden-base s)
                   (set-ship-pods! s (append (ship-pods s) (list (normal-hangar 0.0 0.0 '())))))
                 s)
             )))

  ; put the spare parts upgrade in the hidden base
  (define parts (upgrade (next-id) (space-time ownspace) #f "parts"))

  ; the good guys in this scenario
  (define (new-red-fighter)
    (define s (make-ship "red-fighter" "Empire Fighter" "Empire"
                         #:bat 200.0 #:con 50.0))
    (set-stats-thrust! (ship-stats s) 55.0)
    (set-obj-posvel! s #f)
    s)

  (define goodship (make-ship "red-frigate" "a" "a" #:x -2500.0 #:y -200.0 #:r 0.0))
  (set-ship-stats! goodship (stats (next-id) "red-frigate" "Empire Frigate" "Empire"
                                   ;power bat maxbat con maxcon radius mass thrust rthrust radar drag start
                                   10.0 100.0 100.0 500.0 500.0 18.0 100.0 20.0 0.3 300.0 0.4 #t))

  ; ship starts with pilot tools damaged beyond repair
  ; we will manually remove the dmgs when the engine parts are recovered
  (define steerdmgid (next-id))
  (define fthrustdmgid (next-id))
  (define warpdmgid (next-id))
  (set-ship-pods!
   goodship `(,(normal-lounge)
              ,(normal-hangar pi 10.0 (list (new-red-fighter)
                                            (new-red-fighter)
                                            (new-red-fighter)))
              ,(pod (next-id) "Pilot" #f #f 0.0 3.0 #f #f 100.0 100.0
                    (list (steer (next-id) (list (dmg steerdmgid "offline" 10000.0 0 #f)) (obj-r goodship))
                          (fthrust (next-id) (list (dmg fthrustdmgid "offline" 10000.0 0 #f)) #f)
                          (warp (next-id) (list (dmg warpdmgid "offline" 10000.0 0 #f)) 300.0 0.0 "release")))
              ,(pod (next-id) "WM" #f ai?
                    (degrees->radians 21.8) 21.5 (/ pi 6) pi/2 100.0 100.0
                    (list (pbolt (next-id) '() 5.0 #t)
                          (mtube (next-id) '() 100.0 100.0 "load" #f)))
              ,(pod (next-id) "WM" #f ai?
                    (degrees->radians -21.8) 21.5 (- (/ pi 6)) pi/2 100.0 100.0
                    (list (pbolt (next-id) '() 5.0 #t)
                          (mtube (next-id) '() 100.0 100.0 "load" #f)))
              ,(pod (next-id) "WP" #f ai?
                    (degrees->radians 130) 21.0 pi/2 pi 100.0 100.0
                    (list (pbolt (next-id) '() 5.0 #t)
                          (ptube (next-id) '() 100.0 100.0 "load" #f 120.0)))
              ,(pod (next-id) "WP" #f ai?
                    (degrees->radians -130) 21.0 (- pi/2) pi 100.0 100.0
                    (list (pbolt (next-id) '() 5.0 #t)
                          (ptube (next-id) '() 100.0 100.0 "load" #f 120.0)))))
  

  ; the bad guys
  (define (new-blue-fighter)
    (define s (make-ship "blue-fighter" "Rebel Fighter" "Rebel" #:npc? ai?))
    (set-obj-posvel! s #f)
    s)

  (define enemy-base (make-ship "blue-station" "a" "a" #:x 2500.0 #:y 200.0))
  (set-ship-stats! enemy-base (stats (next-id) "blue-station" "Rebel Outpost" "Rebel"
                               ;power bat maxbat con maxcon radius mass thrust rthrust radar drag start-ship?
                               10.0 500.0 500.0 750.0 750.0 26.0 1000.0 0.0 0.0 600.0 0.4 #t))
  (set-ship-pods!
   enemy-base
   `(,(normal-lounge)
     ,(normal-hangar pi 13.0 '())
     ,@(for/list ((d (in-list (list 0 90 180 270))))
         (pod (next-id) "W" #f #t (degrees->radians d) 26.0 (degrees->radians d) (* 0.9 pi) 50.0 50.0
              (list (pbolt (next-id) '() 10.0 #t)
                    (ptube (next-id) '() 100.0 100.0 "load" #f 100.0))))
     ,@(for/list ((d (in-list (list 45 135 225 315))))
         (pod (next-id) "T" #f #t (degrees->radians d) 28.0 (degrees->radians d) (* 0.9 pi) 100.0 100.0
              (list (shbolt (next-id) '() 20.0 #t)
                    (mtube (next-id) '() 100.0 100.0 "load" #f))))))
  
  (set-space-objects! ownspace (append (space-objects ownspace)
                                       (list goodship enemy-base)))

  (define playing? #t)
  (define time-limit (* 1000 60 15))  ; 15 min
  (define found-base? #f)
  (define dock-base? #f)
  (define parts-returned? #f)
  
  (define orders
    (timeout "Within ~a" 0 time-limit
             (ordercomb #f "" 'and
               (list (alive "Keep Frigate Alive" (ob-id goodship))
                     (ordercomb #f "" 'seq
                                (list (order #f "Find Asteroid Base with Fighters" '()
                                             (lambda (s f o) found-base?))
                                      (order #f "Dock Fighter with Asteroid" '()
                                             (lambda (s f o) dock-base?))
                                      (order #f "Return Parts to Frigate" '()
                                             (lambda (s f o) parts-returned?))
                                      (kill "Destroy Enemy Outpost" (ob-id enemy-base))))))))
  
  (define real-orders (space 0 0 0 '() '() '()))  ; only care about orders
  (set-space-orders-for! real-orders "Empire" orders)
  
  ; return a list of changes
  (define (on-tick ownspace change-scenario!)
    (define changes '())

    (for ((p (space-players ownspace)))
      (when (not (player-faction p))
        ; new player
        (append! changes (chfaction (ob-id p) "Empire"))))

    (for ((fo (space-orders real-orders)))
      (check ownspace (car fo) (cadr fo)))
    
    (define frig (find-id ownspace (ob-id goodship)))
    (define eb (find-id ownspace (ob-id enemy-base)))

    (when (and playing? (or (not frig) (not eb) ((space-time ownspace) . > . time-limit)))
      (set! playing? #f)  ; end scenario
      (define txt
        (cond ((not frig)
               "Frigate Destroyed, You Lose")
              ((not eb)
               "Enemy Outpost Destroyed, You Win!")
              (else
               "Ran Out of Time, You Lose")))
      
      (append! changes (chadd (ann-text (next-id) (space-time ownspace)
                                        (posvel 0 -200 -100 0 0 0 0) #f
                                        txt #f) #f))
      ; add end scenario button
      (append! changes (chadd (ann-button (next-id) (space-time ownspace)
                                          (posvel 0 -150 -160 0 100 50 0) #f
                                          "Quit Scenario" "quit-scenario") #f))
      )

    (when playing?
      (when (time-for (space-time ownspace) 55000 0000)
        (define m (message (next-id) (space-time ownspace) #f "New Fighter at Outpost"))
        (define f (new-blue-fighter))
        (set-ship-ai-strategy! f
          (list (strategy (space-time ownspace) "attack*" (ob-id goodship))
                (strategy (space-time ownspace) "return" (ob-id enemy-base))))
        (append! changes (chadd f (ob-id (ship-hangar enemy-base))) m))

      (define hb (find-id ownspace (ob-id hidden-base)))
      
      ; update scouted status of asteroids
      (define (unscouted-cargo o)
        (and (upgrade? o) (equal? "unscouted" (upgrade-type o))))
      
      (for ((s (space-objects ownspace))
            #:when (and (spaceship? s)
                        (equal? "Empire" (ship-faction s))
                        (not (equal? "probe" (ship-type s))))
            (a (space-objects ownspace))
            #:when (and (spaceship? a)
                        ((distance s a) . < . (ship-radar s))
                        (find-id a unscouted-cargo)))
        ; remove the unscouted cargo
        (append! changes (chrm (ob-id (find-id a unscouted-cargo))))
        ; check if this was the hidden base
        (when (and (not found-base?) (equal? (ob-id hb) (ob-id a)))
          (set! found-base? #t)
          (define newstats (copy (ship-stats hb)))
          (set-stats-faction! newstats "Empire")
          (append! changes (chstats (ob-id hb) newstats)
                   (chadd parts (ob-id hb))
                   (message (next-id) (space-time ownspace) #f "Discovered Hidden Base!"))))

      ; check if the good guys docked
      (when (and found-base? (not dock-base?))
        (define s (find-id (ship-hangar hb) spaceship?))
        (when s
          (set! dock-base? #t)
          (append! changes (chmov (ob-id parts) (ob-id hb) (ob-id s) #f)
                   (message (next-id) (space-time ownspace) #f "Parts Transferred to Fighter"))))

      ; check if the parts got back to goodship
      (when (and found-base? dock-base? (not parts-returned?))
        (define gs (find-id ownspace (ob-id goodship)))
        (define p (find-stack gs (ob-id parts)))
        (when p
          (set! parts-returned? #t)
          ; need to actually repair the engine somehow...
          (append! changes (chrm (ob-id parts))
                   (chrm steerdmgid) (chrm fthrustdmgid) (chrm warpdmgid)
                   (message (next-id) (space-time ownspace) #f "Frigate Engine Repaired!"))))
      )

    (append! changes (order-changes ownspace real-orders))
    
    changes)
  
  (define (on-message space cmd change-scenario!)
    (define o (find-id space (command-id cmd)))
    (when (and o (ann-button? o))
      (case (ann-button-msg o)
        (("quit-scenario") (change-scenario!))))
    '())
  
  (values ownspace on-tick on-message))


