#lang racket/base

(require racket/math)

(require "../defs.rkt"
         "../utils.rkt"
         "../ships.rkt"
         "../order.rkt"
         "../quadtree.rkt"
         "../upgrade.rkt")

(provide (all-defined-out))

(define (base-defense-scenario oldspace oldtick oldmessage old-on-player-restart)  
  (define players (if oldspace (space-players oldspace) '()))
  (for ((p players)) (set-player-faction! p #f))

  (define ownspace (space (next-id) 0 8000 4000 players '()
                          `(
                            ,(standard-quit-scenario-button)
                            ,(make-ann-text -200 -100 0 10000 15000
                                       (string-append
                                        "Defend your base from the incoming destroyer.\n"
                                        "Use your cruiser to attack, dock on the station to repair.\n"
                                        "Enemy cruisers drop upgrades when killed."))
                            )))
  
  (define (new-blue-fighter)
    (make-ship "blue-fighter" "Rebel Fighter" "Rebel" #:ai 'empty
               #:hull 50 #:mass 20 #:drag 0.4
               #:tools (append (tools-pilot 50.0 #f 1.5)
                               (list (tool-pbolt 8.0)
                                     (tool-regen 1.0)))))
  
  (define (new-red-fighter)
    (make-ship "red-fighter" "Empire Fighter" "Empire" #:ai 'always
               #:hull 20 #:mass 20 #:drag 0.4
               #:tools (append (tools-pilot 50.0 #f 1.6)
                               (list (tool-pbolt 8.0)))))
  
  
  (define cruiser (make-ship "blue-cruiser" "Rebel Cruiser" "Rebel"
                             #:x -1800 #:y 400 #:ai 'empty
                             #:hull 200 #:mass 100 #:drag 0.4
                             #:radar 500 #:start-ship? #t
                             #:hangar (list (new-blue-fighter))
                             #:tools (append (tools-pilot 25.0 #f 1.0)
                                             (list (tool-pbolt 10.0)
                                                   (tool-probe 10.0)
                                                   (tool-missile 5.0 10.0)
                                                   (tool-cannon 21.0)
                                                   (tool-mine 30.0)
                                                   (tool-warp 200.0 80.0)
                                                   (tool-regen 1.0)))))
  
  
  (define base (make-ship "blue-station" "Rebel Outpost" "Rebel"
                          #:x -2200 #:y 500 #:ai 'always #:hangar '()
                          #:dr 0.1 #:radar 1000
                          #:hull 1000 #:mass 1000 #:drag 0.4
                          #:tools (list (tool-pbolt 10.0)
                                        (tool-probe 30.0)
                                        (tool-missile 5.0 10.0))))
    
  
  (define destroyer (make-ship "red-destroyer" "Empire Destroyer" "Empire"
                               #:x 2400 #:y 100 #:r pi #:ai 'always
                               #:hangar '() #:radar 1000
                               #:hull 1000 #:mass 500 #:drag 0.4
                               #:tools (append (tools-pilot 6.0 #f 0.1 #:dock? #f)
                                               (list (tool-pbolt 10.0)
                                                     (tool-missile 5.0 10.0)
                                                     (tool-cannon 21.0)))
                               #:ai-strats (list (strategy 0 "attack-only" (ob-id base)))))
  
  (set-space-objects! ownspace (append (space-objects ownspace)
                                       (list cruiser base destroyer)))

  (define rebel-orders
    (ordercomb #f "Do All:" 'and
               (list (alive "Keep Base Alive" (ob-id base))
                     (alive "Keep Cruiser Alive" (ob-id cruiser))
                     (kill "Kill Enemy Destroyer" (ob-id destroyer)))))
  
  (define real-orders (space 0 0 0 0 '() '() '()))  ; only care about orders
  (set-space-orders-for! real-orders "Rebel" rebel-orders)

  
  (define playing? #t)

  (define (on-player-restart space pid)
    (define changes '())
    ; this happens during the processing of the client's dying message
    ; so the player is still in their spacesuit because it hasn't taken effect
    (define c (find-id ownspace ownspace (ob-id cruiser)))
    (when c
      (append! changes (chmov pid (ob-id c) #f)))
    changes)

  (define (upgrade-hit-ship space ship u)
    (define changes '())
    (append! changes
             (upgrade-ship-random space ship)
             (chrm (ob-id u)))
    changes)
  
  ; return a list of changes
  (define (on-tick ownspace qt change-scenario!)
    (define changes '())

    (define hb (find-id ownspace ownspace (ob-id base)))
    (define eb (find-id ownspace ownspace (ob-id destroyer)))
    (define c (find-id ownspace ownspace (ob-id cruiser)))

    (for ((p (space-players ownspace)))
      (when (not (player-faction p))
        ; new player
        (append! changes (chfaction (ob-id p) "Rebel"))
        (when c
          (append! changes (chmov (ob-id p) (ob-id c) #f)))))

    (for ((s (space-objects ownspace))
            #:when (and (obj-alive? s)
                        (upgrade? s)))
      (for ((a (qt-retrieve qt (obj-x s) (obj-y s) (upgrade-radius ownspace s)))
            #:when (and (obj-alive? a)
                        (spaceship? a)))
        (append! changes (upgrade-hit-ship ownspace a s))))

    (for ((fo (space-orders real-orders)))
      (check ownspace (car fo) (cadr fo)))

    (when (and playing? (or (not hb) (not eb) (not c)))
      (set! playing? #f)
      (define txt
        (cond ((not hb)
               "Base Destroyed, Rebels Defeated, You Lose")
              ((not c)
               "Cruiser Destroyed, Rebels Defeated, You Lose")
              (else
               "Enemy Defeated, You Win")))
      
      (append! changes (chadd (make-ann-text -200 -100 (space-time ownspace) #f #f txt) #f))
      ; add end scenario button
      (append! changes (chadd (standard-quit-scenario-button #f) #f))
      )

    (when (and playing? hb eb)
      (when (time-for (space-time ownspace) 55000 0000)
        (define m (make-message ownspace "New Fighter at Outpost"))
        (define f (new-blue-fighter))
        (append! changes (chadd f (ob-id base)) m))
      
      (when (time-for (space-time ownspace) 65000 20000)
        (define f (new-red-fighter))
        (append! changes (chadd f (ob-id destroyer))))
      
      (when (time-for (space-time ownspace) 90000 10000)
        (define m (make-message ownspace "Empire Frigate Incoming"))
        (define x (+ (/ (space-width ownspace) 2) 100))
        (define y (random-between (- (/ (space-height ownspace) 2)) (/ (space-height ownspace) 2)))
        (define fighters (for/list ((i (random 3)))
                           (make-ship "red-fighter" "Empire Fighter" "Empire")))
        (define f (make-ship "red-frigate" "Empire Frigate" "Empire"
                             #:x x #:y y #:r pi #:ai 'always
                             #:hull 200 #:mass 100 #:drag 0.5
                             #:tools (append (tools-pilot 20.0 #f 0.3)
                                             (list (tool-pbolt 10.0)))
                             #:hangar fighters
                             #:cargo (list (make-upgrade 'upgrade "orange" #f #f)
                                           (make-upgrade 'upgrade "orange" #f #f))))
        (set-ship-ai-strategy! f (list (strategy (space-time ownspace) "attack*" (ob-id base))))
        (append! changes (chadd f #f) m)))

    (append! changes (order-changes ownspace real-orders))
    
    changes)
  
  (define (on-message space cmd change-scenario!)
    (define o (find-id space space (anncmd-id cmd)))
    (when (and o (ann-button? o))
      (case (ann-button-msg o)
        (("quit-scenario") (change-scenario!))))
    '())
  
  (values ownspace on-tick on-message on-player-restart))


