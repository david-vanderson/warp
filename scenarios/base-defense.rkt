#lang racket/base

(require racket/math)

(require "../defs.rkt"
         "../utils.rkt"
         "../ships.rkt"
         "../order.rkt"
         "../upgrade.rkt")

(provide (all-defined-out))

(define (base-defense-scenario oldspace oldtick oldmessage old-on-player-restart)  
  (define players (if oldspace (space-players oldspace) '()))
  (for ((p players)) (set-player-faction! p #f))

  (define ownspace (space (next-id) 0 5000 2000 players '()
                          `(
                            ,(standard-quit-scenario-button)
                            ,(ann-text (next-id) 0 #t (posvel 0 -200 -100 0 0 0 0) #f
                                       (string-append
                                        "Defend your base from the incoming destroyer.\n"
                                        "Use your cruiser to attack, dock on the station to repair.\n"
                                        "Enemy cruisers drop upgrades when killed.")
                                       10000)
                            )))
  
  (define (new-blue-fighter)
    (define s (make-ship "blue-fighter" "a" "a" #:ai? #t))
    (set-ship-stats! s (stats (next-id) "blue-fighter" "Rebel Fighter" "Rebel"
                              ;con maxcon mass radar drag start-ship?
                              50.0 50.0 20.0 300.0 0.4 #f))
    (set-ship-tools!
     s (append (tools-pilot 50.0 #f 1.5)
               (list (tool-pbolt 8.0) (tool-regen 1.0))))
    (set-obj-posvel! s #f)
    s)
  
  (define (new-red-fighter)
    (define s (make-ship "red-fighter" "a" "a" #:ai? #t))
    (set-ship-stats! s (stats (next-id) "red-fighter" "Empire Fighter" "Empire"
                              ;con maxcon mass radar drag start
                              20.0 20.0 20.0 300.0 0.4 #f))
    (set-ship-tools!
     s (append (tools-pilot 50.0 #f 2.0)
               (list (tool-pbolt 8.0))))
    (set-obj-posvel! s #f)
    s)
  
  
  (define cruiser (make-ship "blue-cruiser" "z" "z" #:x -1800 #:y -50 #:ai? #f
                             #:hangar (list (new-blue-fighter))))
  (set-ship-stats! cruiser (stats (next-id) "blue-cruiser" "Rebel Cruiser" "Rebel"
                                  ;con maxcon mass radar drag start?
                                  200.0 200.0 100.0 500.0 0.4 #t))
  (set-ship-tools!
   cruiser (append (tools-pilot 25.0 #f 1.0)
                   (list (tool-pbolt 10.0)
                         (tool-probe 10.0)
                         (tool-missile 5.0 10.0)
                         (tool-cannon 21.0)
                         (tool-warp 200.0 80.0)
                         (tool-regen 1.0))))
  
  
  (define base (make-ship "blue-station" "a" "a" #:x -2000 #:y -100 #:ai? #t #:hangar '()
                          #:dr 0.1))
  (set-ship-stats! base (stats (next-id) "blue-station" "Rebel Outpost" "Rebel"
                               ;con maxcon mass radar drag start-ship?
                               1000.0 1000.0 1000.0 1000.0 0.4 #t))
  (set-ship-tools!
   base (list (tool-pbolt 10.0)
              (tool-probe 30.0)
              (tool-missile 5.0 10.0)))
  
  
  (define destroyer (make-ship "red-destroyer" "b" "b" #:x 2400 #:y 100 #:r pi #:ai? #t
                               #:hangar '()))
  (set-ship-stats! destroyer (stats (next-id)
                                    ;type name faction
                                    "red-destroyer" "Empire Destroyer" "Empire"
                                    ;con maxcon mass radar drag start?
                                    1000.0 1000.0 500.0 1000.0 0.4 #f))
  (set-ship-tools!
   destroyer (append (tools-pilot 6.0 #f 0.1 #:dock? #f)
                     (list (tool-pbolt 10.0)
                           (tool-missile 5.0 10.0)
                           (tool-cannon 21.0))))
  
  (set-ship-ai-strategy! destroyer
                         (list (strategy (space-time ownspace) "attack-only" (ob-id base))))
  
  (set-space-objects! ownspace (append (space-objects ownspace) (list cruiser base destroyer #;special)))

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
      
      (append! changes (chadd (ann-text (next-id) (space-time ownspace) #t
                                        (posvel 0 -200 -100 0 0 0 0) #f
                                        txt #f) #f))
      ; add end scenario button
      (append! changes (chadd (standard-quit-scenario-button) #f))
      )

    (when (and playing? hb eb)
      (when (time-for (space-time ownspace) 55000 0000)
        (define m (message (next-id) (space-time ownspace) #t #f "New Fighter at Outpost"))
        (define f (new-blue-fighter))
        (append! changes (chadd f (ob-id base)) m))
      
      (when (time-for (space-time ownspace) 65000 20000)
        (define f (new-red-fighter))
        (append! changes (chadd f (ob-id destroyer))))
      
      (when (time-for (space-time ownspace) 90000 10000)
        (define m (message (next-id) (space-time ownspace) #t #f "Empire Frigate Incoming"))
        (define x (+ (/ (space-width ownspace) 2) 100))
        (define y (random-between (- (/ (space-height ownspace) 2)) (/ (space-height ownspace) 2)))
        (define fighters (for/list ((i (random 3)))
                           (make-ship "red-fighter" "Empire Fighter" "Empire")))
        (define f (make-ship "red-frigate" "Empire Frigate" "Empire" #:x x #:y y #:r pi #:ai? #t
                             #:hangar fighters #:cargo (list (random-upgrade 0 #f)
                                                                (random-upgrade 0 #f))))
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


