#lang racket/base

(require racket/math
         racket/class
         racket/draw)

(require "../defs.rkt"
         "../utils.rkt"
         "../quadtree.rkt"
         "../ships.rkt"
         "../order.rkt"
         "../upgrade.rkt")

(provide (all-defined-out))

(define (asteroid-search-scenario oldspace oldtick oldmessage old-on-player-restart)
  
  (define players (if oldspace (space-players oldspace) '()))
  (for ((p players)) (set-player-faction! p #f))

  (define hidden-base-id #f)

  (define r (new region%))
  (send r set-polygon
        '((-1000.0 . -2000.0)
          (-2000.0 . 2000.0)
          (1000.0 . 2000.0)
          (2000.0 . -2000.0)))
  (define asteroids
    (asteroid-region r
      (lambda (diam x y dx dy dr)
        (define up? ((random) . < . 0.1))
        (make-ship "asteroid" "Asteroid" "_neutral" #:drag 0.1
                   #:size diam
                   #:x x #:y y #:dr dr #:dx dx #:dy dy
                   #:hull (if up? 50 5000)
                   #:invincible? #t
                   #:cargo (if up?
                               (list (make-upgrade 0 'upgrade "orange" #f #f)
                                     (make-upgrade 0 'upgrade "orange" #f #f))
                               '())
                   #:overlays (list (cons "Empire" (overlay 'overlay-qm #t)))))))

  ; pick which asteroid will have the hidden base
  (define idx (random (length asteroids)))
  (define a (list-ref asteroids idx))
;  (set! a (for/first ((o asteroids)
;                      #:when (and ((obj-x o) . < . -1000)
;                                  ((obj-y o) . < . -1000)))
;            o))
  (set! hidden-base-id (ob-id a))
  (set-ship-hangar! a '())
  (set-ship-cargo! a '())
  (set-ship-con! a 250.0)
  (set-ship-maxcon! a 250.0)
  (set-ship-invincible?! a #f)
  (set-ship-tools! a (list (tool-regen 1.0)
                           (tool-pbolt 5.0)))

  (define ownspace
    (space (next-id) 0 4000 4000 players '()
           `(
             ,(standard-quit-scenario-button)
             ,(make-ann-text -200 -100 0 10000 15000
                             (string-append
                              "On mission to destroy a rebel outpost your engines have failed.\n"
                              "Use probes to search the asteroid field for a hidden base.\n"
                              "Bring replacement parts back to the ship with fighters.\n"
                              "Once mobile again, destroy the outpost on the far side of the field.\n"))
             ,@asteroids
             )))

  ; put the spare parts upgrade in the hidden base
  (define parts (make-upgrade 0 'parts "yellow" #f #f))

  ; the good guys in this scenario
  (define (new-red-fighter)
    (make-ship "red-fighter" "Empire Fighter" "Empire"
               #:hull 100 #:mass 20 #:drag 0.5
               #:tools (append (tools-pilot 55.0 #f 1.5)
                               (list (tool-pbolt 8.0)
                                     (tool-regen 1.0)))))

  (define goodship (make-ship "red-cruiser" "Empire Frigate" "Empire"
                              #:x -1500 #:y -1500 #:r pi
                              #:hull 300 #:mass 100 #:drag 0.4 #:start-ship? #t
                              #:hangar (list (new-red-fighter)
                                             (new-red-fighter)
                                             (new-red-fighter))
                              #:tools (append (tools-pilot 25.0 #f 0.4)
                                              (list (tool-warp 200.0 80.0)
                                                    (tool-regen 1.0)
                                                    (tool-pbolt 10.0)
                                                    (tool-probe 20.0)
                                                    (tool-missile 5.0 10.0)
                                                    (tool-cannon 21.0)))))

  ; ship starts with pilot tools damaged beyond repair
  ; we will manually remove the dmgs when the engine parts are recovered
  (define enginedmgid (next-id))
  (define warpdmgid (next-id))
  (set-tool-dmgs! (ship-tool goodship 'engine) (list (dmg enginedmgid "offline" 10000.0 0 #f)))
  (set-tool-dmgs! (ship-tool goodship 'warp) (list (dmg warpdmgid "offline" 10000.0 0 #f)))
  

  ; the bad guys
  (define (new-blue-fighter ownspace)
    (make-ship "blue-fighter" "Rebel Fighter" "Rebel" #:ai 'always
               #:hull 50 #:mass 20 #:drag 0.5
               #:tools (append (tools-pilot 50.0 #f 1.5)
                               (list (tool-pbolt 8.0)))
               #:ai-strats (scout-strats ownspace)))

  (define (scout-strats ownspace)
    (define hb (find-top-id ownspace hidden-base-id))
    (cond
      ((and found-base? hb)
       (list (strategy (space-time ownspace) "attack*" (ob-id hb))
             (strategy (space-time ownspace) "return" (ob-id enemy-base))))
      (else
       (list (strategy (space-time ownspace) "scout" (pvobj -1500.0 1500.0 500.0))
             (strategy (space-time ownspace) "scout" (pvobj 1500.0 -1500.0 500.0))
             (strategy (space-time ownspace) "return" (ob-id enemy-base))))))

  
  (define enemy-base (make-ship "blue-station" "Rebel Outpost" "Rebel"
                                #:x 1500 #:y 1500 #:ai 'always
                                #:dr 0.1 #:radar 600 #:hangar '()
                                #:hull 750 #:mass 1000 #:drag 0.4
                                #:tools (list
                                         (tool-pbolt 10.0)
                                         (tool-missile 5.0 10.0))))
  
  (set-space-objects! ownspace (append (space-objects ownspace)
                                       (list goodship enemy-base)))

  (define playing? #t)
  (define found-base? #f)
  (define dock-base? #f)
  (define parts-returned? #f)
  
  (define orders
    (ordercomb #f "" 'seq
               (list (order #f "Find Asteroid Base with Fighters" '()
                            (lambda (s f o) found-base?))
                     (order #f "Dock Fighter with Asteroid" '()
                            (lambda (s f o) dock-base?))
                     (order #f "Return Parts to Frigate" '()
                            (lambda (s f o) parts-returned?))
                     (kill "Destroy Enemy Outpost" (ob-id enemy-base)))))
  
  (define real-orders (space 0 0 0 0 '() '() '()))  ; only care about orders
  (set-space-orders-for! real-orders "Empire" orders)

  (define (end! win? txt)
    (define changes '())
    (set! playing? #f)  ; end scenario
    (append! changes (chadd (make-ann-text -200 -100 (space-time ownspace) #f #f txt) #f))
    ; add end scenario button
    (append! changes (chadd (standard-quit-scenario-button #f) #f))
    changes)

  (define (on-player-restart space pid)
    (define changes '())
    ; this happens during the processing of the client's dying message
    ; so the player is still in their spacesuit because it hasn't taken effect
    (define frig (find-id ownspace ownspace (ob-id goodship)))
    (when frig
      (append! changes (chmov pid (ob-id frig) #f)))
    changes)

  (define (upgrade-hit-ship space ship u)
    (define changes '())
    ;(printf "upgrade hit ship ~a ~a\n" (ship-name ship) (upgrade-type u))
    (case (upgrade-type u)
      ((upgrade)
       (append! changes
                (upgrade-ship-random space ship)
                (chrm (ob-id u))))
      ((parts)
       (cond
         ((equal? "Empire" (ship-faction ship))
          (when (equal? (ob-id ship) hidden-base-id)
            ; fighter died right next to base and the parts hit it?
            (set! dock-base? #f))
          (append! changes
                   (chmov (ob-id u) (ob-id ship) #f)
                   (chadd (ship-msg space ship "got parts") #f)))
         (else
          ; push upgrade away so it's not stuck under an asteroid
          (define t (theta ship u))
          (set-posvel-t! (obj-posvel u) #t)
          (set-posvel-dx! (obj-posvel u) (+ (obj-dx u) (* 1.0 (cos t))))
          (set-posvel-dy! (obj-posvel u) (+ (obj-dy u) (* 1.0 (sin t))))))))
    changes)
  
  ; return a list of changes
  (define (on-tick ownspace qt change-scenario!)
    (define changes '())

    (define frig (find-id ownspace ownspace (ob-id goodship)))
    (define eb (find-id ownspace ownspace (ob-id enemy-base)))

    (for ((p (space-players ownspace)))
      (when (not (player-faction p))
        ; new player
        (append! changes (chfaction (ob-id p) "Empire"))
        (when frig
          (append! changes (chmov (ob-id p) (ob-id frig) #f)))))

    (for ((fo (space-orders real-orders)))
      (check ownspace (car fo) (cadr fo)))

    (when playing?

      (when (not frig)
        (append! changes (end! #f "Cruiser Destroyed, You Lose")))

      (when (not eb)
        (append! changes (end! #t "Enemy Outpost Destroyed, You Win!")))

      ; once we've found the base, the parts can't be destroyed,
      ; but might be picked up by the enemy
      ; you lose if they make it back to base
      (when (and found-base?
                 (not parts-returned?))
        (define s (find-stack ownspace ownspace (ob-id parts)))
        (define ship (get-topship s))
        (when (and ship (equal? (ob-id ship) (ob-id enemy-base)))
          (append! changes (end! #f "Parts Lost, You Lose"))))
    
      (when (time-for (space-time ownspace) 55000 0000)
        (define m (make-message ownspace "New Enemy Scout Detected"))
        (define f (new-blue-fighter ownspace))
        (append! changes (chadd f (ob-id enemy-base)) m))

      (when (and frig
                 (not (find-id ownspace ownspace
                               (lambda (o) (and (ship? o)
                                                (equal? (ship-type o) "red-fighter"))))))
        (define f (new-red-fighter))
        (append! changes (chadd f (ob-id frig))))

      (when eb
        (for ((f (ship-hangar eb)))
          (when (and (not (ship-strategy f))
                     ((current-strat-age ownspace f) . > . 10000))
            ; fighter has been docked without a strat, send them to scout again
            (append! changes (new-strat (ob-id f) (scout-strats ownspace))))))

      (define hb (find-top-id ownspace hidden-base-id))

      (for ((s (space-objects ownspace))
            #:when (obj-alive? s))
        (cond
          ((and (or (spaceship? s) (probe? s))
                (equal? "Empire" (ship-faction s)))
           (for ((a (qt-retrieve qt (obj-x s) (obj-y s) (+ (/ (ship-radar s) 3.0) 50.0)))
                 #:when (and (obj-alive? a)
                             (spaceship? a)
                             (assoc "Empire" (ship-overlays a))
                             ((distance s a) . < . (+ (ship-radius a) (/ (ship-radar s) 3.0)))))
             ; remove the overlay
             (append! changes (chstat (ob-id a) 'overlay (cons "Empire" #f)))
             (when (not (null? (ship-cargo a)))
               ; add the cargo overlay and remove invincibility
               (append! changes (list (chstat (ob-id a) 'invincible #f)
                                      (chstat (ob-id a) 'overlay
                                              (cons "Empire" (overlay 'overlay-cargo #t))))))
             ; check if this was the hidden base
             (when (and (not found-base?) (equal? hidden-base-id (ob-id a)))
               (set! found-base? #t)
               (append! changes
                        (chfaction (ob-id hb) "Empire")
                        (chstat (ob-id hb) 'ai 'always)
                        (chadd parts (ob-id hb))
                        (make-message ownspace "Discovered Hidden Base!")))))
          ((upgrade? s)
           (for ((a (qt-retrieve qt (obj-x s) (obj-y s) (upgrade-radius ownspace s)))
                 #:when (and (obj-alive? a)
                             (spaceship? a)
                             (close? s a (+ (ship-radius a) (upgrade-radius ownspace s)))))
             (append! changes (upgrade-hit-ship ownspace a s))))))
          
      ; check if the good guys docked
      (when (and found-base? (not dock-base?) hb)
        (for/first ((s (in-list (ship-hangar hb))))
          (set! dock-base? #t)
          (append! changes (chmov (ob-id parts) (ob-id s) #f)
                   (make-message ownspace "Parts Transferred to Fighter"))))
      
      ; check if the parts got back to goodship
      (when (and found-base? dock-base? (not parts-returned?))
        (define gs (find-id ownspace ownspace (ob-id goodship)))
        (define p (find-stack ownspace gs (ob-id parts)))
        (when p
          (set! parts-returned? #t)
          ; repair the engine
          (append! changes (chrm (ob-id parts))
                   (chrm enginedmgid) (chrm warpdmgid)
                   (make-message ownspace "Frigate Engine Repaired!"))))
      )

    (append! changes (order-changes ownspace real-orders))
    
    changes)
  
  (define (on-message space cmd change-scenario!)
    (define o (find-id space space (anncmd-id cmd)))
    (when (and o (ann-button? o))
      (case (ann-button-msg o)
        (("quit-scenario") (change-scenario!))))
    '())
  
  (values ownspace on-tick on-message on-player-restart))


