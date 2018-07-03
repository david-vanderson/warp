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
  (define-values (rx ry rw rh) (send r get-bounding-box))

  (define asteroids '())
  (define sep 500.0)
  (define sep/2 (/ sep 2))
  (define dd 10.0)
  (for* ((x (in-range (+ rx sep/2) (- rw sep/2) sep))
         (y (in-range (+ ry sep/2) (- rh sep/2) sep))
         #:when (send r in-region? x y))
    (define diam (round (exp (exp (random-between (log (log 25.0)) (log (log 250.0)))))))
    (define xd (random-between (* sep -0.5) (* sep 0.5)))
    (define yd (random-between (* sep -0.5) (* sep 0.5)))
    (define dx (random-between (- dd) dd))
    (define dy (random-between (- dd) dd))
    (define dr (random-between -0.5 0.5))
    (define up? ((random) . < . 0.1))
    (define a (make-ship "asteroid_87" "Asteroid" "_neutral" #:drag 0.1
                         #:size (inexact->exact diam)
                         #:x (+ x xd) #:y (+ y yd) #:dr dr #:dx dx #:dy dy
                         #:con (if up? 50.0 5000.0)
                         #:hp-bar? up?
                         #:cargo (if up?
                                     (list (random-upgrade 0 #f)
                                           (random-upgrade 0 #f))
                                     '())))
    (set-ship-overlays! a (list (cons "Empire" (overlay 'overlay-qm #t))))
    (prepend! asteroids a))

  ; pick which asteroid will have the hidden base
  (define idx (random (length asteroids)))
  (define a (list-ref asteroids idx))
  (set! hidden-base-id (ob-id a))
  (set-ship-hangar! a '())
  (set-ship-cargo! a '())
  (set-stats-con! (ship-stats a) 250.0)
  (set-stats-maxcon! (ship-stats a) 250.0)
  (set-ship-hp-bar?! a #t)
  (set-ship-tools! a (list (tool-regen 1.0)
                           (tool-pbolt 5.0)))

  (define ownspace
    (space (next-id) 0 4000 4000 players '()
           `(
             ,(standard-quit-scenario-button #t)
             ,(ann-text (next-id) 0 #t (posvel 0 -200 -100 0 0 0 0) #f
                        (string-append
                         "On mission to destroy a rebel outpost your engines have failed.\n"
                         "Use probes to search the asteroid field for a hidden base.\n"
                         "Bring replacement parts back to the ship with fighters.\n"
                         "Once mobile again, destroy the outpost on the far side of the field.\n")
                        10000)
             ,@asteroids
             )))

  ; put the spare parts upgrade in the hidden base
  (define parts (upgrade (next-id) (space-time ownspace) #t #f "parts" #f))

  ; the good guys in this scenario
  (define (new-red-fighter)
    (define s (make-ship "red-fighter" "Empire Fighter" "Empire"
                         #:con 100.0))
    (define e (ship-tool s 'engine))
    (set-tool-val! e 55.0)
    (set-ship-tools! s (append (list (tool-regen 1.0)) (ship-tools s)))
    (set-obj-posvel! s #f)
    s)

  (define goodship (make-ship "red-frigate" "a" "a" #:x -1500.0 #:y -1500.0 #:r pi))
  (set-ship-stats! goodship (stats (next-id) "red-frigate" "Empire Frigate" "Empire"
                                   ;con maxcon mass radar drag start
                                   500.0 500.0 100.0 300.0 0.4 #t))

  ; ship starts with pilot tools damaged beyond repair
  ; we will manually remove the dmgs when the engine parts are recovered
  (define enginedmgid (next-id))
  (define warpdmgid (next-id))
  (set-ship-hangar! goodship (list (new-red-fighter)
                                   (new-red-fighter)
                                   (new-red-fighter)))
  (set-ship-tools!
   goodship (append (tools-pilot 25.0 #f 0.4)
                    (list (tool-warp 200.0 80.0)
                          (tool-regen 1.0)
                          (tool-pbolt 10.0)
                          (tool-probe 20.0)
                          (tool-missile 5.0 10.0)
                          (tool-cannon 21.0))))

  (set-tool-dmgs! (ship-tool goodship 'engine) (list (dmg enginedmgid "offline" 10000.0 0 #f)))
  (set-tool-dmgs! (ship-tool goodship 'warp) (list (dmg warpdmgid "offline" 10000.0 0 #f)))
  

  ; the bad guys
  (define (new-blue-fighter ownspace)
    (define s (make-ship "blue-fighter" "Rebel Fighter" "Rebel" #:ai? #t #:con 50.0))
    (set-obj-posvel! s #f)
    (set-ship-ai-strategy! s (scout-strat ownspace))
    s)

  (define (scout-strat ownspace)
    (cond
      (found-base?
       (define hb (find-id ownspace ownspace hidden-base-id))
       (list (strategy (space-time ownspace) "scout" hb)
             (strategy (space-time ownspace) "return" (ob-id enemy-base))))
      (else
       (list (strategy (space-time ownspace) "scout" (obj -1 -1 #t
                                                          (posvel -1 -1500.0 1500.0 500.0
                                                                  0 0 0)))
             (strategy (space-time ownspace) "scout" (obj -1 -1 #t
                                                          (posvel -1 1500.0 -1500.0 500.0
                                                                  0 0 0)))
             (strategy (space-time ownspace) "return" (ob-id enemy-base))))))

  
  (define enemy-base (make-ship "blue-station" "a" "a" #:x 1500.0 #:y 1500.0 #:ai? #t #:hangar '()))
  (set-ship-stats! enemy-base (stats (next-id) "blue-station" "Rebel Outpost" "Rebel"
                               ;con maxcon mass radar drag start-ship?
                               750.0 750.0 1000.0 600.0 0.4 #t))
  (set-ship-tools!
   enemy-base
   (list
    (tool-pbolt 10.0)
    (tool-missile 5.0 10.0)))
  
  (set-space-objects! ownspace (append (space-objects ownspace)
                                       (list goodship enemy-base)))

  (define playing? #t)
  (define found-base? #f)
  (define dock-base? #f)
  (define parts-returned? #f)
  
  (define orders
    (ordercomb #f "" 'and
               (list (alive "Keep Frigate Alive" (ob-id goodship))
                     (ordercomb #f "" 'seq
                                (list (order #f "Find Asteroid Base with Fighters" '()
                                             (lambda (s f o) found-base?))
                                      (order #f "Dock Fighter with Asteroid" '()
                                             (lambda (s f o) dock-base?))
                                      (order #f "Return Parts to Frigate" '()
                                             (lambda (s f o) parts-returned?))
                                      (kill "Destroy Enemy Outpost" (ob-id enemy-base)))))))
  
  (define real-orders (space 0 0 0 0 '() '() '()))  ; only care about orders
  (set-space-orders-for! real-orders "Empire" orders)

  (define (end! win? txt)
    (define changes '())
    (set! playing? #f)  ; end scenario
    (append! changes (chadd (ann-text (next-id) (space-time ownspace) #t
                                      (posvel 0 -200 -100 0 0 0 0) #f
                                      txt #f) #f))
    ; add end scenario button
    (append! changes (chadd (standard-quit-scenario-button) #f))
    changes)

  (define (on-player-restart space pid)
    (define changes '())
    ; this happens during the processing of the client's dying message
    ; so the player is still in their spacesuit because it hasn't taken effect
    (define frig (find-id ownspace ownspace (ob-id goodship)))
    (when frig
      (append! changes (chmov pid (ob-id frig) #f)))
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

    (when (and playing? (not frig))
      (append! changes (end! #f "Cruiser Destroyed, You Lose")))

    (when (and playing? (not eb))
      (append! changes (end! #t "Enemy Outpost Destroyed, You Win!")))

    (when (and found-base?
               (not parts-returned?)
               (not (find-id ownspace ownspace (ob-id parts))))
      (append! changes (end! #f "Parts Lost, You Lose")))
      

    (when playing?
      (when (time-for (space-time ownspace) 55000 0000)
        (define m (message (next-id) (space-time ownspace) #t #f "New Enemy Scout Detected"))
        (define f (new-blue-fighter ownspace))
        (append! changes (chadd f (ob-id enemy-base)) m))

      (when (not (find-id ownspace ownspace
                          (lambda (o) (and (ship? o)
                                           (equal? (ship-type o) "red-fighter")))))
        (define f (new-red-fighter))
        (append! changes (chadd f (ob-id frig))))

      (for ((f (ship-hangar eb)))
        (when (and (not (ship-strategy f))
                   ((current-strat-age ownspace f) . > . 10000))
          ; fighter has been docked without a strat, send them to scout again
          (append! changes (new-strat (ob-id f) (scout-strat ownspace)))))

      (define hb (find-id ownspace ownspace hidden-base-id))

      (for ((s (space-objects ownspace))
            #:when (and (obj-alive? s)
                        (or (spaceship? s) (probe? s))
                        (equal? "Empire" (ship-faction s)))
            (a (qt-retrieve qt (obj-x s) (obj-y s) (/ (ship-radar s) 3.0)))
            #:when (and (obj-alive? a)
                        (spaceship? a)
                        (assoc "Empire" (ship-overlays a))
                        ((distance s a) . < . (/ (ship-radar s) 3.0))))
        ; remove the overlay
        (append! changes (chstat (ob-id a) 'overlay (cons "Empire" #f)))
        (when (not (null? (ship-cargo a)))
          ; add the cargo overlay
          (append! changes (chstat (ob-id a) 'overlay
                                   (cons "Empire" (overlay 'overlay-cargo #t)))))
        ; check if this was the hidden base
        (when (and (not found-base?) (equal? (ob-id hb) (ob-id a)))
          (set! found-base? #t)
          (define newstats (struct-copy stats (ship-stats hb)))
          (set-stats-faction! newstats "Empire")
          (append! changes
                   (chstats (ob-id hb) newstats)
                   (chstat (ob-id hb) 'ai #t)
                   (chadd parts (ob-id hb))
                   (message (next-id) (space-time ownspace) #t #f "Discovered Hidden Base!"))))

      ; check if the good guys docked
      (when (and found-base? (not dock-base?))
        (for/first ((s (in-list (ship-hangar hb))))
          (set! dock-base? #t)
          (append! changes (chmov (ob-id parts) (ob-id s) #f)
                   (message (next-id) (space-time ownspace) #t #f "Parts Transferred to Fighter"))))
      
      ; check if the parts got back to goodship
      (when (and found-base? dock-base? (not parts-returned?))
        (define gs (find-id ownspace ownspace (ob-id goodship)))
        (define p (find-stack ownspace gs (ob-id parts)))
        (when p
          (set! parts-returned? #t)
          ; repair the engine
          (append! changes (chrm (ob-id parts))
                   (chrm enginedmgid) (chrm warpdmgid)
                   (message (next-id) (space-time ownspace) #t #f "Frigate Engine Repaired!"))))
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


