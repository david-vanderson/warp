#lang racket/base

(require racket/math
         racket/class
         racket/draw)

(require "../defs.rkt"
         "../utils.rkt"
         "../quadtree.rkt"
         "../ships.rkt"
         "../order.rkt"
         "../change.rkt"
         "../upgrade.rkt")

(provide (all-defined-out))

(define (racketcon2018-scenario oldspace oldtick oldmessage old-on-player-restart)

  (define players (if oldspace (space-players oldspace) '()))
  (for ((p players)) (set-player-faction! p "Undecided"))

  (define team1 "Brackets")
  (define base1id #f)
  (define base1x #f)
  (define base1y #f)
  (define team1-num 0)
  (define team1-score 0)
  
  (define team2 "Parens")
  (define base2id #f)
  (define base2x #f)
  (define base2y #f)
  (define team2-num 0)
  (define team2-score 0)

  (define countdown? #f)
  (define base1-destroyed? #f)
  (define base2-destroyed? #f)

  (define (start-xy ownspace team)
    (define dx (random-between -100 100))
    (define dy (random-between -200 200))
    (define-values (x y)
      (cond
        ((equal? team team1)
         (values (+ base1x -300) base1y))
        ((equal? team team2)
         (values (+ base2x 300) base2y))
        (else
         (error 'start-xy "team not recognized: ~a" team))))
    (values (+ x dx) (+ y dy)))

  (define teams (list team1 team2))

  (define (get-team-color team)
    (cond
      ((equal? team team1) "blue")
      ((equal? team team2) "red")
      (else
       (error 'get-team-color "team not recognized: ~a" team))))

  (define (make-base team x y enemyid)
    (define aifighter (make-ship
                       (string-append (get-team-color team) "-fighter")
                       (string-append team " Fighter") team
                       #:price 3 #:ai 'always
                       #:hull 50 #:mass 20 #:drag 0.4
                       #:tools (append (tools-pilot 50.0 #f 1.4)
                                       (list (tool-pbolt 8.0)
                                             (tool-regen 1.0)))
                       #:ai-strats (list (strategy 0 "attack*" enemyid))))

    (define fighter (make-ship
                     (string-append (get-team-color team) "-fighter")
                     (string-append team " Fighter") team
                     #:price 5 #:hull 50 #:mass 20 #:drag 0.4
                     #:tools (append (tools-pilot 60.0 #f 1.6)
                                     (list (tool-pbolt 8.0)
                                           (tool-regen 1.0)))))

    (define frigate (make-ship
                     (string-append (get-team-color team) "-frigate")
                     (string-append team " Frigate") team
                     #:hangar '() #:price 10 #:radar 400
                     #:hull 100 #:mass 50 #:drag 0.3
                     #:tools (append (tools-pilot 35.0 #f 1.0)
                                     (list (tool-pbolt 8.0)
                                           (tool-mine 30.0)
                                           (tool-warp 250.0 50.0)
                                           (tool-missile 5.0 10.0)
                                           (tool-regen 1.0)))))

    (define cruiser (make-ship
                     (string-append (get-team-color team) "-cruiser")
                     (string-append team " Cruiser") team
                     #:hangar '() #:price 25 #:radar 500
                     #:hull 150 #:mass 100 #:drag 0.4
                     #:tools (append (tools-pilot 25.0 #f 0.7)
                                     (list (tool-pbolt 8.0)
                                           (tool-probe 10.0)
                                           (tool-missile 5.0 10.0)
                                           (tool-cannon 21.0)
                                           (tool-warp 250.0 80.0)
                                           (tool-regen 1.0)))))

    (make-ship (string-append (get-team-color team) "-station")
               (string-append "Base " team) team
               #:x x #:y y #:radar 500
               #:ai 'always #:hangar '() #:dr 0.1
               #:hull 500 #:mass 1000 #:drag 0.4 #:start-ship? #t
               #:tools (list (tool-pbolt 10.0)
                             (tool-probe 30.0)
                             (tool-missile 5.0 10.0)
                             (tool-regen 1.0)
                             (tool-factory 20 (list aifighter
                                                    fighter
                                                    frigate
                                                    cruiser)))))

  (define (new-fighter p x y)
    (define faction (player-faction p))
    (define type (string-append (get-team-color faction) "-fighter"))
    (define name (string-append (player-name p) " fighter"))
    (make-ship type name faction
               #:x x #:y y #:price 5
               #:hull 100 #:mass 20 #:drag 0.4
               #:tools (append (tools-pilot 60.0 #f 1.6)
                               (list (tool-pbolt 8.0)
                                     (tool-regen 1.0)))))

  (define (place-player p x y)
    (define f (new-fighter p x y))
    (list (chadd f #f)
          (chmov (ob-id p) (ob-id f) #f)))

  (define team1-button-id #f)
  (define team2-button-id #f)
  (define (redo-team-buttons [old? #t])
    (define changes '())
    (when (and old? team1-button-id)
      (append! changes (chrm team1-button-id)))
    (when (and old? team2-button-id)
      (append! changes (chrm team2-button-id)))
    (define b1 (make-ann-button -200 0 200 100
                                (string-append "Team " team1 "\n" (number->string team1-num))
                                team1
                                #:tab? #f #:faction "Undecided"))
    (set! team1-button-id (ob-id b1))
    (define b2 (make-ann-button 200 0 200 100
                                (string-append "Team " team2 "\n" (number->string team2-num))
                                team2
                                #:tab? #f #:faction "Undecided"))
    (set! team2-button-id (ob-id b2))
    (append! changes
             (chadd b1 #f)
             (chadd b2 #f))
    changes)

  (define score-txtid #f)
  (define (redo-team-scores [old? #t])
    (define changes '())
    (when (and old? score-txtid)
      (append! changes (chrm score-txtid)))
    (define s (make-ann-text 10 100 0 #f #f #:pos 'topleft
                             (string-append "Team " team1 ": " (number->string team1-score) "\n"
                                            "Team " team2 ": " (number->string team2-score))))
    (set! score-txtid (ob-id s))
    (append! changes (chadd s #f))
    changes)

  (define (make-asteroid diam x y dx dy dr)
    (make-ship "asteroid" "Asteroid" "_neutral" #:drag 0.1
               #:size diam #:x x #:y y #:dr dr #:dx dx #:dy dy
               #:hull 5000 #:invincible? #t))

  (define (make-mine x y)
    (make-ship "mine" "Mine" "_mine" #:x x #:y y
               #:hull 100 #:radar 75 #:drag 0.6
               #:tools (append (tools-pilot 15.0 #f #f)
                               (list (tool-regen 0.5)))))
  
  (define derelict
    (make-ship "red-destroyer" "Destroyer" "_neutral"
               #:x 0 #:y 0 #:r (random-between 0.0 pi)
               #:hangar '() #:radar 700
               #:hull 250 #:mass 500 #:drag 0.4
               #:tools (append (tools-pilot 20.0 #f 0.5 #:dock? #f)
                               (list (tool-pbolt 12.0)
                                     (tool-warp 250.0 100.0)
                                     (tool-missile 10.0 20.0)
                                     (tool-cannon 30.0)
                                     (tool-probe 10.0)
                                     (tool-regen 1.0)))))

  (define (start-space oldspace)

    (define ownspace
      (space (next-id) 0 8000 8000 (space-players oldspace) '() '()))

    (define nebulas
      (let ((r (new region%)))
        (send r set-ellipse -1500 -1000 3500 2000)
        (nebula-region r)))

    (define upper-asteroids
      (let ((r (new region%)))
        (send r set-polygon
              '((-2000.0 . 2000.0)
                (-2000.0 . 4000.0)
                (2000.0 . 4000.0)
                (2000.0 . 2000.0)))
        (asteroid-region r make-asteroid)))

    (define lower-asteroids
      (let ((r (new region%)))
        (send r set-polygon
              '((-2000.0 . -2000.0)
                (-2000.0 . -4000.0)
                (2000.0 . -4000.0)
                (2000.0 . -2000.0)))
        (asteroid-region r make-asteroid)))

    (define lower-mines
      (let ((r (new region%)))
        (send r set-polygon
              '((-500.0 . -1000.0)
                (500.0 . -1000.0)
                (500.0 . -2000.0)
                (-500.0 . -2000.0)))
        (mine-region r make-mine)))

    (define upper-mines
      (let ((r (new region%)))
        (send r set-polygon
              '((-500.0 . 1000.0)
                (-500.0 . 2000.0)
                (500.0 . 2000.0)
                (500.0 . 1000.0)))
        (mine-region r make-mine)))
    (set-space-objects! ownspace (append upper-mines
                                         lower-mines
                                         upper-asteroids
                                         lower-asteroids
                                         nebulas))

    (define changes '())

    (define coords (random-corner ownspace))
    (set-posvel-x! (obj-posvel derelict) (car coords))
    (set-posvel-y! (obj-posvel derelict) (cdr coords))

    (append! changes (chadd derelict #f))
    
    ; add standard stuff
    (append! changes (chadd (make-ann-button 196 76 80 40 "Quit" "quit-scenario"
                                             #:pos 'topleft
                                             #:tab? #t #:faction #f) #f))
    (append! changes (chadd (make-ann-button 294 76 100 40 "Restart" "restart"
                                             #:pos 'topleft
                                             #:tab? #t #:faction #f) #f))
    (append! changes (chadd (make-ann-button 0 200 200 100 "Observer" "Observer"
                                             #:faction "Undecided") #f))
    (append! changes (chadd (make-ann-button 78 76 140 40 "Switch Team" "Undecided"
                                             #:pos 'topleft #:tab? #t) #f))
    (append! changes (redo-team-buttons #f))

    (set! base1id (next-id))
    (set! base2id (next-id))

    (define dist (- (/ (space-width ownspace) 2.0) 1000))

    ; add team1 base
    (define t1 (random-between (+ pi (/ pi/2 2)) (- pi (/ pi/2 2))))
    (set! base1x (* dist (cos t1)))
    (set! base1y (* dist (sin t1)))
    (define b1 (make-base team1 base1x base1y base2id))
    (set-ob-id! b1 base1id)
    (append! changes (chadd b1 #f))

    ; add team2 base
    (define t2 (random-between (- (/ pi/2 2)) (/ pi/2 2)))
    (set! base2x (* dist (cos t2)))
    (set! base2y (* dist (sin t2)))
    (define b2 (make-base team2 base2x base2y base1id))
    (set-ob-id! b2 base2id)
    (append! changes (chadd b2 #f))

    ; add players
    (for ((p (in-list (space-players ownspace)))
          #:when (member (player-faction p) teams))
      (define-values (x y) (start-xy ownspace (player-faction p)))
      (append! changes (place-player p x y)))

    (append! changes (redo-team-scores #f))

    (set! countdown? #f)
    (set! base1-destroyed? #f)
    (set! base2-destroyed? #f)

    (apply-all-changes! ownspace changes "scenario")
    
    ownspace)
  

  (define (on-player-restart ownspace pid)
    (define changes '())
    ; this happens after the player's spacesuit has been removed
    (define p (findfid pid (space-players ownspace)))
    (define-values (x y) (start-xy ownspace (player-faction p)))
    (append! changes (place-player p x y))
    changes)


  (define countdown-time 20000)
  (define (start-countdown! ownspace team)
    (define changes '())
    (when (not countdown?)
      (set! countdown? (space-time ownspace))
      (define a (make-ann-text -100 -100 0 #f #f (string-append "Team " team " Wins!")))
      (append! changes (chadd a #f))
      (for ((fac (cons "Observer" teams)))
        (append! changes
                 (chorders fac
                           (ordertime #f "Restart in ~a" '() #f
                                      countdown-time (space-time ownspace) #f)))))
    changes)

  (define (add-factory-pt base)
    (define pts (car (tool-val (ship-tool base 'factory))))
    (chstat (ob-id base) 'toolval
            (list 'factory (add1 pts))))

  (define (random-corner ownspace)
    (define w (/ (space-width ownspace) 2.0))
    (define h (/ (space-height ownspace) 2.0))
    (define dx (random-between 0.0 100.0))
    (define dy (random-between 0.0 100.0))
    (define lst (list (cons (+ w dx) (+ h dy))
                      (cons (+ w dx) (- (- h) dy))
                      (cons (- (- w) dx) (- (- h) dy))
                      (cons (- (- w) dx) (+ h dy))))
    (list-ref lst (random (length lst))))

  (define (add-upgrade-asteroids ownspace)
    (define changes '())
    (when (time-for (space-time ownspace) 30000 0)
      (define coords (random-corner ownspace))
      (define a (make-ship "asteroid" "Asteroid" "_neutral" #:drag 0.2
                           #:size 50 #:x (car coords) #:y (cdr coords)
                           #:dr (random-between -0.1 0.1)
                           #:hull 100
                           #:cargo (list (make-upgrade 0 'upgrade "orange" #f #f))))
      (append! changes (chadd a #f)))
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

    (define t1num 0)
    (define t2num 0)

    (for ((p (space-players ownspace)))
      (cond
        ((not (player-faction p))
         ; new player
         (append! changes (chfaction (ob-id p) "Undecided")))
        ((equal? team1 (player-faction p))
         (set! t1num (add1 t1num)))
        ((equal? team2 (player-faction p))
         (set! t2num (add1 t2num)))))

    (when (or (not (equal? team1-num t1num))
              (not (equal? team2-num t2num)))
      (set! team1-num t1num)
      (set! team2-num t2num)
      (append! changes (redo-team-buttons)))

    
    (define addpts? (time-for (space-time ownspace) 5000 0))

    (when (not base1-destroyed?)
      (define t1b (find-top-id ownspace base1id))
      (cond
        (t1b
         (when addpts?
           (append! changes (add-factory-pt t1b))))
        (else
         (set! base1-destroyed? #t)
         (set! team2-score (add1 team2-score))
         (append! changes (redo-team-scores))
         (append! changes (start-countdown! ownspace team2)))))
      
    (when (not base2-destroyed?)
      (define t2b (find-top-id ownspace base2id))
      (cond
        (t2b
         (when addpts?
           (append! changes (add-factory-pt t2b))))
         (else
          (set! base2-destroyed? #t)
          (set! team1-score (add1 team1-score))
          (append! changes (redo-team-scores))
          (append! changes (start-countdown! ownspace team1)))))
        
    (when countdown?
      (define time-since ((space-time ownspace) . - . countdown?))
      (when (time-for time-since 300)
        (append! changes
                 (chadd
                  (make-ann-anim "red-fireworks" #f #f 150) #f)))
      (when (time-for time-since 300 150)
        (append! changes
                 (chadd
                  (make-ann-anim "blue-fireworks" #f #f 150) #f)))
      (when (time-since . > . countdown-time)
        (change-scenario! (lambda (oldspace . xs)
                            (set! changes '())
                            (values (start-space oldspace)
                                    on-tick on-message on-player-restart)))))

    (append! changes (add-upgrade-asteroids ownspace))

    (for ((s (space-objects ownspace))
            #:when (obj-alive? s))
      (cond
        ((upgrade? s)
         (for ((a (qt-retrieve qt (obj-x s) (obj-y s) (upgrade-radius ownspace s)))
               #:when (and (obj-alive? a)
                           (spaceship? a)))
           (append! changes (upgrade-hit-ship ownspace a s))))
        ((and (equal? (ob-id derelict) (ob-id s))
              (equal? (ship-faction s) "_neutral"))
         ; derelict hasn't been claimed
         (for/first ((a (qt-retrieve qt (obj-x s) (obj-y s) (+ (ship-radius s) 50.0)))
                     #:when (and (obj-alive? a)
                                 (spaceship? a)
                                 (member (ship-faction a) teams)))
           (append! changes
                    (chfaction (ob-id s) (ship-faction a))
                    (make-message ownspace (string-append (ship-faction a)
                                                          " found derelict destroyer!")))))))
    
    changes)
  
  (define (on-message space cmd change-scenario!)
    (define changes '())
    (define o (find-id space space (anncmd-id cmd)))
    (when (and o (ann-button? o))
      (cond
        ((equal? "quit-scenario" (ann-button-msg o))
         (change-scenario!))
        ((equal? "restart" (ann-button-msg o))
         (change-scenario! (lambda (oldspace . xs)
                             (values (start-space oldspace)
                                     on-tick on-message on-player-restart))))
        ((member (ann-button-msg o) teams)
         (append! changes (chfaction (anncmd-pid cmd) (ann-button-msg o)))
         
         (define p (struct-copy player (findfid (anncmd-pid cmd) (space-players space))))
         (set-player-faction! p (ann-button-msg o))
         (define-values (x y) (start-xy space (player-faction p)))
         (append! changes (place-player p x y)))
        ((member (ann-button-msg o) '("Observer" "Undecided"))
         (append! changes (chfaction (anncmd-pid cmd) (ann-button-msg o)))
         (when (equal? (ann-button-msg o) "Undecided")
           ; player is leaving their team, if they are on a ship, remove them
           (append! changes (chmov (anncmd-pid cmd) #f #f))))))
    changes)
  
  (values (start-space oldspace) on-tick on-message on-player-restart))


