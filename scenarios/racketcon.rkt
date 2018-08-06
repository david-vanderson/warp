#lang racket/base

(require racket/math)

(require "../defs.rkt"
         "../utils.rkt"
         "../ships.rkt"
         "../order.rkt"
         "../change.rkt"
         "../upgrade.rkt")

(provide (all-defined-out))

(define (racketcon-scenario oldspace oldtick oldmessage old-on-player-restart)  
  (define players (if oldspace (space-players oldspace) '()))
  (for ((p players)) (set-player-faction! p "undecided"))

  (define team1 "Brackets")
  (define base1id #f)
  (define base1x -500)
  (define team1-button-id #f)
  (define team1-num 0)
  
  (define team2 "Parens")
  (define base2id #f)
  (define base2x 500)
  (define team2-button-id #f)
  (define team2-num 0)

  (define (start-xy ownspace team)
    (define dx (random-between -100 100))
    (define dy (random-between -200 200))
    (define-values (x y)
      (cond
        ((equal? team team1)
         (define b (find-top-id ownspace base1id))
         (if b
             (values (+ (obj-x b) -300) (obj-y b))
             (values (+ base1x -300) 0)))
        ((equal? team team2)
         (define b (find-top-id ownspace base2id))
         (if b
             (values (+ (obj-x b) 300) (obj-y b))
             (values (+ base2x 300) 0)))
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

  (define ownspace (space (next-id) 0 6000 6000 players '() '()))

  (define (make-base team x y)
    (define type (string-append (get-team-color team) "-station"))
    (define b (make-ship type (string-append "Base " team) team
                         #:x x #:y y
                         #:ai 'always #:hangar '() #:dr 0.1))    
    (set-ship-stats! b (stats (next-id) (ship-type b) (ship-name b) (ship-faction b)
                              ;con maxcon mass radar drag start-ship?
                              100.0 100.0 1000.0 500.0 0.4 #t))
    (set-ship-tools! b
                     (list (tool-pbolt 10.0)
                           (tool-probe 30.0)
                           (tool-missile 5.0 10.0)))
    b)

  (define (new-fighter p x y)
    (define faction (player-faction p))
    (define type (string-append (get-team-color faction) "-fighter"))
    (define s (make-ship type (player-name p) faction
                         #:x x #:y y))
    (set-ship-stats! s (stats (next-id) (ship-type s) (ship-name s) (ship-faction s)
                              ;con maxcon mass drag radar start?
                              100.0 100.0 20.0 300.0 0.4 #f))
    (set-ship-tools! s
                     (append (tools-pilot 50.0 #f 1.5)
                             (list (tool-pbolt 80.0)
                                   (tool-regen 1.0))))
    s)

  (define (place-player p x y)
    (define f (new-fighter p x y))
    (list (chadd f #f)
          (chmov (ob-id p) (ob-id f) #f)))

  (define (redo-team-buttons)
    (define changes '())
    (when team1-button-id
      (append! changes (chrm team1-button-id)))
    (when team2-button-id
      (append! changes (chrm team2-button-id)))
    (define b1 (ann-button (next-id) 0 #t (posvel 'center -200 0 0 200 100 0)
                          #f "undecided"
                          (string-append "Team " team1 "\n\n" (number->string team1-num))
                          team1))
    (set! team1-button-id (ob-id b1))
    (define b2 (ann-button (next-id) 0 #t (posvel 'center 200 0 0 200 100 0)
                          #f "undecided"
                          (string-append "Team " team2 "\n\n" (number->string team2-num))
                          team2))
    (set! team2-button-id (ob-id b2))
    (append! changes
             (chadd b1 #f)
             (chadd b2 #f))
    changes)

  (define (restart-scenario! ownspace)
    ; remove everything from space
    (define changes
      (for/list ((o (space-objects ownspace)))
        (chrm (ob-id o))))

    ; add standard stuff
    (append! changes (chadd (standard-quit-scenario-button) #f))
    (append! changes (redo-team-buttons))

    ; add team1 base
    (define b1 (make-base team1 base1x (random-between -500 500)))
    (set! base1id (ob-id b1))
    (append! changes (chadd b1 #f))

    ; add team2 base
    (define b2 (make-base team2 base2x (random-between -500 500)))
    (set! base2id (ob-id b2))
    (append! changes (chadd b2 #f))

    ; add players
    (for ((p (in-list (space-players ownspace)))
          #:when (member (player-faction p) teams))
      (define-values (x y) (start-xy ownspace (player-faction p)))
      (append! changes (place-player p x y)))
    
    changes)

  ; prime the initial space
  (apply-all-changes! ownspace (restart-scenario! ownspace) "scenario")
  

  (define (on-player-restart space pid)
    (define changes '())
    ; this happens after the player's spacesuit has been removed
    (define p (findfid pid (space-players space)))
    (define-values (x y) (start-xy ownspace (player-faction p)))
    (append! changes (place-player p x y))
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
         (append! changes (chfaction (ob-id p) "undecided")))
        ((equal? team1 (player-faction p))
         (set! t1num (add1 t1num)))
        ((equal? team2 (player-faction p))
         (set! t2num (add1 t2num)))))

    (when (or (not (equal? team1-num t1num))
              (not (equal? team2-num t2num)))
      (set! team1-num t1num)
      (set! team2-num t2num)
      (append! changes (redo-team-buttons)))
    
    changes)
  
  (define (on-message space cmd change-scenario!)
    (define changes '())
    (define o (find-id space space (anncmd-id cmd)))
    (when (and o (ann-button? o))
      (cond
        ((equal? "quit-scenario" (ann-button-msg o))
         (change-scenario!))
        ((member (ann-button-msg o) teams)
         (append! changes (chfaction (anncmd-pid cmd) (ann-button-msg o)))
         
         (define p (struct-copy player (findfid (anncmd-pid cmd) (space-players space))))
         (set-player-faction! p (ann-button-msg o))
         (define-values (x y) (start-xy ownspace (player-faction p)))
         (append! changes (place-player p x y)))))
    changes)
  
  (values ownspace on-tick on-message on-player-restart))


