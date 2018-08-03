#lang racket/base

(require racket/math)

(require "../defs.rkt"
         "../utils.rkt"
         "../ships.rkt"
         "../order.rkt"
         "../upgrade.rkt")

(provide (all-defined-out))

(define (racketcon-scenario oldspace oldtick oldmessage old-on-player-restart)  
  (define players (if oldspace (space-players oldspace) '()))
  (for ((p players)) (set-player-faction! p "undecided"))

  (define ownspace
    (space (next-id) 0 5000 5000 players '()
           `(
             ,(standard-quit-scenario-button)
             ,(ann-button (next-id) 0 #t (posvel 0 500 100 0 200 50 0)
                          #f "undecided" "Team Parens" "parens")
             ,(ann-button (next-id) 0 #t (posvel 0 500 200 0 200 50 0)
                          #f "undecided" "Team Brackets" "brackets")
             )))
  
  (define base-brackets (make-ship "blue-station" "a" "a" #:x -1000 #:y -1000
                                   #:ai 'always #:hangar '() #:dr 0.1))
  (set-ship-stats! base-brackets (stats (next-id) "blue-station" "Base Brackets" "brackets"
                                        ;con maxcon mass radar drag start-ship?
                                        1000.0 1000.0 1000.0 1000.0 0.4 #t))
  (set-ship-tools!
   base-brackets (list (tool-pbolt 10.0)
                       (tool-probe 30.0)
                       (tool-missile 5.0 10.0)))
  
  
  (define base-parens (make-ship "red-station" "b" "b" #:x 1000 #:y 1000 #:r pi #:ai 'always
                                 #:hangar '()))
  (set-ship-stats! base-parens (stats (next-id)
                                      ;type name faction
                                      "red-station" "Base Parens" "parens"
                                      ;con maxcon mass radar drag start?
                                      1000.0 1000.0 500.0 1000.0 0.4 #t))
  (set-ship-tools!
   base-parens (append (tools-pilot 6.0 #f 0.1 #:dock? #f)
                       (list (tool-pbolt 10.0)
                             (tool-missile 5.0 10.0)
                             (tool-cannon 21.0))))
  
  (set-ship-ai-strategy! base-parens
                         (list (strategy (space-time ownspace)
                                         "attack-only" (ob-id base-brackets))))
  
  (set-space-objects! ownspace
                      (append (space-objects ownspace)
                              (list base-brackets base-parens)))

  (define (get-base space faction)
    (find-top-id space (case faction
                         (("parens") (ob-id base-parens))
                         (("brackets") (ob-id base-brackets))
                         (else #f))))
  
  (define playing? #t)

  (define (on-player-restart space pid)
    (define changes '())
    ; this happens after the player's spacesuit has been removed
    (define c (get-base space (player-faction (findfid pid (space-players space)))))
    (when c
      (append! changes (chmov pid (ob-id c) #f)))
    changes)
  
  ; return a list of changes
  (define (on-tick ownspace qt change-scenario!)
    (define changes '())

    (for ((p (space-players ownspace)))
      (when (not (player-faction p))
        ; new player
        (append! changes (chfaction (ob-id p) "undecided"))))
    
    changes)
  
  (define (on-message space cmd change-scenario!)
    (define changes '())
    (define o (find-id space space (anncmd-id cmd)))
    (when (and o (ann-button? o))
      (case (ann-button-msg o)
        (("quit-scenario") (change-scenario!))
        (("parens" "brackets")
         (append! changes (chfaction (anncmd-pid cmd) (ann-button-msg o)))
         (define c (get-base space (ann-button-msg o)))
         (when c
           (append! changes (chmov (anncmd-pid cmd) (ob-id c) #f)))
         )))
    changes)
  
  (values ownspace on-tick on-message on-player-restart))


