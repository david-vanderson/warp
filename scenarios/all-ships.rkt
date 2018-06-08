#lang racket/base

(require racket/math)

(require "../defs.rkt"
         "../utils.rkt"
         "../ships.rkt"
         "../order.rkt"
         "../physics.rkt"
         "../effect.rkt"
         "../upgrade.rkt")

(provide (all-defined-out))

(define (all-ships-scenario oldspace oldtick oldmessage)
  (define players (if oldspace (space-players oldspace) '()))
  (for ((p players)) (set-player-faction! p "Test Faction"))

  (define ownspace (space (next-id) 0 10000 10000 players '()
                          `(,(standard-quit-scenario-button #t))))
  
  (set-space-objects! ownspace
                      (append (space-objects ownspace)
                              (for/list (((name si) (in-hash ship-list))
                                         (x (in-range -1000 1000 100)))
                                (make-ship name name "Test Faction" #:x x #:start-ship? #t))))
  
  ; return a list of changes
  (define (on-tick ownspace qt change-scenario!)
    (define changes '())

    (for ((p (space-players ownspace)))
      (when (not (player-faction p))
        ; new player
        (append! changes (chfaction (ob-id p) "Test Faction"))))
    
    changes)
  
  (define (on-message space cmd change-scenario!)
    (define o (find-id space space (anncmd-id cmd)))
    (when (and o (ann-button? o))
      (case (ann-button-msg o)
        (("quit-scenario") (change-scenario!))))
    '())
  
  (values ownspace on-tick on-message))
  
