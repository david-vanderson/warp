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

(define (all-ships-scenario oldspace oldtick oldmessage old-on-player-restart)
  (define players (if oldspace (space-players oldspace) '()))
  (for ((p players)) (set-player-faction! p "Test Faction"))

  (define ownspace (space (next-id) 0 10000 10000 players '()
                          `(,(standard-quit-scenario-button))))

  (define ships
    (for/list (((name si) (in-hash ship-list))
               (x (in-naturals)))
      (define s (make-ship name name "Test Faction" #:x (+ -2000 (* 200 x)) #:start-ship? #t
                           #:hull 100 #:mass 100 #:drag 0.4
                           #:tools (append (tools-pilot 40.0 #f 1.0)
                                           (list (tool-pbolt 8.0)))))
      (when (or (missile? s) (probe? s))
        (set-ship-tools! s (cons (tool-endrc 10.0) (ship-tools s))))
      s))
  
  (set-space-objects! ownspace
                      (append (space-objects ownspace)
                              ships))
  
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
  
  (values ownspace on-tick on-message #f))
  
