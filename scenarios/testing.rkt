#lang racket/base

(require racket/math)

(require "../defs.rkt"
         "../utils.rkt"
         "../ships.rkt"
         "../order.rkt"
         "../upgrade.rkt")

(provide (all-defined-out))

(define (testing-scenario oldspace oldtick oldmessage)
  (define ai? #t)
  (define players (if oldspace (space-players oldspace) '()))
  (for ((p players)) (set-player-faction! p "Rebel"))

  (define ownspace (space 0 20000 2000 players '()
                          `(,(standard-quit-scenario-button #t))))
  
  (set-space-objects! ownspace
                      (append (space-objects ownspace)
                              (for/list (((name si) (in-hash ship-list))
                                         (x (in-range -1000 1000 100)))
                                (make-ship name name "Rebel" #:x x #:start-ship? #t))))
  
  (define real-orders (space 0 0 0 '() '() '()))  ; only care about orders
  
  ; return a list of changes
  (define (on-tick ownspace change-scenario!)
    (define changes '())

    (for ((p (space-players ownspace)))
      (when (not (player-faction p))
        ; new player
        (append! changes (chfaction (ob-id p) "Rebel"))))

    (for ((fo (space-orders real-orders)))
      (check ownspace (car fo) (cadr fo)))
    
    (append! changes (order-changes ownspace real-orders))
    
    changes)
  
  (define (on-message space cmd change-scenario!)
    (define o (find-id space (anncmd-id cmd)))
    (when (and o (ann-button? o))
      (case (ann-button-msg o)
        (("quit-scenario") (change-scenario!))))
    '())
  
  (values ownspace on-tick on-message))


