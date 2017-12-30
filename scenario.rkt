#lang racket/base

(require "defs.rkt"
         "utils.rkt"
         "change.rkt"
         "scenarios/testing.rkt"
         "scenarios/pilot-training.rkt"
         "scenarios/base-defense.rkt"
         "scenarios/asteroid-search.rkt"
         )

(provide (all-defined-out))


;; Scenario function
; oldspace old-on-tick old-on-message -> (values newspace on-tick on-message)
; on-tick: space change-scenario! -> list of commands/changes
; on-message: space command-message change-scenario! -> list of commands/changes


;; Scenario Picking

(define (sc-pick oldspace old-on-tick old-on-message)
  (define players (if oldspace (space-players oldspace) '()))
  (for ((p players)) (set-player-faction! p "players"))
  (define newspace
    (space 0 1000 1000
           players
           '()
           `(
             ,(ann-button (next-id) 0 (posvel 0 200 -200 0 200 100 0) #f "Testing" "testing")
             ,(ann-button (next-id) 0 (posvel 0 -200 -200 0 200 100 0) #f "Pilot Training" "pilot-training")
             ,(ann-button (next-id) 0 (posvel 0 -200 0 0 200 100 0) #f "Base Defense" "base-defense")
             ,(ann-button (next-id) 0 (posvel 0 -200 200 0 200 100 0) #f "Asteroid Search" "asteroid-search")
             )))
  (define (on-tick space change-scenario!)
    (define changes '())
    (for ((p (space-players space)))
      (when (not (player-faction p))
        ; new player
        (append! changes (chfaction (ob-id p) "players"))))
    changes)
  (define (on-message space cmd change-scenario!)
    (define o (find-id space (command-id cmd)))
    (when (and o (ann-button? o))
      (case (ann-button-msg o)
        (("testing") (change-scenario! testing-scenario))
        (("pilot-training") (change-scenario! pilot-training-scenario))
        (("base-defense") (change-scenario! base-defense-scenario))
        (("asteroid-search") (change-scenario! asteroid-search-scenario))
        ))
    '())
  (values newspace on-tick on-message))

  