#lang racket/base

(require "defs.rkt"
         "utils.rkt"
         "change.rkt"
         "scenarios/testing.rkt"
         "scenarios/all-ships.rkt"
         "scenarios/racketcon2018.rkt"
         "scenarios/pilot-training.rkt"
         "scenarios/base-defense.rkt"
         "scenarios/asteroid-search.rkt"
         )

(provide (all-defined-out))


;; Scenario function
; oldspace old-on-tick old-on-message -> (values newspace on-tick on-message)
; on-tick: space quadtree change-scenario! -> list of commands/changes
; on-message: space command-message change-scenario! -> list of commands/changes


;; Scenario Picking

(define (sc-pick oldspace old-on-tick old-on-message old-on-player-restart)
  (define players (if oldspace (space-players oldspace) '()))
  (for ((p players)) (set-player-faction! p "players"))
  (define newspace
    (space (next-id) 0 500 500
           players
           '()
           `(
             ,(make-ann-button 150 -75 200 50 "Testing" "testing")
             ,(make-ann-button 150 0 200 50 "All Ships" "all-ships")
             ,(make-ann-button -150 -75 200 50 "Pilot Training" "pilot-training")
             ,(make-ann-button -150 0 200 50 "Base Defense" "base-defense")
             ,(make-ann-button -150 75 200 50 "Asteroid Search" "asteroid-search")
             ,(make-ann-button -150 150 200 50 "RacketCon 2018" "racketcon2018")
             )))
  (define (on-tick space qt change-scenario!)
    (define changes '())
    (for ((p (space-players space)))
      (when (not (player-faction p))
        ; new player
        (append! changes (chfaction (ob-id p) "players"))))
    changes)
  (define (on-message space cmd change-scenario!)
    (define o (find-id space space (anncmd-id cmd)))
    (when (and o (ann-button? o))
      (case (ann-button-msg o)
        (("testing") (change-scenario! testing-scenario))
        (("all-ships") (change-scenario! all-ships-scenario))
        (("racketcon2018") (change-scenario! racketcon2018-scenario))
        (("pilot-training") (change-scenario! pilot-training-scenario))
        (("base-defense") (change-scenario! base-defense-scenario))
        (("asteroid-search") (change-scenario! asteroid-search-scenario))
        ))
    '())
  (values newspace on-tick on-message #f))

  