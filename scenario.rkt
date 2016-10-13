#lang racket/base

(require "defs.rkt"
         "utils.rkt"
         "change.rkt"
         "base-defense.rkt"
         "ships.rkt")

(provide (all-defined-out))


;; Scenario function
; oldspace old-on-tick old-on-message -> (values newspace on-tick on-message)
; on-tick: space change-scenario! -> list of commands/changes
; on-message: space command-message change-scenario! -> list of commands/changes


;; Scenario Picking

(define (sc-pick oldspace old-on-tick old-on-message)
  (define newspace
    (space 0 1000 1000
           (if oldspace (space-players oldspace) '())
           `(
             ,(ann-button (next-id) 0 (posvel 0 -200 200 0 0 0 0) 200 100 "Pilot Training" "pilot-training")
             ,(ann-button (next-id) 0 (posvel 0 -200 0 0 0 0 0) 200 100 "Base Defense" "base-defense")
             )))
  (define (on-tick space change-scenario!)
    '())
  (define (on-message space cmd change-scenario!)
    (define o (find-id space (command-id cmd)))
    (when (and o (ann-button? o))
      (case (ann-button-msg o)
        (("base-defense") (change-scenario! base-defense-scenario))
        (("pilot-training") (change-scenario! pilot-training-scenario))))
    '())
  (values newspace on-tick on-message))


(define (pilot-training-scenario oldspace old-on-tick old-on-message)
  (define s (make-ship "red-fighter" "Pilot Trainer" "Empire"))
  (set-ship-stats! s (stats (next-id) (ship-type s) (ship-name s) (ship-faction s)
                            ;power bat maxbat con maxcon radius mass thrust rthrust radar start?
                            1.0 100.0 100.0 20.0 20.0 6.0 20.0 50.0 1.5 300.0 #f))
  (define newspace
    (space 0 10000 10000
           (if oldspace (space-players oldspace) '())
           `(
             ,s
             )))
  (define (on-tick space change-scenario!)
    '())
  (define (on-message space cmd change-scenario!)
    '())
  (values newspace on-tick on-message))
  