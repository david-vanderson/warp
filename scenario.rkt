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
           `(
             ,(ann-button (next-id) 0 (posvel 0 -100 100 0 0 0 0) 200 100 "Base Defense" "base-defense")
             )))
  (define (on-tick space change-scenario!)
    '())
  (define (on-message space cmd change-scenario!)
    (define o (find-id space (command-id cmd)))
    (when (and o (ann-button? o))
      (case (ann-button-msg o)
        (("base-defense") (change-scenario! base-defense-scenario))))
    '())
  (values newspace on-tick on-message))