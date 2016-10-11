#lang racket/base

(require "defs.rkt"
         "utils.rkt"
         "change.rkt"
         "ships.rkt")

(provide (all-defined-out))


(define (string->scenario str)
  (case str
    (("pick") pick)))


;; Scenario function
; oldspace old-on-tick -> (values newspace on-tick)
; on-tick: space -> list of commands/changes


;; Scenario Picking

(define (pick (oldspace #f) (oldtick #f))
  (define newspace
    (space 0 1000 1000 '()))
  (define (on-tick space)
    '())
  (values newspace on-tick))