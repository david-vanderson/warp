#lang racket/base

(require "defs.rkt"
         "utils.rkt"
         "draw.rkt")

(provide (all-defined-out))

;; client/server

(define (update-pilot p ownspace stack)
  (define role (get-role stack))
  (set-pilot-course! role (pilot-course p))
  (set-pilot-fore! role (pilot-fore p))
  (list p))


;; client

(define (click-pilot x y button stack)
  (define role (get-role stack))
  (cond
    (button
     ;(when button (printf "~a: pilot clicked button ~a\n" (player-name me) button))
     (case button
       (("fore")
        (struct-copy pilot role (fore (not (pilot-fore role)))))))
    (else
     ;(printf "~a: pilot course change\n" (player-name me))
     (define course (atan y x))
     (when (course . < . 0)
       (set! course (+ course 2pi)))
     (struct-copy pilot role (course course)))))


(define (draw-pilot dc ownspace stack)
  (define role (get-role stack))
  (draw-observer dc ownspace stack)
  (list leave-button
        (button -100 -100 60 30 5 5 "fore" (if (pilot-fore role) "Stop" "Go"))))
