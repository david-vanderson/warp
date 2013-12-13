#lang racket/base

(require "defs.rkt"
         "draw.rkt")

(provide (all-defined-out))


; client
(define (click-helm x y button stack)
  (define role (get-role stack))
  (cond
    (button
     ;(when button (printf "~a: helm clicked button ~a\n" (player-name me) button))
     (case button
       (("fore")
        (struct-copy helm role (fore (not (helm-fore role)))))))
    (else
     ;(printf "~a: helm course change\n" (player-name me))
     (define course (atan y x))
     (when (course . < . 0)
       (set! course (+ course 2pi)))
     (struct-copy helm role (course course)))))


; server
(define (command-helm cmd ownspace stack)
  (define role (get-role stack))
  (set-helm-course! role (helm-course cmd))
  (set-helm-fore! role (helm-fore cmd)))


; client
(define (draw-helm dc ownspace stack)
  (define role (get-role stack))
  (draw-observer dc ownspace stack)
  (list leave-button
        (button -100 -100 60 30 5 5 "fore" (if (helm-fore role) "Stop" "Go"))))
