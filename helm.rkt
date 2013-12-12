#lang racket/base

(require "defs.rkt"
         "draw.rkt")

(provide (all-defined-out))

(define (click-helm x y button role)
  (cond
    (button
     ;(when button (printf "~a: helm clicked button ~a\n" (player-name me) button))
     (case button
       (("fore")
        (struct-copy helm role (fore (not (helm-fore role)))))
       (("fire")
        (struct-copy helm role (aft #t)))))
    (else
     ;(printf "~a: helm course change\n" (player-name me))
     (define course (atan y x))
     (when (course . < . 0)
       (set! course (+ course 2pi)))
     (struct-copy helm role (course course)))))


(define (draw-helm dc role ownspace stack)
  (draw-observer dc ownspace stack)
  (list leave-button
        (button -100 -100 60 30 "fore" (if (helm-fore role) "Stop" "Go"))
        (button  100 -100 60 30 "fire" "Fire")))