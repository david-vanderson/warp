#lang racket/base

(require racket/math)

(require "defs.rkt"
         "physics.rkt")

(provide (all-defined-out))

(define (run-ai! space)
  (define u #f)
  (define objects (space-objects space))
  (define ships (filter ship? objects))
  (define aiships (filter ship-npc? ships))
  (for ((s aiships))
    (set! u (or u (ai-steer! space ships s))))
  u)


(define (ai-steer! space ships ownship)
  (define u #f)
  
  (define agro-dist 600)  ; ignore ships farther away than this
  (define min-dist 200)  ; try to keep at least this far from enemies
  (define max-dist 300)  ; try to keep this close to enemies
  
  (define enemies (filter (lambda (s) (not (equal? (ship-faction s)
                                                   (ship-faction ownship))))
                          ships))
  (define ne #f)
  (define ne-dist #f)
  (for ((e enemies))
    (define d (distance ownship e))
    (when (and (d . < . agro-dist)
               (or (not ne) (d . < . ne-dist)))
      (set! ne e)
      (set! ne-dist d)))
  
  (when ne
    (define t (theta ownship ne))
    (define h (ship-helm ownship))
    (define ht (angle-diff (helm-course h) t))
    (define r (posvel-r (obj-posvel ownship)))
    (cond
      ((< ne-dist min-dist)
       ; too close
       ;(printf "~a too close to ~a\n" (ship-name ownship) (ship-name ne))
       (unless (helm-fore h)
         (set-helm-fore! h #t)
         (printf "~a moving\n" (ship-name ownship))
         (set! u #t))
       (when (and ((abs (angle-diff r t)) . < . (/ pi 2))  ; pointed at enemy
                  ((abs ht) . < . (/ pi 2)))  ; course towards enemy
         ;(printf "~a h ~a t ~a ht ~a\n" (ship-name ownship) (helm-course h) t ht)
         (set-helm-course! h ((if (ht . >= . 0) angle-sub angle-add)
                              (helm-course h) (/ pi 2)))
         (set-helm-fore! h #t)
         (printf "~a turning away ~a ~a\n" (ship-name ownship) (ship-name ne) (helm-course h))
         (set! u #t)))
      ((< min-dist ne-dist max-dist)
       ; good distance, keep it
       ;(printf "~a good distance from ~a\n" (ship-name ownship) (ship-name ne))
       (unless (helm-fore h)
         (set-helm-fore! h #t)
         (printf "~a moving\n" (ship-name ownship))
         (set! u #t))
       (when (not (< (/ pi 8) (abs ht) (+ (/ pi 2) (/ pi 8))))
         (set-helm-course! h ((if (ht . >= . 0) angle-sub angle-add)
                              t (random-between (/ pi 6) (/ pi 2))))
         (set-helm-fore! h #t)
         (printf "~a turning along ~a ~a\n" (ship-name ownship) (ship-name ne) (helm-course h))
         (set! u #t)))
      ((< ne-dist agro-dist)
       ; attack!
       ;(printf "~a agro ~a\n" (ship-name ownship) (ship-name ne))
       (unless (helm-fore h)
         (set-helm-fore! h #t)
         (printf "~a moving\n" (ship-name ownship))
         (set! u #t))
       (when (not ((abs ht) . < . (/ pi 8)))
         (set-helm-course! h t)
         (set-helm-fore! h #t)
         (printf "~a turning toward ~a ~a\n" (ship-name ownship) (ship-name ne) (helm-course h))
         (set! u #t)))
      (else
       ; follow other orders?
       (unless (not (helm-fore h))
         (set-helm-fore! h #f)
         (printf "~a stopping\n" (ship-name ownship))
         (set! u #t)))
      ))
  u)
