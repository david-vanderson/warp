#lang racket/base

(require racket/math)

(require "defs.rkt"
         "physics.rkt"
         "tactics.rkt")

(provide (all-defined-out))

(define (run-ai! space)
  (define u #f)
  (define objects (space-objects space))
  (define ships (filter ship? objects))
  (define aiships (filter ship-npc? ships))
  (for ((s aiships))
    (set! u (or u (ai-steer! space ships s)))
    (set! u (or u (ai-tactics! space ships s))))
  u)


(define (ai-steer! space ships ownship)
  (define u #f)
  (define p (ship-pilot ownship))
  
  (define agro-dist 600)  ; ignore ships farther away than this
  
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
    (define ht (angle-diff (pilot-course p) t))
    (define r (posvel-r (obj-posvel ownship)))
    (unless (pilot-fore p)
      (set-pilot-fore! p #t)
      (printf "~a moving\n" (ship-name ownship))
      (set! u #t))
    (when (or ((abs ht) . > . (* 3/4 pi))  ; we are heading away from the enemy
              (and (ne-dist . > . 300)  ; enemy is getting away
                   ((abs ht) . > . (* 1/6 pi))))  ; we aren't pointed towards him
      ; retarget for a new attack pass
      (set-pilot-course! p (angle-add t (random-between (- (* 1/8 pi)) (* 1/8 pi))))
      (set-pilot-fore! p #t)
      (printf "~a attack ~a ~a\n" (ship-name ownship) (ship-name ne) (pilot-course p))
      (set! u #t)))
  
  (when (not ne)
    ; follow other orders?
    (unless (not (pilot-fore p))
      (set-pilot-fore! p #f)
      (printf "~a stopping\n" (ship-name ownship))
      (set! u #t)))
  u)


(define (ai-tactics! space ships ownship)
  (for ((tstack (search ownship tactics? #t)))
    (cond
      (((random) . > . 0.9)
       (define pod (get-pod tstack))
       (define ps (obj-posvel ownship))
       (define podangle (+ (posvel-r ps) (pod-angle pod)))
       (command-tactics (struct-copy tactics (car tstack) (shield podangle))
        space tstack))))
  #f)
