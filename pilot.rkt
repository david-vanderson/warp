#lang racket/base

(require racket/math)

(require "defs.rkt"
         "utils.rkt"
         "draw.rkt")

(provide (all-defined-out))

;; utils

(define (can-launch? stack)
  (define ships (get-ships stack))
  (and (not (ship-flying? (car ships)))
       (not (null? (cdr ships)))
       (ship-flying? (cadr ships))))


;; server

; return a list of changes
(define (pilot-ai! space stack)
  (define changes '())
  (define ownship (get-ship stack))
  (when (ship-flying? ownship)
    (define p (get-role stack))
    (define newp (copy-role p))
    (define ne (nearest-enemy space ownship))
  
    (when ne
      (define ne-dist (distance ownship ne))
      
      (define t (theta ownship ne))
      (define ht (angle-diff (pilot-course p) t))
      (define r (posvel-r (obj-posvel ownship)))
      (unless (pilot-fore p)
        (set-pilot-fore! newp #t)
        ;(printf "~a moving\n" (ship-name ownship))
        (set! changes (list newp)))
      (when (or ((abs ht) . > . (* 3/4 pi))  ; we are heading away from the enemy
                (and (ne-dist . > . 300)  ; enemy is getting away
                     ((abs ht) . > . (* 1/6 pi))))  ; we aren't pointed towards him
        ; retarget for a new attack pass
        (set-pilot-course! newp (angle-add t (random-between (- (* 1/8 pi)) (* 1/8 pi))))
        (set-pilot-fore! newp #t)
        ;(printf "~a attack ~a ~a\n" (ship-name ownship) (ship-name ne) (pilot-course newp))
        (set! changes (list newp))))
  
    (when (not ne)
      ; follow other orders?
      (unless (not (pilot-fore p))
        (set-pilot-fore! newp #f)
        ;(printf "~a stopping\n" (ship-name ownship))
        (set! changes (list newp)))))
  changes)


;; client/server

(define (update-pilot p space stack)
  (cond
    ((pilot-launch p)  ; server only
     ; launch this ship off of it's parent
     (define ships (get-ships stack))
     (define ship (car ships))
     (define parent (cadr ships))
     (define hangar (get-hangar parent))
     (set-hangarpod-ships! hangar (remove ship (hangarpod-ships hangar)))
     (define r (angle-add (posvel-r (obj-posvel parent)) pi))
     (set-obj-posvel! ship (posvel 0
                                   (+ (posvel-x (obj-posvel parent)) (* 20 (cos r)))
                                   (+ (posvel-y (obj-posvel parent)) (* 20 (sin r)))
                                   r
                                   (- (posvel-dx (obj-posvel parent)))
                                   (- (posvel-dy (obj-posvel parent)))
                                   0))
     (define pilot (ship-pilot ship))
     (set-pilot-course! pilot r)
     (set-pilot-fore! pilot #t)
     (set-space-objects! space (cons ship (space-objects space)))
     (list (chmov (ob-id ship) (ob-id hangar) #f) pilot))
    (else
     ; client/server
     (define role (get-role stack))
     (set-pilot-course! role (pilot-course p))
     (set-pilot-fore! role (pilot-fore p))
     (list p))))


;; client

(define (click-pilot x y button stack)
  (define role (get-role stack))
  (cond
    (button
     ;(when button (printf "~a: pilot clicked button ~a\n" (player-name me) button))
     (case button
       (("fore")
        (struct-copy pilot role (fore (not (pilot-fore role)))))
       (("launch")
        (struct-copy pilot role (launch #t)))))
    (else
     ;(printf "~a: pilot course change\n" (player-name me))
     (define course (atan y x))
     (when (course . < . 0)
       (set! course (+ course 2pi)))
     (struct-copy pilot role (course course)))))


(define (draw-pilot dc ownspace stack)
  (define role (get-role stack))
  (define ship (get-ship stack))
  (draw-observer dc ownspace stack)
  (define buttons (list leave-button))
  (when (can-launch? stack)
    (set! buttons (cons (button -200 -300 70 30 5 5 "launch" "Launch") buttons)))
  (when (ship-flying? ship)
    (set! buttons (cons (button 0 -300 60 30 5 5 "fore" (if (pilot-fore role) "Stop" "Go")) buttons)))
  buttons)
