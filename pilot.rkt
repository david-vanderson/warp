#lang racket/base

(require racket/math)

(require "defs.rkt"
         "utils.rkt"
         "draw.rkt"
         "physics.rkt")

(provide (all-defined-out))

;; utils

(define (can-launch? stack)
  (define ships (get-ships stack))
  (and (not (ship-flying? (car ships)))
       (not (null? (cdr ships)))
       (ship-flying? (cadr ships))))


; return positive number representing how good a position ownship is in
(define (pilot-fitness space ownship)
  (define f 1.0)
  
  ; reduce fitness for hitting ships
  (for ((o (space-objects space)))
    (when (and (ship? o)
               (not (= (ob-id ownship) (ob-id o))))
      (define d (distance ownship o))
      (define mind (* 2 (+ (stats-radius (ship-stats ownship))
                           (stats-radius (ship-stats o)))))
      (when (d . < . mind)
        (set! f (* f (/ d mind))))))
  
  ; increase fitness for being in a good position on the nearest enemy
  (define ne (nearest-enemy space ownship))
  (when ne
    (define ne-dist (distance ownship ne))
    (define p (ship-pilot ownship))
    (define t (theta ownship ne))
    (define ht (abs (angle-diff (posvel-r (obj-posvel ownship)) t)))
    
    (cond
      ((ne-dist . < . 200)
       ; pointing away is better
       (set! f (* f (if (ht . > . pi/2) 1.0 0.5))))
      (else
        ; pointing more at ne is better
       (set! f (* f (- 1.0 (/ ht pi))))))
    
    ; moving is better
    (when (not (pilot-fore p))
      (set! f (* f 0.5))))
  f)


;; server

(define (pilot-predict! space ship)
  (for ((i (inexact->exact (round (/ 300.0 TICK)))))
    (update-physics! space ship (/ TICK 100.0))))

; return a list of changes
; the whole world is already predicted forward
(define (pilot-ai! space stack)
  (define changes '())
  (define pod (get-pod stack))
  (define p (get-role stack))
  (define newp (copy-role p))
  (define ownship (get-ship stack))
  (define predicted-posvel (obj-posvel ownship))
  
  ;(printf "~a pilot-ai\n" (ship-name ownship))
  
  ; search space around our original inputs
  (define bestp (copy-role p))
  (define bestfit 0)
  (set-pod-role! pod newp)
  (for* ((course-change '(-5 0 5))
         (engine-on '(#t #f)))
    
    ; go back to present
    (set-obj-posvel! ownship (struct-copy posvel (posvel-t predicted-posvel)))
    ; set new inputs
    (set-pilot-course! newp (angle-add (pilot-course p) (degrees->radians course-change)))
    (set-pilot-fore! newp engine-on)
    ; predict into future
    (pilot-predict! space ownship)
    ; calculate fitness
    (define fitness (pilot-fitness space ownship))
    
    ;(printf "trying ~a ~a ~a\nold p ~v\nnewp  ~v\nbestp ~v\n" course-change engine-on fitness p newp bestp)
    
    (when (fitness . > . bestfit)
      (set! bestfit fitness)
      (set! bestp (copy-role newp))))
  
  ; reset ship to original pilot
  (set-pod-role! pod p)
  
  ; predict our ship back to the future with unchanged inputs
  (set-obj-posvel! ownship predicted-posvel)
  
  (when (not (equal? p bestp))
    (printf "~a new pilot ~a ~v\n" (ship-name ownship) bestfit bestp)
    (set! changes (list bestp)))
  
  changes)


;; client/server

(define (change-pilot p space stack)
  (cond
    ((pilot-launch p)  ; server only
     ; launch this ship off of it's parent
     (define ships (get-ships stack))
     (define ship (car ships))
     (define parent (cadr ships))
     (define hangar (get-hangar parent))
     (define r (angle-add (posvel-r (obj-posvel parent)) pi))
     (define pv (posvel 0
                        (+ (posvel-x (obj-posvel parent)) (* 20 (cos r)))
                        (+ (posvel-y (obj-posvel parent)) (* 20 (sin r)))
                        r
                        (- (posvel-dx (obj-posvel parent)))
                        (- (posvel-dy (obj-posvel parent)))
                        0))
     (define pilot (copy-role (ship-pilot ship)))
     (set-pilot-course! pilot r)
     (set-pilot-fore! pilot #t)
     (list (chmov (ob-id ship) (ob-id hangar) #f pv) pilot))
    (else
     (define role (get-role stack))
     (set-pilot-course! role (pilot-course p))
     (set-pilot-fore! role (pilot-fore p))
     '())))


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


(define (draw-pilot dc space stack)
  (define role (get-role stack))
  (define ship (get-ship stack))
  
  (draw-view dc (get-center stack) space)
  (draw-hud dc ship (get-pod stack))
  
  (define buttons (list leave-button))
  (when (can-launch? stack)
    (set! buttons (cons (button -200 -300 70 30 5 5 "launch" "Launch") buttons)))
  (when (ship-flying? ship)
    (set! buttons (cons (button 0 -300 60 30 5 5 "fore" (if (pilot-fore role) "Stop" "Go")) buttons)))
  buttons)
