#lang racket/base

(require racket/math
         racket/class)

(require "defs.rkt"
         "utils.rkt"
         "draw-utils.rkt"
         "draw.rkt"
         "physics.rkt")

(provide (all-defined-out))

;; utils

(define (can-launch? stack)
  (define ships (get-ships stack))
  (and (not (ship-flying? (car ships)))
       (not (null? (cdr ships)))
       (ship-flying? (cadr ships))))


; return [0,1] representing how good ownship's position is
; goal is where we want to go
(define (pilot-fitness space ownship)
  (define f 1.0)
  (define strat (ship-ai-strategy ownship))
  
  ; reduce fitness for hitting ships
  (for ((o (space-objects space)))
    (when (and (ship? o)
               (not (= (ob-id ownship) (ob-id o))))
      (define d (distance ownship o))
      (define mind (+ (stats-radius (ship-stats ownship))
                      (stats-radius (ship-stats o))))
      (cond ((d . < . mind)
             (set! f 0.0))  ; don't hit
            ((d . < . (* 2 mind))
             (set! f (* f (/ d (* 2 mind))))))))  ; try not to get close
  
  (when strat
    ; assuming "goto" strategy
    (define d (distance ownship (strategy-args strat)))
    (when (d . > . AI_GOTO_DIST)
      (define dd (- d AI_GOTO_DIST))
      (set! f (* f (if (dd . < . 500.0)
                       (- 1.0 (/ dd 500.0))
                       (/ 1.0 dd))))))
  f)


;; server

; update strategy, return strategy if updated
; pilot-ai! plans the route
(define (pilot-ai-strategy! space stack)
  (define new-strat '())
;  (define ship (get-ship stack))
;  (define strat (ship-ai-strategy ship))
;  (strategy-args strat)
  new-strat)


(define (pilot-predict! space ship dt)
  (for ((i (inexact->exact (round (/ (* 100.0 dt) TICK)))))
    (update-physics! space ship (/ TICK 100.0))))


(define (best-fitness space ships ownship dt)
  (define p (ship-pilot ownship))
  (define courses (for/list ((cx '(0 -5 -1 1 5)))
                    (angle-add (pilot-course p) (degrees->radians cx))))
  (define engines (list (pilot-fore p) (not (pilot-fore p))))
  
  (define bestfit #f)
  (define bestc #f)
  (define beste #f)
  (for* ((c courses) (e engines))
    (set-pilot-course! p c)
    (set-pilot-fore! p e)
    
    ; run everything forward a second
    (for ((s ships))  ; includes ownship
      ; using posvel-t for fun
      (set-posvel-t! (obj-posvel s) (struct-copy posvel (obj-posvel s)))
      (pilot-predict! space s 1.0))
    
    (define newf (pilot-fitness space ownship))
    (when (dt . > . 0)
      (define-values (ff fc fe) (best-fitness space ships ownship (- dt 1.0)))
      (set! newf (* newf ff)))
       
    ; run everything back
    (for ((s ships))
      (set-obj-posvel! s (posvel-t (obj-posvel s))))
    
    (when (or (not bestfit)  ; first pass
              (newf . > . bestfit))
      (set! bestfit newf)
      (set! bestc c)
      (set! beste e)))
  
  (values bestfit bestc beste))


; return a list of changes
; the whole world is already predicted forward
(define (pilot-ai! space stack)
  (define changes '())
  (define p (get-role stack))
  (define origp (copy-role p))
  (define ownship (get-ship stack))
  
  ; only consider ships
  (define ships (filter ship? (space-objects space)))
  
  (define-values (ff fc fe) (best-fitness space ships ownship 3.0))
  (define bestp (copy-role origp))
  (set-pilot-course! bestp fc)
  (set-pilot-fore! bestp fe)
  
  ; reset ship to original pilot
  (set-pod-role! (get-pod stack) origp)
  
  (when (not (equal? p bestp))
    (printf "~a new pilot ~a ~v\n" (ship-name ownship) ff bestp)
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


(define (draw-pilot-fitness dc space ship)
  (define pv (struct-copy posvel (obj-posvel ship)))
  (for* ((cx (in-range -300 350 25))
         (cy (in-range -300 350 25)))
    (define testpv (struct-copy posvel pv))
    (set-posvel-x! testpv (+ cx (posvel-x testpv)))
    (set-posvel-y! testpv (+ cy (posvel-y testpv)))
    (set-obj-posvel! ship testpv)
    (define f (pilot-fitness space ship))
    (set-obj-posvel! ship pv)
    (define cc (linear-color "blue" "red" f 1.0))
    ;(printf "f ~a cc ~a ~a ~a : ~a ~a ~a\n" f (send cc red) (send cc green) (send cc blue) cx cy cr)
    (send dc set-pen cc 2.0 'solid)
    (send dc set-brush cc 'solid)
    (send dc draw-ellipse (- cx 1.5) (- cy 1.5) 3 3))
  
  (define pvp (struct-copy posvel (obj-posvel ship)))
  (pilot-predict! space ship 3.0)
  (define futurepv (obj-posvel ship))
  (set-obj-posvel! ship pvp)
  (send dc set-pen "green" 2.0 'solid)
  (send dc set-brush nocolor 'transparent)
  (define-values (fx fy) (recenter ship (obj #f #f futurepv)))
  (send dc draw-ellipse (- fx 3) (- fy 3) 6 6)
  
  (send dc set-pen "green" 2.0 'solid)
  (send dc set-brush nocolor 'transparent)
  (define-values (x y) (recenter ship (strategy-args (ship-ai-strategy ship))))
  (send dc draw-ellipse (- x AI_GOTO_DIST) (- y AI_GOTO_DIST)
        (* AI_GOTO_DIST 2) (* AI_GOTO_DIST 2)))
