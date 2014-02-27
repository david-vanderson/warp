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
             (set! f (* f 0.0)))
            ((d . < . (* 5 mind))
             (set! f (* f (expt (/ (- d mind) (* 5 mind)) 2)))))))
  
  ; assuming "goto" strategy
  (define d (distance ownship (strategy-args strat)))
  (when (d . > . AI_GOTO_DIST)
    ;(define dd (- d AI_GOTO_DIST))
    (set! f (* f (if (d . < . 500.0)
                     (- 1.0 (/ d 500.0))
                     (/ 1.0 d)))))
  
  (define ad (abs (angle-diff (posvel-r (obj-posvel ownship))
                              (theta ownship (strategy-args strat)))))
  (set! f (* f (- 1.0 (* 0.01 (/ ad pi)))))
  
  f)


;; server


; update strategy, return strategy if updated
; pilot-ai! plans the route
(define (pilot-ai-strategy! space stack)
  (define changes '())
  (define ship (get-ship stack))
  (define strat (ship-ai-strategy ship))
  (define ne (nearest-enemy space ship))
  (when (and ne
             (or ((distance ship (strategy-args strat)) . < . AI_GOTO_DIST)
                 ((abs (angle-diff (posvel-r (obj-posvel ship)) (theta ship ne))) . > . (* 4/5 pi))))
    ; pick a new destination on the far side of ne
    (define t (angle-add (theta ship ne) (random-between (- (* 1/6 pi)) (* 1/6 pi))))
    (define r (+ (distance ship ne) (random-between 150 200)))
    (define s (strategy "goto" (obj #f #f (posvel #f
                                                  (+ (posvel-x (obj-posvel ship)) (* r (cos t)))
                                                  (+ (posvel-y (obj-posvel ship)) (* r (sin t)))
                                                  0 #f #f #f))))
    (set! changes (list (new-strat (ob-id ship) s))))
  changes)

(define predict-secs 10)

; return a list of changes
(define (pilot-ai! space stack)
  (define changes '())
  
  (define ownship (get-ship stack))
  (define p (get-role stack))
  (define origp (copy-role p))
  
  ; only worry about ships
  (define ships (filter (lambda (o)
                          (and (ship? o)
                               (not (= (ob-id ownship) (ob-id o)))))
                        (space-objects space)))
  
  ; search space around our original inputs
  (define bestp (copy-role p))
  (define bestfit #f)
  (for ((newce '((#f 0) (#f -90) (#f 90) (#t 0)
                        (#t -170) (#t -83) (#t -44) (#t -5)
                        (#t 3) (#t 42) (#t 81) (#t 120))))
    (define origpv (struct-copy posvel (obj-posvel ownship)))
    (set-pilot-fore! p (car newce))
    (set-pilot-course! p (angle-add (pilot-course origp) (degrees->radians (cadr newce))))
    (define maxfit 0.0)
    (define curfit 1.0)
    
    (for ((i predict-secs))
      (for ((s ships)) (physics! (obj-posvel s) 1.0))
      (update-physics! space ownship 0.5)
      (update-physics! space ownship 0.5)
      (define f (pilot-fitness space ownship))
      (set! maxfit (max maxfit (* curfit (expt f (- predict-secs i)) (expt 0.9 (- predict-secs i)))))
      (set! curfit (* curfit f)))
    
    (for ((s ships)) (physics! (obj-posvel s) (- predict-secs)))
    (set-obj-posvel! ownship origpv)
    
    (when (or (not bestfit)  ; first pass
              (maxfit . > . bestfit))
      ;(printf "better fit ~a ~v\n" maxfit newce)
      (set! bestfit maxfit)
      (set! bestp (copy-role p))))
  
  (set! p (copy-role bestp))
  (set-pod-role! (ship-helm ownship) origp)
  
  (when (not (equal? origp p))
    (printf "~a new pilot ~v\n" (ship-name ownship) p)
    (set! changes (list p)))
  
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
  
  (define origpv (struct-copy posvel (obj-posvel ship)))
  (define center (obj #f #f origpv))
  
;  (for* ((cx (in-range -300 350 25))
;         (cy (in-range -300 350 25)))
;    (define testpv (struct-copy posvel origpv))
;    (set-posvel-x! testpv (+ cx (posvel-x testpv)))
;    (set-posvel-y! testpv (+ cy (posvel-y testpv)))
;    (set-obj-posvel! ship testpv)
;    (define f (pilot-fitness space ship))
;    (define cc (linear-color "blue" "red" f 1.0))
;    ;(printf "f ~a cc ~a ~a ~a : ~a ~a ~a\n" f (send cc red) (send cc green) (send cc blue) cx cy cr)
;    (send dc set-pen cc 2.0 'solid)
;    (send dc set-brush cc 'solid)
;    (send dc draw-ellipse (- cx 1.5) (- cy 1.5) 3 3))
;  (set-obj-posvel! ship (struct-copy posvel origpv))
  
  
  
  (define ships (filter (lambda (o)
                             (and (ship? o)
                                  (not (= (ob-id ship) (ob-id o)))))
                           (space-objects space)))
  
  (send dc set-brush nocolor 'transparent)
  (define p (ship-pilot ship))
  (define origp (struct-copy pilot p))
  (for ((newce '((#f 0) (#f -90) (#f 90) (#t 0)
                        (#t -170) (#t -83) (#t -44) (#t -5)
                        (#t 3) (#t 42) (#t 81) (#t 120))))
    (define origpv (struct-copy posvel (obj-posvel ship)))
    (set-pilot-fore! p (car newce))
    (set-pilot-course! p (angle-add (pilot-course origp) (degrees->radians (cadr newce))))
    (define maxfit 0.0)
    (define curfit 1.0)
    
    (for ((i predict-secs))
      (define-values (oldx oldy) (recenter center ship))
      (update-physics! space ship 0.5)
      (update-physics! space ship 0.5)
      (define f (pilot-fitness space ship))
      (set! maxfit (max maxfit (* curfit (expt f (- predict-secs i)) (expt 0.9 (- predict-secs i)))))
      (set! curfit (* curfit f))
      (define-values (newx newy) (recenter center ship))
      (define cc (linear-color "blue" "red" maxfit 1.0))
      (send dc set-pen cc 2.0 'solid)
      (send dc draw-line oldx oldy newx newy))
    
    (set-obj-posvel! ship (struct-copy posvel origpv)))
  
  (set-pod-role! (ship-helm ship) origp)
    
;    (for ((s ships)) (physics! (obj-posvel s) -1.0))
;    (set-obj-posvel! ownship origpv)
;    
;    
;    (define p (ship-pilot ship))
;    (define origp (struct-copy pilot p))
;    (set-pilot-course! p (+ (pilot-course p) (degrees->radians course-change)))
;    (set-pilot-fore! p engine-on)
;    (define pvp (struct-copy posvel (obj-posvel ship)))
;    (pilot-predict! space ship)
;    (define futurepv (obj-posvel ship))
;    (set-obj-posvel! ship pvp)
;    (set-pod-role! (ship-helm ship) origp)
;    (send dc set-pen "greenyellow" 2.0 'solid)
;    (send dc set-brush nocolor 'transparent)
;    (define-values (fx fy) (recenter ship (obj #f #f futurepv)))
;    (send dc draw-ellipse (- fx 3) (- fy 3) 6 6))
  
;  (define pvp (struct-copy posvel (obj-posvel ship)))
;  (pilot-predict! space ship predict-secs)
;  (define futurepv (obj-posvel ship))
;  (set-obj-posvel! ship pvp)
;  (send dc set-pen "green" 2.0 'solid)
;  (send dc set-brush nocolor 'transparent)
;  (define-values (fx fy) (recenter ship (obj #f #f futurepv)))
;  (send dc draw-ellipse (- fx 3) (- fy 3) 6 6)
  
  (send dc set-pen "green" 2.0 'solid)
  (send dc set-brush nocolor 'transparent)
  (define-values (x y) (recenter ship (strategy-args (ship-ai-strategy ship))))
  (send dc draw-ellipse (- x AI_GOTO_DIST) (- y AI_GOTO_DIST)
        (* AI_GOTO_DIST 2) (* AI_GOTO_DIST 2)))
