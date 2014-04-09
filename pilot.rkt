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


(define (will-dock? s1 s2)
  (and (pilot-dock (ship-pilot s1))
       (equal? (ship-faction s1) (ship-faction s2))
       (findf hangarpod? (ship-pods s2))))


; return a number representing how good ship's position is
(define (pilot-fitness space ship)
  (define f 0.0)
  (define strat (ship-strategy ship))
  
  ; reduce fitness for hitting ships
  (for ((o (space-objects space)))
    (when (and (ship? o)
               (not (= (ob-id ship) (ob-id o)))
               (not (will-dock? o ship)))
      (define d (distance ship o))
      (define mind (* 1.1 (hit-distance ship o)))
      (define ad (abs (angle-diff (posvel-r (obj-posvel ship)) (theta ship o))))
      (cond ((d . < . mind)
             (set! f (+ f -100.0))
             (set! f (+ f (* 5.0 (min 0.8 (/ ad pi))))))
            ((d . < . (* 2 mind))
             (set! f (+ f (* -100.0 (- 1.0 (/ (- d mind) mind)))))
             (set! f (+ f (* 5.0 (min 0.8 (/ ad pi))))))
            )))
  
  
  (case (and strat (strategy-name strat))
    (("retreat")
     (define ne (findf (lambda (o) (= (ob-id o) (strategy-arg strat))) (space-objects space)))
     (when ne
       (define d (distance ship ne))
       (set! f (+ f (* 25.0 (min 1.0 (/ d 1000.0)))))
       (define ad (abs (angle-diff (posvel-r (obj-posvel ship))
                                   (theta ship ne))))
       (set! f (+ f (* 5.0 (min 0.8 (/ ad pi)))))
       ))
    (("attack")
     ; trying to go to the far side of ne
     (define ne (findf (lambda (o) (= (ob-id o) (strategy-arg strat))) (space-objects space)))
     (when ne
       (define d (distance ship ne))
       (set! f (+ f (* 25.0 (- 1.0 (min 1.0 (/ d 1000.0))))))
       
       (define ad (abs (angle-diff (posvel-r (obj-posvel ship))
                                   (theta ship ne))))
       (set! f (+ f (* 5.0 (- 1.0 (max 0.2 (/ ad pi))))))
       )))
     
  f)


;; server


; update strategy, return strategy if updated
; pilot-ai! plans the route
(define (pilot-ai-strategy! space stack)
  (define changes '())
  (define ship (get-ship stack))
  (define ne (nearest-enemy space ship))
  (define strats (ship-ai-strategy ship))
  (define strat (ship-strategy ship))
  (cond
    ((not strat)
     (when ne
       (define ns (strategy (space-time space) "attack" (ob-id ne)))
       (set! changes (list (new-strat (ob-id ship) (list ns))))))
    ((equal? "retreat" (strategy-name strat))
     (cond
       ((not ne)
        (set! changes (list (new-strat (ob-id ship) (cdr strats)))))
       ((and ne (or (not (equal? (ob-id ne) (strategy-arg strat)))
                    ((distance ship ne) . > . (* 10 (hit-distance ship ne)))
                    (and ((abs (angle-diff (posvel-r (obj-posvel ship)) (theta ship ne))) . > . (* 5/6 pi))
                         ((strategy-age space strat) . > . 10000))))
        (define ns (strategy (space-time space) "attack" (ob-id ne)))
        (set! changes (list (new-strat (ob-id ship) (cons ns (cdr strats))))))))
    ((equal? "attack" (strategy-name strat))
     (cond
       ((not ne)
        (set! changes (list (new-strat (ob-id ship) (cdr strats)))))
       ((and ne (not (equal? (ob-id ne) (strategy-arg strat))))
        (define ns (strategy (space-time space) "attack" (ob-id ne)))
        (set! changes (list (new-strat (ob-id ship) (cons ns (cdr strats))))))
       ((and ne ((distance ship ne) . < . (* 5 (hit-distance ship ne))))
        (define ns (strategy (space-time space) "retreat" (ob-id ne)))
        (set! changes (list (new-strat (ob-id ship) (cons ns (cdr strats)))))))))
  (when (not (null? changes))
    (printf "new strat: ~v\n" (car changes)))
;  (when (and ne
;             (or ((distance ship (strategy-args strat)) . < . AI_GOTO_DIST)
;                 ((abs (angle-diff (posvel-r (obj-posvel ship)) (theta ship ne))) . > . (* 4/5 pi))))
;    ; pick a new destination on the far side of ne
;    (define t (angle-add (theta ship ne) (random-between (- (* 1/6 pi)) (* 1/6 pi))))
;    (define r (+ (distance ship ne) (random-between 150 200)))
;    (define s (strategy "goto" (obj #f #f (posvel #f
;                                                  (+ (posvel-x (obj-posvel ship)) (* r (cos t)))
;                                                  (+ (posvel-y (obj-posvel ship)) (* r (sin t)))
;                                                  0 #f #f #f))))
;    (set! changes (list (new-strat (ob-id ship) s))))
  changes)


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
  (for* ((f '(#f #t))
         (c '(0 -10 10 -40 40 180)))
    (define origpv (struct-copy posvel (obj-posvel ownship)))
    (set-pilot-fore! p f)
    (set-pilot-course! p (angle-add (pilot-course origp) (degrees->radians c)))
    (define maxfit -inf.0)
    (define curfit 0.0)
    
    (define predict-secs (inexact->exact (round (/ 150 (stats-thrust (ship-stats ownship))))))
    (for ((i predict-secs))
      (for ((s ships)) (physics! (obj-posvel s) 1.0))
      (update-physics! space ownship 0.5)
      (update-physics! space ownship 0.5)
      (define f (pilot-fitness space ownship))
      (set! curfit (+ curfit f))
      (set! maxfit (max maxfit (/ curfit (add1 i)))))
    
    (for ((s ships)) (physics! (obj-posvel s) (- predict-secs)))
    (set-obj-posvel! ownship origpv)
    
    ;(printf "fit ~a ~a ~a\n" maxfit f c)
    
    (when (or (not bestfit)  ; first pass
              (maxfit . > . (* 1.01 bestfit)))
      ;(printf "better fit ~a ~a ~a\n" maxfit f c)
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
     (define dist (+ (stats-radius (ship-stats ship))
                     (stats-radius (ship-stats parent))
                     10))
     (define pv (posvel 0
                        (+ (posvel-x (obj-posvel parent)) (* dist (cos r)))
                        (+ (posvel-y (obj-posvel parent)) (* dist (sin r)))
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
     (set-pilot-dock! role (pilot-dock p))
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
        (struct-copy pilot role (launch #t)))
       (("dock")
        (struct-copy pilot role (dock (not (pilot-dock role)))))))
    (else
     ;(printf "~a: pilot course change\n" (player-name me))
     (define course (atan y x))
     (when (course . < . 0)
       (set! course (+ course 2pi)))
     (struct-copy pilot role (course course)))))


(define (draw-docking dc space stack)
  (define center (get-center stack))
  (define ship (get-ship stack))
  (for ((s (space-objects space)))
    (when (and (ship? s)
               (not (= (ob-id ship) (ob-id s)))
               (will-dock? ship s))
      (define-values (x y) (recenter center s))
      (send dc set-brush nocolor 'transparent)
      (send dc set-pen "hotpink" 2.0 'solid)
      (send dc draw-ellipse (- x 10) (- y 10) 20 20))))


(define (draw-pilot dc space stack)
  (define role (get-role stack))
  (define ship (get-ship stack))
  
  (draw-view dc (get-center stack) space)
  (when (and (ship-flying? ship) (pilot-dock role))
    (draw-docking dc space stack))
  (draw-hud dc ship (get-pod stack))
  
  (define buttons (list leave-button))
  (when (can-launch? stack)
    (set! buttons (cons (button -200 -300 70 30 5 5 "launch" "Launch") buttons)))
  (when (ship-flying? ship)
    (set! buttons (cons (button -100 -300 70 30 5 5 "dock" (if (pilot-dock role) "Docking..." "Dock")) buttons))
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
  (for* ((f '(#f #t))
         (c '(0 -10 10 -40 40 180)))
    (define origpv (struct-copy posvel (obj-posvel ship)))
    (set-pilot-fore! p f)
    (set-pilot-course! p (angle-add (pilot-course origp) (degrees->radians c)))
    (define curfit 0.0)
    
    (define predict-secs (inexact->exact (round (/ 150 (stats-thrust (ship-stats ship))))))
    (for ((i predict-secs))
      (define-values (oldx oldy) (recenter center ship))
      (update-physics! space ship 0.5)
      (update-physics! space ship 0.5)
      (define f (pilot-fitness space ship))
      (set! curfit (+ curfit f))
      (define normfit (/ curfit (add1 i)))
      (define-values (newx newy) (recenter center ship))
      (define cc (if (normfit . > . 0)
                     (linear-color "blue" "green" (sigmoid normfit) 1.0)
                     (linear-color "blue" "red" (- (sigmoid normfit)) 1.0)))
      (send dc set-pen cc 2.0 'solid)
      (send dc draw-line oldx oldy newx newy))
    
    (set-obj-posvel! ship (struct-copy posvel origpv)))
  
  (set-pod-role! (ship-helm ship) origp)
  
  
  (define strats (ship-ai-strategy ship))
  (when (not (null? strats))
    (define strat (car strats))
    (send dc set-pen "green" 2.0 'solid)
    (send dc set-brush nocolor 'transparent)
    (case (strategy-name strat)
      (("retreat" "attack")
       (define ne (findf (lambda (o) (= (ob-id o) (strategy-arg strat)))
                         (space-objects space)))
       (when ne
         (define-values (x y) (recenter ship ne))
         (send dc draw-ellipse (- x AI_GOTO_DIST) (- y AI_GOTO_DIST)
               (* AI_GOTO_DIST 2) (* AI_GOTO_DIST 2)))))))
