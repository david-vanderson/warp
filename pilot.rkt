#lang racket/base

(require racket/math
         racket/class)

(require "defs.rkt"
         "utils.rkt"
         "quadtree.rkt"
         "draw-utils.rkt"
         "draw.rkt"
         "missile.rkt"
         "physics.rkt")

(provide (all-defined-out))

; return a number representing how good ship's position is
(define (pilot-fitness space qt ship)
  (define f 0.0)
  (define live? #t)
  (define strat (ship-strategy ship))
  
  ; reduce fitness for hitting ships
  (for ((o (in-list (qt-retrieve qt (obj-x ship) (obj-y ship)
                                 (+ (ship-radius ship) AI_HIT_CLOSE)))))
    (when (and (spaceship? o)
               (not (= (ob-id ship) (ob-id o)))
               (not (will-dock? o ship))
               (not (will-dock? ship o)))
      (define d (distance ship o))
      (define hd (hit-distance ship o))
      (define maxd (+ hd AI_HIT_CLOSE))
      (when (d . < . maxd)
        (define z (- maxd d))  ; meters inside maxd
        (set! f (- f (* z z))))))
  
  
  (case (and strat (strategy-name strat))
    (("scout")
     (define o (strategy-arg strat))
     (define d (distance ship o))
     (set! f (- f d)))
    (("retreat")
     (define e (find-top-id space (strategy-arg strat)))
     (when e
       (define d (distance ship e))
       (set! f (+ f d))
       (define ad (abs (angle-frto (posvel-r (obj-posvel ship))
                                   (theta ship e))))
       (set! f (+ f (* 360.0 (/ ad pi))))
       ))
    (("attack" "attack*" "attack-only")
     (define e (find-top-id space (strategy-arg strat)))
     (when e
       (define d (distance ship e))
       (set! f (- f d))
       
       (define ad (abs (angle-frto (posvel-r (obj-posvel ship))
                                   (theta ship e))))
       ; 360 for angle is fairly precise, needed for cannon
       (set! f (+ f (* 360.0 (- 1.0 (/ ad pi)))))
       ))
    (("return")
     (define mship (find-top-containing-id space (strategy-arg strat)))
     (when mship
       (define d (distance ship mship))
       (set! f (- f d))
       (when (d . < . (hit-distance ship mship))
         (set! live? #f))

       (define ad (abs (angle-frto (posvel-r (obj-posvel ship))
                                   (theta ship mship))))
       (set! f (+ f (* 360.0 (- 1.0 (/ ad pi))))))))
     
  (values f live?))


;; server

(define (return-to-base? space ship)
  #f)
;  ; we have a return strategy somewhere that is old enough
;  (for/first ((s (ship-ai-strategy ship))
;              #:when (and (equal? "return" (strategy-name s))
;                          ((strategy-age space s) . > . 30000)))
;              #t))

; return a list of changes
; update strategy, pilot-ai! plans the route
(define (pilot-ai-strategy! space qt stack)
  (define changes '())
  (define ship (get-ship stack))
  (define strats (ship-ai-strategy ship))
  ;(printf "pilot-ai-strategy! ~a ~v\n" (ship-name ship) strats)
  (define strat (ship-strategy ship))
  (define d (ship-tool ship 'dock))
  (define e (ship-tool ship 'engine))
  (cond
    ((ship-flying? ship)
     (define ne (nearest-enemy qt ship spaceship?))
     (case (and strat (strategy-name strat))
       ((#f)
        (cond
          (ne
           (define ns (strategy (space-time space) "attack" (ob-id ne)))
           (set! changes (list (new-strat (ob-id ship) (list ns)))))
          (else
           ; we are just sitting in space with no strat, at least
           ; - turn on docking so a ship can pick us up
           ; - turn off engines so we don't fly forever
           (when (and d (tool-online? d) (not (tool-rc d)))
             (append! changes (command (ob-id ship) #f 'dock #t)))
           (when (and e (tool-online? e) (tool-rc e))
             (append! changes (command (ob-id ship) #f 'engine #f)))
           )))
       (("scout")
        (define o (strategy-arg strat))
        (define d (distance ship o))
        (cond
          ((d . < . (obj-r o))
           ; close enough, remove strat
           (set! changes (list (new-strat (ob-id ship) (cdr strats)))))
          (ne
           ; attack takes precedence over scouting
           (define ns (strategy (space-time space) "attack" (ob-id ne)))
           (set! changes (list (new-strat (ob-id ship) (cons ns strats)))))))
       (("return")
        (define mothership (find-top-containing-id space (strategy-arg strat)))
        (cond
          ((not mothership)
           ; our mothership is dead
           (set! changes (list (new-strat (ob-id ship) (cdr strats)))))
          ((and ne (not (return-to-base? space ship)))
           (define ns (strategy (space-time space) "attack" (ob-id ne)))
           (set! changes (list (new-strat (ob-id ship) (cons ns strats)))))))
       (("retreat")
        (define e (find-top-id space (strategy-arg strat)))
        (cond
          ((or (not e) (return-to-base? space ship))
           ; abort
           (set! changes (list (new-strat (ob-id ship) (cdr strats)))))
          ((and ne (not (equal? (ob-id ne) (ob-id e))))
           ; new enemy, attack
           ;(printf "new enemy\n")
           (define ns (strategy (space-time space) "attack" (ob-id ne)))
           (set! changes (list (new-strat (ob-id ship) (cons ns strats)))))
          ((or ((distance ship e) . > . (+ (hit-distance ship e) AI_STRAT_TOO_FAR))
               ((current-strat-age space ship) . > . 10000))
           ; done retreating
           ;(printf "done retreating\n")
           (set! changes (list (new-strat (ob-id ship) (cdr strats)))))))
       (("attack" "attack*")
        (define e (find-top-id space (strategy-arg strat)))
        (cond
          ((or (not e) (return-to-base? space ship)
               (and (equal? "attack" (strategy-name strat))
                    ((distance ship e) . > . (* 2 (ship-radar ship)))))
           ; abort
           (set! changes (list (new-strat (ob-id ship) (cdr strats)))))
          ((and ne (not (equal? (ob-id ne) (ob-id e))))
           ; new enemy, attack
           ;(printf "new enemy\n")
           (define ns (strategy (space-time space) "attack" (ob-id ne)))
           (set! changes (list (new-strat (ob-id ship) (cons ns strats)))))
          ((and ((distance ship e) . < . (+ (hit-distance ship e) AI_STRAT_TOO_CLOSE))
                ((current-strat-age space ship) . > . 10000))
           ; too close, retreat
           ;(printf "too close\n")
           (define ns (strategy (space-time space) "retreat" (ob-id e)))
           (set! changes (list (new-strat (ob-id ship) (cons ns strats)))))))
       (("attack-only")
        (define e (find-top-id space (strategy-arg strat)))
        (cond
          ((or (not e) (return-to-base? space ship))
           ; abort
           (set! changes (list (new-strat (ob-id ship) (cdr strats)))))))
        ))
    (else
     (define mothership (cadr (get-ships stack)))
     (cond
       ((ship-flying? mothership)
        (cond
          ((and strat (equal? "return" (strategy-name strat)))
           (cond
             ((not (= (ob-id mothership) (strategy-arg strat)))
              (when (and (find-top-id space (strategy-arg strat))
                         (not (ship-behind? qt mothership))
                         (tool-online? d "nolaunch"))
                ; we accidentally docked with not our real mothership, launch again
                (set! changes (list (command (ob-id ship) #f 'dock 'launch)))))
             (else
              ; we successfully docked with our real mothership, remove the strat
              (set! changes (list (new-strat (ob-id ship) (cdr strats)))))))
          (strat
           (when (and (tool-online? d "nolaunch")
                      (= (ship-con ship) (ship-maxcon ship))
                      (not (ship-behind? qt mothership)))
             ; we have a strat to do and ready to go, launch
             (set! changes (list (command (ob-id ship) #f 'dock 'launch)))))
          (else
           (define ne (nearest-enemy qt mothership spaceship?))
           (when (and ne
                      ((distance mothership ne) . < . (* 2 (ship-radar ship)))
                      (tool-online? d "nolaunch")
                      (= (ship-con ship) (ship-maxcon ship))
                      (not (ship-behind? qt mothership)))
             ; there's an enemy and we're ready to go - launch and attack
             (define returnstrat (strategy (space-time space) "return" (ob-id mothership)))
             (define attackstrat (strategy (space-time space) "attack" (ob-id ne)))
             (set! changes (list (command (ob-id ship) #f 'dock 'launch)
                                 (new-strat (ob-id ship)
                                            (cons attackstrat
                                                  (cons returnstrat strats))))))))))))
     
  ;(when (not (null? changes))
  ;  (printf "changes ~v\n" changes))
  changes)


(struct state (origf f origc c pv fitness length live?) #:mutable #:prefab)

; return a list of changes
(define (pilot-ai-fly! space qt stack)
  (define changes '())
  
  (define ownship (get-ship stack))
  
  ; check if we need to change pilot-dock
;  (define dt (ship-tool ownship 'dock))
;  (when (and dt (tool-online? dt))
;    (define strat (ship-strategy ownship))
;    (when (and strat
;               (equal? "return" (strategy-name strat))
;               (not (tool-rc dt)))
;      (set! changes (append changes (list (command (ob-id ownship) #f 'dock #t)))))
;    (when (and strat
;               (not (equal? "return" (strategy-name strat)))
;               (tool-rc dt))
;      (set! changes (append changes (list (command (ob-id ownship) #f 'dock #f))))))


  (define st (ship-tool ownship 'steer))
  (define ft (ship-tool ownship 'engine))
  (define origf (if ft (tool-rc ft) #f))
  (define origc (if st (tool-rc st) #f))
  (define basec (obj-r ownship))
  
  ; only worry about ships that are close to us
  (define ships (filter (lambda (o)
                          (and (spaceship? o)
                               (not (= (ob-id ownship) (ob-id o)))))
                        (qt-retrieve qt (obj-x ownship) (obj-y ownship) 500.0)))

  (define predict-secs
    (if (missile? ownship)
        (inexact->exact (round (max 0 (- (tool-rc (ship-tool ownship 'endrc))
                                         (/ (obj-age space ownship) 1000.0)))))
        ;(inexact->exact (round (/ 250.0 (tool-val ft))))
        8
        ))

  (define origpv (struct-copy posvel (obj-posvel ownship)))
  
  (define (newf)
    (if (missile? ownship)
        #t
        (if (and ft (tool-online? ft))
            (if ((random) . < . 0.1) #f #t)
            #f)))
  (define (newc)
    (if (and st (tool-online? st))
        (random-between 0.0 2pi)
        0))

  (define (perturb-state! s (initial? #f))
    (set-state-f! s (newf))
    (when initial? (set-state-origf! s (state-f s)))
    (set-state-c! s (angle-add (state-c s) (newc)))
    (when initial? (set-state-origc! s (state-c s))))

  (define states
    (for/list ((i 16))
      (state origf origf basec basec (struct-copy posvel origpv) 0.0 1 #t)))

  (for ((i (in-range predict-secs)))
    (for ((s (in-list ships))) (physics! (obj-posvel s) 1.0))

    ;(printf "states is now ~v\n" states)
    (for ((s (in-list states))
          (k (in-naturals))
          #:when (state-live? s))
      ; randomly pick what to do next in this state
      ; but keep the first state as "what if we don't change anything?"
      (when (k . > . 0)
        (perturb-state! s (= i 0)))
      ; put state into ownship
      (set-obj-posvel! ownship (state-pv s))
      (when ft (set-tool-rc! ft (state-f s)))
      (when st (set-tool-rc! st (state-c s)))
      ; time update ownship      
      (for ((z (in-range 5)))
        (update-physics! space ownship 0.2))
      ; calculate fitness
      (define-values (f live?)
        ((if (missile? ownship) missile-fitness pilot-fitness) space qt ownship))

      (set-state-fitness! s (+ (state-fitness s) f))
      (set-state-live?! s live?)
      (set-state-length! s (+ 1 (state-length s)))
      ))
      
  (define bestfit #f)
  (define bestf #f)
  (define bestc #f)
  (for ((s (in-list states)))
    ;(append! changes (chadd (effect (next-id) (space-time space) #t
    ;                                (struct-copy posvel (state-pv s) [dx 0.0] [dy 0.0])
    ;                                1.0 1000) #f))
    ;(printf "state ~v\n" s)
    (define f (/ (state-fitness s) (state-length s)))
    (when (or (not bestfit)
              (f . > . (+ bestfit 1.0)))
      (set! bestfit f)
      (set! bestf (state-origf s))
      (set! bestc (state-origc s))))

  ;(printf "\n\n")
  
  (for ((s (in-list ships))) (physics! (obj-posvel s) (- predict-secs)))
  (set-obj-posvel! ownship origpv)

  (when ft
    (set-tool-rc! ft origf)
    (when (and (not (equal? origf bestf)) (tool-online? ft))
      (append! changes (command (ob-id ownship) #f 'engine bestf))))
  (when st
    (set-tool-rc! st origc)
    (when (and (not (equal? origc bestc)) (tool-online? st))
      (append! changes (command (ob-id ownship) #f 'steer bestc))))
  
  changes)


;; client/server

(define (launch! cmd space stack who)
  (define ships (get-ships stack))
  (cond
    ((not (can-launch? stack))
     (printf "~a discarding message (can't launch) ~v\n" who cmd)
     (values #f '()))
    (((length ships) . < . 2)
     (printf "~a discarding message (less than 2 ships in stack) ~v\n" who cmd)
     (values #f '()))
    (else
     (define ship (car ships))
     (define parent (cadr ships))
     (define r (angle-add (obj-r parent) pi))
     (define dist (+ (ship-radius ship)
                     (ship-radius parent)
                     (random-between 9 11)))
     (define pv (posvel 0
                        (+ (posvel-x (obj-posvel parent)) (* dist (cos r)))
                        (+ (posvel-y (obj-posvel parent)) (* dist (sin r)))
                        r
                        (- (posvel-dx (obj-posvel parent)) 2.0)
                        (- (posvel-dy (obj-posvel parent)) 2.0)
                        0))
     (values #f (list (chmov (ob-id ship) #f pv))))))

#;(define (change-pilot-tool! cmd space stack who)
  (define tool (car stack))
  (cond
    ((dock? tool)
     (cond
       ((boolean? cmd)
        (set-dock-on! tool cmd)
        (values #t '()))
       ((equal? cmd "launch")
        ; launch this ship off of it's parent
        )))
    ((steer? tool)
     (set-steer-course! tool cmd)
     (values #t '()))
    ((fthrust? tool)
     (set-fthrust-on! tool cmd)
     (values #t '()))
    ((shbolt? tool)
     (define a cmd)  ; cmd is angle to fire
     (define ship (get-ship stack))
     (define pod (get-pod stack))
     (cond
       ((not (ship-flying? ship))
        (printf "~a discarding message (not flying) ~v\n" who cmd)
        (values #f '()))
       ((not ((pod-energy pod) . > . (shbolt-shield-size tool)))
        (printf "~a discarding message (not enough energy) ~v\n" who cmd)
        (values #f '()))
       (else
        (define po (pod-obj pod ship))
        
        (define sh (shield (next-id) (space-time space)
                           (posvel (space-time space) (obj-x po) (obj-y po) (obj-r po)
                                   (+ (* SHIELD_SPEED (cos a)) (posvel-dx (obj-posvel po)))
                                   (+ (* SHIELD_SPEED (sin a)) (posvel-dy (obj-posvel po)))
                                   0)
                           (shbolt-shield-size tool)))
        
        (values #f (list (chadd sh #f) (cherg (ob-id pod) (- (shbolt-shield-size tool))))))))))


;; client



#;(define (draw-pilot-fitness dc space ship)
  
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
                             (and (spaceship? o)
                                  (not (= (ob-id ship) (ob-id o)))))
                           (space-objects space)))
  
  (send dc set-brush nocolor 'transparent)
  (define p (ship-pilot ship))
  (define origp (struct-copy pilot p))
  (for* ((f (in-list '(#f #t)))
         (c (in-list '(0 -10 10 -40 40 180))))
    (define origpv (struct-copy posvel (obj-posvel ship)))
    (set-pilot-fore! p f)
    (set-pilot-course! p (angle-add (pilot-course origp) (degrees->radians c)))
    (define curfit 0.0)
    
    (define predict-secs (inexact->exact (round (/ 150 (stats-thrust (ship-stats ship))))))
    (for ((i (in-range predict-secs)))
      (define-values (oldx oldy) (recenter center ship))
      (update-physics! space ship 0.5)
      (update-physics! space ship 0.5)
      (define f (pilot-fitness space ship))
      (set! curfit (+ curfit f))
      (define normfit (/ curfit (add1 i)))
      (define-values (newx newy) (recenter center ship))
      (define cc (if (normfit . > . 0)
                     (linear-color "blue" "green" (sigmoid normfit 20) 1.0)
                     (linear-color "blue" "red" (- (sigmoid normfit 20)) 1.0)))
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
       (define ne (find-top-id space (strategy-arg strat)))
       (when ne
         (define-values (x y) (recenter ship ne))
         (send dc draw-ellipse (- x AI_GOTO_DIST) (- y AI_GOTO_DIST)
               (* AI_GOTO_DIST 2) (* AI_GOTO_DIST 2)))))))
