#lang racket/base

(require racket/math
         racket/gui)

(require "defs.rkt"
         "utils.rkt"
         "client.rkt"
         "server.rkt"
         "ships.rkt")

(define enemy-base
  (make-ship "red-station" "Empire Base" "Empire" #:x 0 #:y 100 #:r pi/2 #:start-ship? #t
      #:in-hangar
      (list
       (make-ship "red-fighter" "Empire Fighter" "Empire")
       (make-ship "red-fighter" "Empire Fighter" "Empire")
       (make-ship "red-fighter" "Empire Fighter" "Empire")
       (make-ship "red-fighter" "Empire Fighter" "Empire")
       (make-ship "red-fighter" "Empire Fighter" "Empire")
       (make-ship "red-fighter" "Empire Fighter" "Empire")
       (make-ship "red-fighter" "Empire Fighter" "Empire")
       (make-ship "red-fighter" "Empire Fighter" "Empire")
       (make-ship "red-fighter" "Empire Fighter" "Empire")
       (make-ship "red-fighter" "Empire Fighter" "Empire")
       (make-ship "red-fighter" "Empire Fighter" "Empire")
       )))

(set-ship-stats! enemy-base (stats (next-id) "red-station" "Empire Base" "Empire"
                                   100.0 100.0 100.0 100.0 100.0 26.0 1000.0 50.0 0.4 1000.0 0.4 #t))

(set-ship-pods!
 enemy-base `(,(normal-lounge)
              ,@(for/list ((d (in-list (list #;0 #;90 180 #;270))))
                  (pod (next-id) "W" #f #t
                       (degrees->radians d) 26.0 (degrees->radians d) (* 0.8 pi) 100.0 100.0
                       (list #;(pbolt (next-id) '() 15.0 #t)
                             (mtube (next-id) '() 100.0 50.0 "load" #f))))
              ,@(for/list ((d (in-list (list 45 135 225 315))))
                  (pod (next-id) "T" #f #t
                       (degrees->radians d) 28.0 (degrees->radians d) (* 0.8 pi) 100.0 100.0
                       (list (shbolt (next-id) '() 50.0 #t))))))

;(set-stats-con! (ship-stats enemy-base) 200)

(define f (make-ship "red-fighter" "Empire1" "Empire" #:start-ship? #t #:x 0 #:y -100 #:npc? #f))

(define ai? #f)

(define cruiser (make-ship "blue-cruiser" "z" "z" #:x 0 #:y -500))
(set-ship-stats! cruiser (stats (next-id) "blue-cruiser" "Rebel Cruiser" "Rebel"
                                ;power bat maxbat con maxcon radius mass thrust rthrust radar drag start?
                                5.0 150.0 150.0 1500.0 1500.0 15.0 100.0 30.0 1.0 5000.0 0.4 #t))
(set-ship-pods!
 cruiser
 `(,(normal-lounge)
   ,(normal-hangar pi 5.0 '())
   ,(pod (next-id) "Pilot" #f #f 0.0 5.0 #f #f 100.0 100.0
         (list (steer (next-id) '() pi/2) (fthrust (next-id) '() #f) (dock (next-id) '() #f)
               (warp (next-id) '() 300.0 0.0 "release")))
   ,(pod (next-id) "W" #f ai? (degrees->radians 90.0) 10.0 (degrees->radians 75.0) (* 0.8 pi) 60.0 60.0
         (list (pbolt (next-id) '() 5.0 #t)))
   ,(pod (next-id) "W" #f ai? (degrees->radians 45.0) 12.0 (degrees->radians 30.0) (* 0.4 pi) 40.0 40.0
         (list (pbolt (next-id) '() 20.0 #t)))
   ,(pod (next-id) "W" #f ai? (degrees->radians 270.0) 10.0 (degrees->radians 285.0) (* 0.8 pi) 60.0 60.0
         (list (pbolt (next-id) '() 5.0 #t)))
   ,(pod (next-id) "W" #f ai? (degrees->radians 315.0) 12.0 (degrees->radians 330.0) (* 0.4 pi) 40.0 40.0
         (list (pbolt (next-id) '() 20.0 #t)))
   ,(pod (next-id) "T" #f ai? (degrees->radians 0.0) 15.0 (degrees->radians 0.0) (* 0.8 pi) 50.0 50.0
         (list (shbolt (next-id) '() 5.0 #t)
               (mtube (next-id) '() 50.0 0.0 "load" #f)))
   ,(pod (next-id) "T" #f ai? (degrees->radians 180.0) 12.0 (degrees->radians 180.0) (* 0.8 pi) 50.0 50.0
         (list (shbolt (next-id) '() 20.0 #t)
               (ptube (next-id) '() 50.0 0.0 "load" #f 50.0)))))

(define ownspace
  (space
   0 10000 6000 '() '()
   `(
     ,cruiser
     #;,f

     ,@(for/list ((i 50))
         (define x (+ -2500.0 (* i 100.0)))
         (define y (random-between -3000 3000))
         (define dx 0.0)
         (define dy (random-between -150.0 150.0))
         (define dr (random-between -1.0 1.0))
         (make-ship "asteroid_43" "Asteroid" "Rebel2" #:x x #:y y #:r 0 #:dx dx #:dy dy #:dr dr #:start-ship? #t #:npc? #t))
     ;(make-ship "blue-fighter" "Blue 6" "Rebel" #:x x #:y y #:r 0 #:start-ship? #t))
    ;,(make-ship "asteroid" "Asteroid" "Rebel" #:x 100 #:y 0 #:r 0 #:dx 100.0 #:dy 100.0 #:dr 1.0 #:start-ship? #t #:npc? #t)
    ;(make-ship "red-frigate" "Red Frigate" "Empire" #:x 0 #:y 0 #:r pi #:start-ship? #t)
    ;(make-ship "blue-station" "Rebel Base" "Rebel" #:x 0 #:y 0 #:start-ship? #t)
    
    #;,enemy-base
    )))

(define next-enemy-count 0)
(define last-base-con 10000)
(define last-enemy-base-con 10000)

; return a list of changes
(define (on-tick space change-scenario!)
  (define changes '())

  (for ((p (space-players space)))
      (when (not (player-faction p))
        ; new player
        (append! changes (chfaction (ob-id p) "Rebel"))))
  
;  (when (<= 1 (modulo (space-time space) 1800) TICK)
;    (set! commands (append commands (list (message (next-id) (space-time space) #f
;                                                   (format "Time ~a" (space-time space)))))))
  
;  (when (<= 15001 (space-time space) (+ 15000 TICK))
;    (define eb (find-id space (lambda (o) (and (ship? o) (equal? "red-station" (ship-type o))))))
;    (set! commands (append commands (list (chdam (ob-id eb) 200)))))
  
;  (define hb (find-id space (lambda (o) (and (ship? o) (equal? "blue-station" (ship-type o))))))
;  (when (and hb ((ship-con hb) . < . (- last-base-con 100)))
;    (define m (message (next-id) (space-time space) #f (format "Base Health: ~a" (inexact->exact (round (ship-con hb))))))
;    (set! commands (append commands (list m)))
;    (set! last-base-con (ship-con hb)))
  
;  (define eb (find-id space (lambda (o) (and (ship? o) (equal? "red-station" (ship-type o))))))
;  (when (and eb (< (ship-con eb) (- last-enemy-base-con 50)))
;    (define m (message (next-id) (space-time space) #f (format "Enemy Base Health: ~a" (inexact->exact (round (ship-con eb))))))
;    (set! commands (append commands (list m)))
;    (set! last-enemy-base-con (ship-con eb)))
;  
;  (when (and hb (>= (space-time space) (+ 1000 (* 5 60000 next-enemy-count))))
;  ;(when (and hb (<= 5001 (space-time space) (+ 5000 TICK)))
;    (set! next-enemy-count (+ 1 next-enemy-count))
;    (for ((i next-enemy-count))
;      (define a (random-between pi/2 (* 3/2 pi)))
;      (define r (random-between 1000 1500))
;      (define ns
;        (make-ship "red-frigate" "Empire Frigate" "Empire" #:x (* r (cos a)) #:y (* r (sin a))
;                   #:in-hangar
;                   (list (make-ship "red-fighter" "Empire Fighter" "Empire")
;                         #;(make-ship "red-fighter" "Empire Fighter" "Empire"))))
;      (set-ship-ai-strategy! ns (list (strategy (space-time space) "attack" (ob-id hb))))
;      (set! commands (append commands (list (chadd ns #f)))))
;    (define m (message (next-id) (space-time space) #f (format "~a new enemy ships detected!" next-enemy-count)))
;    (set! commands (append commands (list m))))
  
;  (define types (map ship-type (filter ship? (space-objects space))))
;  ;(printf "types ~v\n" types)
;  (define (count-type type)
;    (length (filter (lambda (t) (equal? t type)) types)))
;  
;  (define theta (random-between 0 2pi))
;  (define r (random-between 0 500))
;  (define x (* r (cos theta)))
;  (define y (* r (sin theta)))
;  
;  (cond
;    (((count-type "blue-frigate") . < . 1)
;     (define s (make-ship "blue-frigate" "Blue Frigate" "Rebel" #:x x #:y y #:r (angle-add theta pi) #:start-ship? #t
;                 #:in-hangar (list
;                              (make-ship "blue-fighter" "Blue Fighter" "Rebel")
;                              (make-ship "blue-fighter" "Blue Fighter" "Rebel"))))
;     (set! commands (append commands (list (chadd s #f)))))
;    (((count-type "red-frigate") . < . 1)
;     (define s (make-ship "red-frigate" "Red Frigate" "Empire" #:x x #:y y #:r (angle-add theta pi) #:start-ship? #t
;                 #:in-hangar (list
;                              (make-ship "red-fighter" "Red Fighter" "Empire")
;                              (make-ship "red-fighter" "Red Fighter" "Empire"))))
;     (set! commands (append commands (list (chadd s #f))))))
  
  
;    (((count-type "blue-fighter") . < . 2)
;     (define s (make-ship "blue-fighter" "Blue Fighter" "Rebel" #:start-ship? #t #t #:x x #:y y #:r (angle-add theta pi)))
;     (set! commands (append commands (list (chadd s #f)))))
;    (((count-type "red-fighter") . < . 2)
;     (define s (make-ship "red-fighter" "Red Fighter" "Empire" #t #:x x #:y y #:r (angle-add theta pi)))
;     (set! commands (append commands (list (chadd s #f))))))
  
  changes)

(define (on-message space cmd change-scenario!)
  '())

(define (sc oldspace oldtick oldmessage)
  (set-space-players! ownspace (if oldspace (space-players oldspace) '()))
  (values ownspace on-tick on-message))

(thread (lambda ()
(start-server PORT sc)
))

(start-client "127.0.0.1" PORT "Dave" #f #f)
;(start-client "127.0.0.1" PORT "Andrea" #f #f)

(yield 'wait)

