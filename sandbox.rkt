#lang racket/base

(require racket/math)

(require "defs.rkt"
         "client.rkt"
         "server.rkt"
         "ships.rkt")

(define ownspace
  (space
   0 4000 4000
   (list
;    (make-ship "blue-frigate" "Rebel1" "Rebel" #:npc? #t #:x -200 #:y 0
;               #:in-hangar (list
;                            (make-ship "blue-fighter" "Blue 1" "Rebel" #:npc? #t #:posvel? #f)))
    
;    (make-ship "blue-frigate" "R1" "Rebel" #:npc? #t #:x -200 #:y 0 #:r 0.5)
;    (make-ship "blue-frigate" "E1" "Empire" #:npc? #t #:x -200 #:y 50 #:r -0.5)

    
    (make-ship "blue-fighter" "RF 1" "Rebel" #:npc? #t #:x -300 #:y 50)
    (make-ship "blue-frigate" "Rebel1" "Rebel" #:npc? #t #:x -200 #:y 0)
    (make-ship "blue-frigate" "Rebel1" "Rebel" #:npc? #t #:x -200 #:y 100)
    (make-ship "blue-frigate" "Rebel1" "Rebel" #:npc? #t #:x -200 #:y 200)
    (make-ship "blue-frigate" "Empire1" "Empire" #:npc? #t #:x 200 #:y 0 #:r pi)
    (make-ship "blue-frigate" "Empire1" "Empire" #:npc? #t #:x 200 #:y 100 #:r pi)
    (make-ship "blue-frigate" "Empire1" "Empire" #:npc? #t #:x 200 #:y 300 #:r pi)
    (make-ship "blue-fighter" "RF 2" "Rebel" #:npc? #t #:x -300 #:y 150)
    (make-ship "blue-fighter" "EF 1" "Empire" #:npc? #t #:x 300 #:y 50 #:r pi)
    (make-ship "blue-fighter" "EF 2" "Empire" #:npc? #t #:x 200 #:y 150 #:r pi)
;    (make-ship "blue-fighter" "EF 3" "Empire" #:npc? #t #:x 200 #:y 75)
    ;(make-ship "blue-fighter" "Red 5" "Rebel" #:start-ship? #t #:npc? #t #:x 100 #:y 20 #:r pi #:dx -20)
    ;(big-ship "Empire1" "Empire" 400 0 pi/2 #f #t #f #t #t #t)
    ;(big-ship "Empire2" "Empire" 600 0 (- pi/2) #f #t #f #t #t #t)
    )))


(thread (lambda () (start-server PORT ownspace)))

(sleep 0.5)

(thread (lambda () (start-client "127.0.0.1" PORT "Dave" #t)))
;(thread (lambda () (start-client "127.0.0.1" PORT "Andrea" #t)))

(semaphore-wait (make-semaphore))

