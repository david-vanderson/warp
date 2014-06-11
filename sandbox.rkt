#lang racket/base

(require racket/math)

(require "defs.rkt"
         "client.rkt"
         "server.rkt"
         "ships.rkt")

(define ownspace
  (space
   0 2000 2000
   (list
    
    ;(make-ship "space-suit" "Dave" "Empire" #:start-ship? #t)
    (make-ship "blue-frigate" "Blue Fighter" "Rebel" #:start-ship? #t #:x 200 #:helm-fore? #t)
    (make-ship "red-fighter" "Red 1" "Empire" #:npc? #t #:npc-helm? #f #:x 0 #:y 0)
    (make-ship "red-fighter" "Red 1" "Empire" #:npc? #t #:npc-helm? #f #:x 0 #:y 50)
    (make-ship "red-fighter" "Red 1" "Empire" #:npc? #t #:npc-helm? #f #:x 0 #:y -50)
    
;    (make-ship "red-frigate" "Empire1" "Empire" #:npc? #t #:x 500 #:y 0 #:r pi)
;    (make-ship "red-fighter" "Red 1" "Empire" #:npc? #t #:x 500 #:y 100 #:r pi)
;    (make-ship "red-fighter" "Red 2" "Empire" #:npc? #t #:x 500 #:y -100 #:r pi)
    
    ;(make-ship "blue-fighter" "Blue 5" "Rebel" #:start-ship? #t #:npc? #t #:x 0 #:y 0)
;    (make-ship "blue-fighter" "Blue 6" "Rebel" #:start-ship? #t #:npc? #t #:x -400 #:y -100)
;    (make-ship "blue-frigate" "Rebel1" "Rebel" #:npc? #t #:x -400 #:y 0 #:start-ship? #t
;               #:in-hangar (list
;                            (make-ship "blue-fighter" "Blue 1" "Rebel" #:npc? #t #:posvel? #f)
;                            (make-ship "blue-fighter" "Blue 2" "Rebel" #:npc? #t #:posvel? #f)))
    
    
;    (make-ship "blue-fighter" "RF 1" "Rebel" #:npc? #t #:x -300 #:y 50 #:start-ship? #t)
;    (make-ship "blue-frigate" "Rebel1" "Rebel" #:npc? #t #:x -200 #:y 100)
;    (make-ship "blue-frigate" "Rebel1" "Rebel" #:npc? #t #:x -200 #:y 200)
;    (make-ship "red-frigate" "Empire1" "Empire" #:npc? #t #:x 200 #:y 100 #:r pi)
;    (make-ship "red-frigate" "Empire1" "Empire" #:npc? #t #:x 200 #:y 300 #:r pi)
;    (make-ship "blue-fighter" "RF 2" "Rebel" #:npc? #t #:x -300 #:y 150)
;    (make-ship "red-fighter" "EF 1" "Empire" #:npc? #t #:x 300 #:y 50 #:r pi)
;    (make-ship "red-fighter" "EF 2" "Empire" #:npc? #t #:x 200 #:y 150 #:r pi)
;    (make-ship "blue-fighter" "EF 3" "Empire" #:npc? #t #:x 200 #:y 75)
;    (make-ship "blue-fighter" "Red 5" "Rebel" #:start-ship? #t #:npc? #t #:x 0 #:y 20 #:r pi #:dx -20)
    ;(big-ship "Empire1" "Empire" 400 0 pi/2 #f #t #f #t #t #t)
    ;(big-ship "Empire2" "Empire" 600 0 (- pi/2) #f #t #f #t #t #t)
    )))


(thread (lambda ()
(start-server PORT ownspace)
))

(thread (lambda () (start-client "127.0.0.1" PORT "Dave" #t #f)))
;(thread (lambda () (start-client "127.0.0.1" PORT "Andrea" #t)))

(semaphore-wait (make-semaphore))

