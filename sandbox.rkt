#lang racket/base

(require "defs.rkt"
         "client.rkt"
         "server.rkt"
         "ships.rkt")

(define ownspace
  (space
   0 4000 4000
   (list
    (make-ship "blue-frigate" "Rebel1" "Rebel"
               #:start-ship? #t #:npc? #t)
    (make-ship "blue-fighter" "Red 5" "Rebel"
               #:start-ship? #t #:npc? #t)
    #;(big-ship "Empire1" "Empire" 400 0 pi/2 #f #t #f #t #t #t)
    #;(big-ship "Empire2" "Empire" 600 0 (- pi/2) #f #t #f #t #t #t))))


(thread (lambda () (start-server PORT ownspace)))

(sleep 0.5)

(thread (lambda () (start-client "127.0.0.1" PORT "Dave" #t)))
;(thread (lambda () (start-client "127.0.0.1" PORT "Andrea" #t)))

(semaphore-wait (make-semaphore))

