#lang racket/base

(require racket/math)

(require "defs.rkt"
         "client.rkt"
         "server.rkt"
         "ships.rkt")

(define enemy-base
  (make-ship "red-station" "Empire Base" "Empire" #:npc? #t #:x 1000 #:y 100
      #:in-hangar
      (list (make-ship "red-fighter" "Empire Fighter" "Empire" #:npc? #t)
            (make-ship "red-fighter" "Empire Fighter" "Empire" #:npc? #t)
            (make-ship "red-fighter" "Empire Fighter" "Empire" #:npc? #t)
            (make-ship "red-fighter" "Empire Fighter" "Empire" #:npc? #t))))

(set-stats-con! (ship-stats enemy-base) 200)

(define f (make-ship "red-fighter" "Empire1" "Empire" #:npc? #t #:start-ship? #t #:x 300 #:y 10))
(set-ship-ai-strategy! f (list (strategy 0 "return" (ob-id enemy-base))))

(define ownspace
  (space
   0 2000 2000
   (list
    
    ;f
    
    ;(make-ship "blue-fighter" "Blue 6" "Rebel" #:npc? #t #:x 400 #:y 0)
    
    (make-ship "blue-station" "Rebel Base" "Rebel" #:npc? #t #:x 0 #:y 0 #:start-ship? #t
      #:in-hangar
      (list (make-ship "blue-frigate" "Rebel Frigate" "Rebel"
              #:in-hangar
              (list (make-ship "blue-fighter" "Rebel Fighter" "Rebel")
                    (make-ship "blue-fighter" "Rebel Fighter" "Rebel")))
            (make-ship "blue-frigate" "Rebel Frigate" "Rebel"
              #:in-hangar
              (list (make-ship "blue-fighter" "Rebel Fighter" "Rebel")
                    (make-ship "blue-fighter" "Rebel Fighter" "Rebel")))
            (make-ship "blue-frigate" "Rebel Frigate" "Rebel"
              #:in-hangar
              (list (make-ship "blue-fighter" "Rebel Fighter" "Rebel")
                    (make-ship "blue-fighter" "Rebel Fighter" "Rebel")))
            (make-ship "blue-fighter" "Rebel Fighter" "Rebel" #:npc? #t)
            (make-ship "blue-fighter" "Rebel Fighter" "Rebel" #:npc? #t)
            (make-ship "blue-fighter" "Rebel Fighter" "Rebel" #:npc? #t)
            (make-ship "blue-fighter" "Rebel Fighter" "Rebel" #:npc? #t)))
    
    enemy-base
    )))


(thread (lambda ()
(start-server PORT ownspace)
))

;(thread (lambda () (start-client "127.0.0.1" PORT "Dave" #t #f)))
;(thread (lambda () (start-client "127.0.0.1" PORT "Andrea" #t #f)))

(semaphore-wait (make-semaphore))

