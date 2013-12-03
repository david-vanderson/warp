#lang racket/base

(require racket/math
         "defs.rkt"
         "client.rkt"
         "server.rkt")

(define (big-ship x y)
    (ship (next-id) (posvel x y (* 0.5 pi) 0 0 0)
          (helm (next-id) #f #f (* 0.5 pi) #f #f #f #f)
          100 1
          (list 
           (shield (next-id) #f 57 "blue" 100 (make-vector 16 50))
           (shield (next-id) #f 50 "red" 100 (make-vector 16 50)))))

(define ownspace (space 0 (list (big-ship 0 0) (big-ship 100 0))))

(define (new-client ownspace player)
  (printf "new-client ~a\n" (obj-id player))
  (define ship (car (space-objects ownspace)))
  (set-role-player! (ship-helm ship) player))

(thread (lambda () (start-server PORT ownspace new-client)))

(sleep 0.5)

(thread (lambda () (start-client "127.0.0.1" PORT (player 1 #f "Dave") #t)))
;(thread (lambda () (start-client "127.0.0.1" PORT (player 2 #f "Andrea") #t)))

(semaphore-wait (make-semaphore))
