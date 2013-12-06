#lang racket/base

(require racket/math
         "defs.rkt"
         "client.rkt"
         "server.rkt")

(define ownspace (space 0 2000 2000 (list (big-ship 100 100) (big-ship 200 100))))

(define (new-client ownspace player)
  (printf "new-client ~a\n" (obj-id player))
;  (define ship (car (space-objects ownspace)))
;  (set-role-player! (ship-helm ship) player)
  )

(thread (lambda () (start-server PORT ownspace new-client)))

(sleep 0.5)

(thread (lambda () (start-client "127.0.0.1" PORT (player 1 #f "Dave" #f) #t)))
;(thread (lambda () (start-client "127.0.0.1" PORT (player 2 #f "Andrea" #f) #t)))

(semaphore-wait (make-semaphore))
