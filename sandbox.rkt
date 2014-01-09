#lang racket/base

(require racket/math
         "defs.rkt"
         "client.rkt"
         "server.rkt")

(define ownspace
  (space
   0 4000 4000
   (list
    (big-ship "Rebel1" #f "Rebel" 0 0 0 #f)
    (big-ship "Empire1" #f "Empire" 400 0 pi #f))))

(thread (lambda () (start-server PORT ownspace)))

(sleep 0.5)

(thread (lambda () (start-client "127.0.0.1" PORT "Dave" #t)))
;(thread (lambda () (start-client "127.0.0.1" PORT "Andrea" #t)))

(semaphore-wait (make-semaphore))

