#lang racket/base

(require racket/math
         "defs.rkt"
         "client.rkt"
         "server.rkt")

(define ownspace
  (space
   0 4000 4000
   (list
    (big-ship "Rebel1" #t "Rebel" 0 0 0 #f #t)
    (big-ship "Empire1" #t "Empire" 200 100 pi #f #t))))

(thread (lambda () (start-server PORT ownspace)))

(sleep 0.5)

(thread (lambda () (start-client "127.0.0.1" PORT "Dave" #t)))
;(thread (lambda () (start-client "127.0.0.1" PORT "Andrea" #t)))

(semaphore-wait (make-semaphore))

