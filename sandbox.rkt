#lang racket/base

(require racket/math
         "defs.rkt"
         "client.rkt"
         "server.rkt")

(define ownspace (space 0 2000 2000 (list (big-ship 0 0 "Ship1") #;(big-ship 200 0 "Ship2"))))

(thread (lambda () (start-server PORT ownspace)))

(sleep 0.5)

(thread (lambda () (start-client "127.0.0.1" PORT "Dave" #t)))
;(thread (lambda () (start-client "127.0.0.1" PORT "Andrea" #t)))

(semaphore-wait (make-semaphore))
