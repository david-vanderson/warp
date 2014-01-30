#lang racket/base

(require racket/math
         "defs.rkt"
         "client.rkt"
         "server.rkt")

(define ownspace
  (space
   0 4000 4000
   (list
    (big-ship "Rebel1" "Rebel" 0 0 0 #f #t #f #f #t #f)
    (big-ship "Empire1" "Empire" 100 0 (* 1/2 pi) #f #t #f #f #f #f))))

;(set-posvel-dy! (obj-posvel (cadr (space-objects ownspace))) -10)

(thread (lambda () (start-server PORT ownspace)))

(sleep 0.5)

(thread (lambda () (start-client "127.0.0.1" PORT "Dave" #t)))
;(thread (lambda () (start-client "127.0.0.1" PORT "Andrea" #t)))

(semaphore-wait (make-semaphore))

