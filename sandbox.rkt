#lang racket/base

(require racket/math
         "defs.rkt"
         "client.rkt"
         "server.rkt")

(define ownship (ship 0 0 (* 0.5 pi) 0 0 0
                      (random-id)
                      (helm #f (* 0.5 pi) #f #f #f #f)
                      (list 
                       (shield 57 "blue" 100
                               (make-vector 16 0))
                       (shield 50 "red" 100
                               (make-vector 16 0)))))

(vector-set! (shield-sections (car (ship-shields ownship))) 0 100)

(define ownspace (space (list #;ownship)))

(define (new-client ownspace player)
  (printf "new-client ~a\n" (player-id player))
  (define x (* 200 (sub1 (player-id player))))
  (define new-ship
    (ship x 0 (* 0.5 pi) 0 0 0
          (random-id)
          (helm player (* 0.5 pi) #t #f #f #f)
          (list 
           (shield 57 "blue" 100 (make-vector 16 0))
           (shield 50 "red" 100 (make-vector 16 0)))))
  (vector-set! (shield-sections (car (ship-shields new-ship))) 0 100)
  ;(vector-set! (shield-sections (cadr (ship-shields new-ship))) 8 100)
  (set-space-objects! ownspace (cons new-ship (space-objects ownspace))))

(thread (lambda () (start-server ownspace new-client)))

(thread (lambda () (start-client (player 1 "Dave"))))
(thread (lambda () (start-client (player 2 "Andrea"))))

(semaphore-wait (make-semaphore))
