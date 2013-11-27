#lang racket/base

(require racket/math
         "defs.rkt"
         "client.rkt"
         "server.rkt")

(define ownship (ship 0 0 (* 0.5 3 pi) 0 0 0
                      (helm #f (* 0.5 3 pi) #f #f #f #f)
                      (list 
                       (shield 57 "blue" 100
                                    (make-vector 16 100))
                       (shield 50 "red" 100
                                    (make-vector 16 100)))))

(define ownspace (space (list ownship)))


(thread (lambda () (start-server ownspace)))
(start-client)