#lang racket/base

(require racket/math
         "defs.rkt"
         "client.rkt"
         "server.rkt")

(define ownship (ship 0 0 (* 0.5 3 pi) 0 0 0
                      (helm #f (* 0.5 3 pi) #f #f #f #f)
                      (list (shield 100 "red" 100
                                    '(100 50 25 0 20 20 20 20 20 20 20 20 20 20 20 20))
                            (shield 107 "blue" 100
                                    '(100 50 25 0 20 20 20 20 20 20 20 20 20 20 20 20)))))

(define ownspace (space (list ownship)))


(thread (lambda () (start-server ownspace)))
(start-client)