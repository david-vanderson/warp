#lang racket/base

(require racket/math
         "defs.rkt"
         "client.rkt"
         "server.rkt"
         "physics.rkt")

(define ownspace
  (space
   0 4000 4000
   (list
    (big-ship "Rebel1" "Rebel" 0 0 0 #f #t #f #t #t #t)
    (big-ship "Empire1" "Empire" 200 0 pi/2 #f #t #f #t #t #t))))

;(for ((s (space-objects ownspace)))
;  (printf "ship ~a\n" (ship-name s))
;  (for ((p (ship-pods s)))
;    (printf "pod ~a ~a\n" (role-name (pod-role p)) (pod-energy p))))
;(newline)
;(for ((o (space-objects ownspace)))
;      (when (ship? o)
;        (update-energy! (/ TICK 1000.0) o 0)))
;(for ((s (space-objects ownspace)))
;  (printf "ship ~a\n" (ship-name s))
;  (for ((p (ship-pods s)))
;    (printf "pod ~a ~a\n" (role-name (pod-role p)) (pod-energy p))))

;(set-posvel-dy! (obj-posvel (cadr (space-objects ownspace))) -10)

(thread (lambda () (start-server PORT ownspace)))

(sleep 0.5)

(thread (lambda () (start-client "127.0.0.1" PORT "Dave" #t)))
;(thread (lambda () (start-client "127.0.0.1" PORT "Andrea" #t)))

(semaphore-wait (make-semaphore))

