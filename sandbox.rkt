#lang racket/base

(require racket/math)

(require "defs.rkt"
         "utils.rkt"
         "client.rkt"
         "server.rkt"
         "ships.rkt")

(define enemy-base
  (make-ship "red-station" "Empire Base" "Empire" #:x 1000 #:y 100
      #:in-hangar
      (list (make-ship "red-fighter" "Empire Fighter" "Empire")
            (make-ship "red-fighter" "Empire Fighter" "Empire")
            (make-ship "red-fighter" "Empire Fighter" "Empire")
            (make-ship "red-fighter" "Empire Fighter" "Empire"))))

(set-stats-con! (ship-stats enemy-base) 200)

(define f (make-ship "red-fighter" "Empire1" "Empire" #:start-ship? #t #:x 300 #:y 10))

(define ownspace
  (space
   0 2000 2000
   (list
    
    ;f
    
    ;(make-ship "blue-fighter" "Blue 6" "Rebel" #:x 400 #:y 0)
    
    (make-ship "blue-station" "Rebel Base" "Rebel" #:x 0 #:y 0 #:start-ship? #t
      #:in-hangar
      (list (make-ship "blue-frigate" "Rebel Frigate" "Rebel"
              #:in-hangar
              (list (make-ship "blue-fighter" "Rebel Fighter" "Rebel")
                    (make-ship "blue-fighter" "Rebel Fighter" "Rebel")))
            (make-ship "blue-frigate" "Rebel Frigate" "Rebel"
              #:in-hangar
              (list (make-ship "blue-fighter" "Rebel Fighter" "Rebel")
                    (make-ship "blue-fighter" "Rebel Fighter" "Rebel")))
            (make-ship "blue-frigate" "Rebel Frigate" "Rebel"
              #:in-hangar
              (list (make-ship "blue-fighter" "Rebel Fighter" "Rebel")
                    (make-ship "blue-fighter" "Rebel Fighter" "Rebel")))
            (make-ship "blue-fighter" "Rebel Fighter" "Rebel")
            (make-ship "blue-fighter" "Rebel Fighter" "Rebel")
            (make-ship "blue-fighter" "Rebel Fighter" "Rebel")
            (make-ship "blue-fighter" "Rebel Fighter" "Rebel")))
    
    enemy-base
    )))

(define next-enemy-count 0)
(define last-base-con 10000)
(define last-enemy-base-con 10000)

; return a list of changes
(define (hook space)
  (define commands '())
  
;  (when (<= 1 (modulo (space-time space) 1800) TICK)
;    (set! commands (append commands (list (message (next-id) (space-time space) #f
;                                                   (format "Time ~a" (space-time space)))))))
  
;  (when (<= 15001 (space-time space) (+ 15000 TICK))
;    (define eb (find-id space (lambda (o) (and (ship? o) (equal? "red-station" (ship-type o))))))
;    (set! commands (append commands (list (chdam (ob-id eb) 200)))))
  
  (define hb (find-id space (lambda (o) (and (ship? o) (equal? "blue-station" (ship-type o))))))
  (when (and hb ((ship-con hb) . < . (- last-base-con 100)))
    (define m (message (next-id) (space-time space) #f (format "Base Health: ~a" (inexact->exact (round (ship-con hb))))))
    (set! commands (append commands (list m)))
    (set! last-base-con (ship-con hb)))
  
  (define eb (find-id space (lambda (o) (and (ship? o) (equal? "red-station" (ship-type o))))))
  (when (and eb (< (ship-con eb) (- last-enemy-base-con 50)))
    (define m (message (next-id) (space-time space) #f (format "Enemy Base Health: ~a" (inexact->exact (round (ship-con eb))))))
    (set! commands (append commands (list m)))
    (set! last-enemy-base-con (ship-con eb)))
  
  (when (and hb (>= (space-time space) (+ 1000 (* 5 60000 next-enemy-count))))
  ;(when (and hb (<= 5001 (space-time space) (+ 5000 TICK)))
    (set! next-enemy-count (+ 1 next-enemy-count))
    (for ((i next-enemy-count))
      (define a (random-between pi/2 (* 3/2 pi)))
      (define r (random-between 1000 1500))
      (define ns
        (make-ship "red-frigate" "Empire Frigate" "Empire" #:x (* r (cos a)) #:y (* r (sin a))
                   #:in-hangar
                   (list (make-ship "red-fighter" "Empire Fighter" "Empire")
                         #;(make-ship "red-fighter" "Empire Fighter" "Empire"))))
      (set-ship-ai-strategy! ns (list (strategy (space-time space) "attack" (ob-id hb))))
      (set! commands (append commands (list (chadd ns #f)))))
    (define m (message (next-id) (space-time space) #f (format "~a new enemy ships detected!" next-enemy-count)))
    (set! commands (append commands (list m))))
  
;  (define types (map ship-type (filter ship? (space-objects space))))
;  ;(printf "types ~v\n" types)
;  (define (count-type type)
;    (length (filter (lambda (t) (equal? t type)) types)))
;  
;  (define theta (random-between 0 2pi))
;  (define r (random-between 0 500))
;  (define x (* r (cos theta)))
;  (define y (* r (sin theta)))
;  
;  (cond
;    (((count-type "blue-frigate") . < . 1)
;     (define s (make-ship "blue-frigate" "Blue Frigate" "Rebel" #:x x #:y y #:r (angle-add theta pi) #:start-ship? #t
;                 #:in-hangar (list
;                              (make-ship "blue-fighter" "Blue Fighter" "Rebel")
;                              (make-ship "blue-fighter" "Blue Fighter" "Rebel"))))
;     (set! commands (append commands (list (chadd s #f)))))
;    (((count-type "red-frigate") . < . 1)
;     (define s (make-ship "red-frigate" "Red Frigate" "Empire" #:x x #:y y #:r (angle-add theta pi) #:start-ship? #t
;                 #:in-hangar (list
;                              (make-ship "red-fighter" "Red Fighter" "Empire")
;                              (make-ship "red-fighter" "Red Fighter" "Empire"))))
;     (set! commands (append commands (list (chadd s #f))))))
  
  
;    (((count-type "blue-fighter") . < . 2)
;     (define s (make-ship "blue-fighter" "Blue Fighter" "Rebel" #:start-ship? #t #t #:x x #:y y #:r (angle-add theta pi)))
;     (set! commands (append commands (list (chadd s #f)))))
;    (((count-type "red-fighter") . < . 2)
;     (define s (make-ship "red-fighter" "Red Fighter" "Empire" #t #:x x #:y y #:r (angle-add theta pi)))
;     (set! commands (append commands (list (chadd s #f))))))
  
  commands)

; return a list of changes
(define (on-destroy ownspace)
  (define commands '())
  (printf "on-destroy\n")
  commands)

(thread (lambda ()
(start-server PORT ownspace hook on-destroy)
))

;(thread (lambda () (start-client "127.0.0.1" PORT "Dave" #t #f)))
;(thread (lambda () (start-client "127.0.0.1" PORT "Andrea" #t #f)))

(semaphore-wait (make-semaphore))

