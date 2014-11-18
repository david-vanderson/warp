#lang racket/base

(require racket/math)

(require "defs.rkt"
         "utils.rkt"
         "client.rkt"
         "server.rkt"
         "ships.rkt")

(define ownspace (space 0 4000 2000 '()))

(define base (make-ship "blue-station" "a" "a" #:x -1500 #:y -100 #:start-ship? #t))
(set-ship-stats! base (stats (next-id) "blue-station" "Rebel Outpost" "Rebel" 10.0 200.0 200.0 1000.0 1000.0 26.0 1000.0 0.0 0.0))
(set-ship-pods!
 base
 `(,(multipod (next-id) (observer (next-id) #f #f) 0.0 0.0 #f #f 0.0 0.0 #f '())
   ,(hangarpod (next-id) (hangar (next-id) #f #f) pi 13.0 #f #f 0.0 0.0 #f '() '())
   ,@(for/list ((d (in-list (list 0 90 180 270))))
       (weapon (next-id) (weapons (next-id) #f #t #f)
               (degrees->radians d) 26.0 (degrees->radians d) (* 0.8 pi) 50.0 50.0 5.0))
   ,@(for/list ((d (in-list (list 45 135 225 315))))
       (tactical (next-id) (tactics (next-id) #f #t #f)
                 (degrees->radians d) 28.0 (degrees->radians d) (* 0.8 pi) 100.0 100.0 10.0))))

(define destroyer
  (make-ship "red-destroyer" "b" "b" #:x -1000 #:y 100 #:r pi #:start-ship? #t))

(set-ship-stats! destroyer (stats (next-id)
                                  ;type name faction
                                  "red-destroyer" "Empire Destroyer" "Empire"
                                  ;power bat maxbat con maxcon radius mass thrust rthrust
                                  10.0 500.0 500.0 500.0 500.0 23.0 500.0 4.0 0.1))
(set-ship-pods!
 destroyer
 `(,(helm (next-id) (pilot (next-id) #f #t pi #t #f #f) 0.0 0.0 #f #f 100.0 100.0)
   ,(multipod (next-id) (observer (next-id) #f #f) 0.0 10.0 #f #f 0.0 0.0 #f '())
   ,(hangarpod (next-id) (hangar (next-id) #f #f) pi 10.0 #f #f 0.0 0.0 #f '() '())
   ,@(for/list ((d (in-list (list -10 10))))
       (weapon (next-id) (weapons (next-id) #f #t #f)
               ;angle dist facing spread energy maxe shot-size
               (degrees->radians d) 23.0 0.0 (* 0.4 pi) 200.0 200.0 20.0))
   ,@(for/list ((d (in-list (list -62 62))))
       (weapon (next-id) (weapons (next-id) #f #t #f)
               (degrees->radians d) 23.0 (degrees->radians d) (* 0.8 pi) 100.0 100.0 10.0))
   ,@(for/list ((d (in-list (list -130 130))))
       (weapon (next-id) (weapons (next-id) #f #t #f)
               (degrees->radians d) 21.0 (degrees->radians d) (* 0.8 pi) 50.0 50.0 5.0))
   ,@(for/list ((d (in-list (list -35 35))))
       (tactical (next-id) (tactics (next-id) #f #t #f)
                 (degrees->radians d) 24.0 (degrees->radians d) (* 0.7 pi) 150.0 150.0 15.0))
   ,@(for/list ((d (in-list (list -90 90))))
       (tactical (next-id) (tactics (next-id) #f #t #f)
                 (degrees->radians d) 21.0 (degrees->radians d) (* 0.9 pi) 50.0 50.0 5.0))))

(set-ship-ai-strategy! destroyer
                       (list (strategy (space-time ownspace) "attack" (ob-id base))))



(set-space-objects! ownspace (list base destroyer))



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
      (set! commands (append commands (list (chadd ns)))))
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
;     (set! commands (append commands (list (chadd s)))))
;    (((count-type "red-frigate") . < . 1)
;     (define s (make-ship "red-frigate" "Red Frigate" "Empire" #:x x #:y y #:r (angle-add theta pi) #:start-ship? #t
;                 #:in-hangar (list
;                              (make-ship "red-fighter" "Red Fighter" "Empire")
;                              (make-ship "red-fighter" "Red Fighter" "Empire"))))
;     (set! commands (append commands (list (chadd s))))))
  
  
;    (((count-type "blue-fighter") . < . 2)
;     (define s (make-ship "blue-fighter" "Blue Fighter" "Rebel" #:start-ship? #t #t #:x x #:y y #:r (angle-add theta pi)))
;     (set! commands (append commands (list (chadd s)))))
;    (((count-type "red-fighter") . < . 2)
;     (define s (make-ship "red-fighter" "Red Fighter" "Empire" #t #:x x #:y y #:r (angle-add theta pi)))
;     (set! commands (append commands (list (chadd s))))))
  
  commands)


(thread (lambda ()
(start-server PORT ownspace hook)
))

;(thread (lambda () (start-client "127.0.0.1" PORT "Dave" #t #f)))
;(thread (lambda () (start-client "127.0.0.1" PORT "Andrea" #t #f)))

(semaphore-wait (make-semaphore))

