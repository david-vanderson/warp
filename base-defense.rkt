#lang racket/base

(require racket/math)

(require "defs.rkt"
         "utils.rkt"
         "client.rkt"
         "server.rkt"
         "ships.rkt"
         "upgrade.rkt")

(define ai? #t)

(define ownspace (space 0 5000 2000 '()))

(define (new-blue-fighter)
  (define s (make-ship "blue-fighter" "a" "a"))
  (set-ship-stats! s (stats (next-id) "blue-fighter" "Rebel Fighter" "Rebel"
                            ;power bat maxbat con maxcon radius mass thrust rthrust
                            1.0 150.0 150.0 50.0 50.0 6.0 20.0 50.0 1.5))
  (set-ship-pods!
   s (list
      (helm (next-id) (pilot (next-id) #f #t '() 0.0 #f #f #f #f) 0.0 6.5 0.0 (* 0.2 pi) 150.0 150.0 '() 5.0)
      (multipod (next-id) (observer (next-id) #f #f '()) 0.0 3.0 #f #f 0.0 0.0 '() #f '())))
  (set-obj-posvel! s #f)
  s)

(define (new-red-fighter)
  (define s (make-ship "red-fighter" "a" "a"))
  (set-ship-stats! s (stats (next-id) "red-fighter" "Empire Fighter" "Empire"
                            ;power bat maxbat con maxcon radius mass thrust rthrust
                            1.0 100.0 100.0 20.0 20.0 6.0 20.0 50.0 1.5))
  (set-ship-pods!
   s (list
      (helm (next-id) (pilot (next-id) #f #t '() 0.0 #f #f #f #f) 0.0 6.5 0.0 (* 0.2 pi) 150.0 150.0 '() 5.0)
      (multipod (next-id) (observer (next-id) #f #f '()) 0.0 3.0 #f #f 0.0 0.0 '() #f '())))
  (set-obj-posvel! s #f)
  s)


(define cruiser (make-ship "blue-cruiser" "z" "z" #:x -1800 #:y -50 #:start-ship? #t))
(set-ship-stats! cruiser (stats (next-id) "blue-cruiser" "Rebel Cruiser" "Rebel"
                                ;power bat maxbat con maxcon radius mass thrust rthrust
                                5.0 150.0 150.0 100.0 100.0 15.0 100.0 30.0 1.0))
(set-ship-pods!
 cruiser
 `(,(helm (next-id) (pilot (next-id) #f #f '() pi/2 #f #f #f #f) 0.0 0.0 #f #f 100.0 100.0 '() #f)
   ,(multipod (next-id) (observer (next-id) #f #f '()) 0.0 8.0 #f #f 0.0 0.0 '() #f '())
   ,(hangarpod (next-id) (hangar (next-id) #f #f '()) pi 5.0 #f #f 0.0 0.0 '() #f '() (list #;(new-blue-fighter)))
   ,(weapon (next-id) (weapons (next-id) #f ai? '() #f)
               (degrees->radians 90.0) 10.0 (degrees->radians 75.0) (* 0.8 pi) 60.0 60.0 '() 5.0)
   ,(weapon (next-id) (weapons (next-id) #f ai? '() #f)
               (degrees->radians 45.0) 12.0 (degrees->radians 30.0) (* 0.4 pi) 40.0 40.0 '() 20.0)
   ,(weapon (next-id) (weapons (next-id) #f ai? '() #f)
               (degrees->radians 270.0) 10.0 (degrees->radians 285.0) (* 0.8 pi) 60.0 60.0 '() 5.0)
   ,(weapon (next-id) (weapons (next-id) #f ai? '() #f)
               (degrees->radians 315.0) 12.0 (degrees->radians 330.0) (* 0.4 pi) 40.0 40.0 '() 20.0)
   ,(tactical (next-id) (tactics (next-id) #f ai? '() #f)
                 (degrees->radians 0.0) 15.0 (degrees->radians 0.0) (* 0.8 pi) 50.0 50.0 '() 5.0)
   ,(tactical (next-id) (tactics (next-id) #f ai? '() #f)
                 (degrees->radians 180.0) 12.0 (degrees->radians 180.0) (* 0.8 pi) 50.0 50.0 '() 5.0)))


(define base (make-ship "blue-station" "a" "a" #:x -2000 #:y -100 #:start-ship? #t))
(set-ship-stats! base (stats (next-id) "blue-station" "Rebel Outpost" "Rebel"
                             ;power bat maxbat con maxcon radius mass thrust rthrust
                             10.0 500.0 500.0 1000.0 1000.0 26.0 1000.0 0.0 0.0))
(set-ship-pods!
 base
 `(,(multipod (next-id) (observer (next-id) #f #f '()) 0.0 0.0 #f #f 0.0 0.0 '() #f '())
   ,(hangarpod (next-id) (hangar (next-id) #f #f '()) pi 13.0 #f #f 0.0 0.0 '() #f '() '())
   ,@(for/list ((d (in-list (list 0 90 180 270))))
       (weapon (next-id) (weapons (next-id) #f #t '() #f)
               (degrees->radians d) 26.0 (degrees->radians d) (* 0.8 pi) 50.0 50.0 '() 5.0))
   ,@(for/list ((d (in-list (list 45 135 225 315))))
       (tactical (next-id) (tactics (next-id) #f #t '() #f)
                 (degrees->radians d) 28.0 (degrees->radians d) (* 0.8 pi) 100.0 100.0 '() 10.0))))


(define destroyer (make-ship "red-destroyer" "b" "b" #:x 2400 #:y 100 #:r pi #:start-ship? #f))
(set-ship-stats! destroyer (stats (next-id)
                                  ;type name faction
                                  "red-destroyer" "Empire Destroyer" "Empire"
                                  ;power bat maxbat con maxcon radius mass thrust rthrust
                                  10.0 500.0 500.0 500.0 500.0 23.0 500.0 4.0 0.1))
(set-ship-pods!
 destroyer
 `(,(helm (next-id) (pilot (next-id) #f #t '() pi #t #f #f #f) 0.0 0.0 #f #f 100.0 100.0 '() #f)
   ,(multipod (next-id) (observer (next-id) #f #f '()) 0.0 10.0 #f #f 0.0 0.0 '() #f '())
   ,(hangarpod (next-id) (hangar (next-id) #f #f'() ) pi 10.0 #f #f 0.0 0.0 '() #f '() '())
   ,@(for/list ((d (in-list (list -10 10))))
       (weapon (next-id) (weapons (next-id) #f #t '() #f)
               ;angle dist facing spread energy maxe shot-size
               (degrees->radians d) 23.0 0.0 (* 0.4 pi) 200.0 200.0 '() 20.0))
   ,@(for/list ((d (in-list (list -62 62))))
       (weapon (next-id) (weapons (next-id) #f #t '() #f)
               (degrees->radians d) 23.0 (degrees->radians d) (* 0.8 pi) 100.0 100.0 '() 10.0))
   ,@(for/list ((d (in-list (list -130 130))))
       (weapon (next-id) (weapons (next-id) #f #t '() #f)
               (degrees->radians d) 21.0 (degrees->radians d) (* 0.8 pi) 50.0 50.0 '() 5.0))
   ,@(for/list ((d (in-list (list -35 35))))
       (tactical (next-id) (tactics (next-id) #f #t '() #f)
                 (degrees->radians d) 24.0 (degrees->radians d) (* 0.7 pi) 150.0 150.0 '() 15.0))
   ,@(for/list ((d (in-list (list -90 90))))
       (tactical (next-id) (tactics (next-id) #f #t '() #f)
                 (degrees->radians d) 21.0 (degrees->radians d) (* 0.9 pi) 50.0 50.0 '() 5.0))))

(set-ship-ai-strategy! destroyer
                       (list (strategy (space-time ownspace) "attack" (ob-id base))))


(define special (new-blue-fighter))
(set-obj-posvel! special (posvel 0 -2100.0 -100.0 0.0 0.0 0.0 0.0))
(set-ship-cargo! special (list (random-upgrade ownspace #f) (random-upgrade ownspace #f)))
;(define special2 (new-red-fighter))
;(set-obj-posvel! special2 (posvel 0 1000.0 0.0 0.0 0.0 0.0 0.0))


(set-space-objects! ownspace (list cruiser base destroyer #;special))

;(for ((i 10) (t (in-cycle '("power" "thrust" "bat" "con"))))
;  (define u (upgrade (next-id) (space-time ownspace)
;                     (posvel (space-time ownspace) -1700 (+ -250 (* i 50)) 0 0 0 0)
;                     t))      
;  (set-space-objects! ownspace (cons u (space-objects ownspace))))


(define next-enemy-count 0)
(define last-base-con 10000)
(define last-enemy-base-con 10000)

; return a list of changes
(define (on-tick ownspace)
  (define commands '())
  
  (define hb (find-id ownspace (ob-id base)))
  (define eb (find-id ownspace (ob-id destroyer)))
  (when (and hb eb)
    
    (when (time-for (space-time ownspace) 55000 0000)
      (define m (message (next-id) (space-time ownspace) #f "New Fighter at Outpost"))
      (define f (new-blue-fighter))
      (set! commands (append commands (list (chadd f (ob-id (get-hangar base))) m))))
    
    (when (time-for (space-time ownspace) 65000 20000)
      (define f (new-red-fighter))
      (set! commands (append commands (list (chadd f (ob-id (get-hangar destroyer)))))))
    
    (when (time-for (space-time ownspace) 90000 30000)
      (define m (message (next-id) (space-time ownspace) #f "Empire Frigate Incoming"))
      (define x (+ (/ (space-width ownspace) 2) 100))
      (define y (random-between (- (/ (space-height ownspace) 2)) (/ (space-height ownspace) 2)))
      (define fighters (for/list ((i (random 3)))
                         (make-ship "red-fighter" "Empire Fighter" "Empire")))
      (define f (make-ship "red-frigate" "Empire Frigate" "Empire" #:x x #:y y #:r pi
                           #:in-hangar fighters #:cargo (list (random-upgrade ownspace #f)
                                                              (random-upgrade ownspace #f))))
      (set-ship-ai-strategy! f (list (strategy (space-time ownspace) "attack" (ob-id base))))
      (set! commands (append commands (list (chadd f #f) m))))
    
    
    
    (when ((ship-con hb) . < . (- last-base-con 100))
      (define m (message (next-id) (space-time ownspace) #f (format "Outpost Health: ~a" (inexact->exact (round (ship-con hb))))))
      (set! commands (append commands (list m)))
      (set! last-base-con (ship-con hb)))
    
    
    (when (< (ship-con eb) (- last-enemy-base-con 50))
      (define m (message (next-id) (space-time ownspace) #f (format "Empire Destroyer Health: ~a" (inexact->exact (round (ship-con eb))))))
      (set! commands (append commands (list m)))
      (set! last-enemy-base-con (ship-con eb))))
  
  commands)

; return a list of changes
(define (on-destroy ownspace)
  (define commands '())
  (printf "on-destroy\n")
  commands)


(thread (lambda ()
(start-server PORT ownspace on-tick on-destroy)
))

;(thread (lambda () (start-client "127.0.0.1" PORT "Dave" #t #f)))
;(thread (lambda () (start-client "127.0.0.1" PORT "Andrea" #t #f)))

(semaphore-wait (make-semaphore))

