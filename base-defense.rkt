#lang racket/base

(require racket/math)

(require "defs.rkt"
         "utils.rkt"
         "client.rkt"
         "server.rkt"
         "ships.rkt")

(define ai? #t)

(define ownspace (space 0 5000 2000 '()))

(define base (make-ship "blue-station" "a" "a" #:x -2000 #:y -100 #:start-ship? #t))
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
  (make-ship "red-destroyer" "b" "b" #:x 2400 #:y 100 #:r pi #:start-ship? #t))

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

(define (new-blue-fighter mothership)
  (define s (make-ship "blue-fighter" "f" "f"))
  (set-ship-stats! s (stats (next-id) "blue-fighter" "Rebel Fighter" "Rebel" 1.0 100.0 100.0 50.0 50.0 6.0 20.0 50.0 2.0))
  (set-ship-pods!
   s (list
      (helm (next-id) (pilot (next-id) #f #t 0.0 #f #f #f) 0.0 0.0 #f #f 100.0 100.0)
      (multipod (next-id) (observer (next-id) #f #f) 0.0 3.0 #f #f 0.0 0.0 #f '())
      (weapon (next-id) (weapons (next-id) #f #t #f)
              0.0 6.5 0.0 (* 0.1 pi) 50.0 50.0 5.0)))
  (set-obj-posvel! s #f)
  s)

(define (new-red-fighter mothership)
  (define s (make-ship "red-fighter" "f" "f"))
  (set-ship-stats! s (stats (next-id) "red-fighter" "Empire Fighter" "Empire"
                            ;power bat maxbat con maxcon radius mass thrust rthrust
                            1.0 100.0 100.0 50.0 50.0 6.0 20.0 50.0 2.0))
  (set-ship-pods!
   s (list
      (helm (next-id) (pilot (next-id) #f #t 0.0 #f #f #f) 0.0 0.0 #f #f 100.0 100.0)
      (multipod (next-id) (observer (next-id) #f #f) 0.0 3.0 #f #f 0.0 0.0 #f '())
      (weapon (next-id) (weapons (next-id) #f #t #f)
              0.0 6.5 0.0 (* 0.1 pi) 50.0 50.0 5.0)))
  (set-obj-posvel! s #f)
  s)


(define next-enemy-count 0)
(define last-base-con 10000)
(define last-enemy-base-con 10000)

; return a list of changes
(define (on-tick ownspace)
  (define commands '())
  
  (when (time-for (space-time ownspace) 5000);55000 20000)
    (define m (message (next-id) (space-time ownspace) #f "New Fighter at Outpost"))
    (define f (new-blue-fighter base))
    (set! commands (append commands (list (chadd f)
                                          (chmov (ob-id f) #f (ob-id (get-hangar base)) #f)
                                          m))))
  
  (when (time-for (space-time ownspace) 65000 20000)
    (define f (new-red-fighter destroyer))
    (set! commands (append commands (list (chadd f)
                                          (chmov (ob-id f) #f (ob-id (get-hangar destroyer)) #f)))))
  
  (when (time-for (space-time ownspace) 30000);90000 30000)
    (define m (message (next-id) (space-time ownspace) #f "Empire Frigate Incoming"))
    (define x (+ (/ (space-width ownspace) 2) 100))
    (define y (random-between (- (/ (space-height ownspace) 2)) (/ (space-height ownspace) 2)))
    (define fighters (for/list ((i (random 8)))
                       (make-ship "red-fighter" "Empire Fighter" "Empire")))
    (define f (make-ship "red-frigate" "Empire Frigate" "Empire" #:x x #:y y #:r pi #:in-hangar fighters #:start-ship? #t))
    (set-ship-ai-strategy! f (list (strategy (space-time ownspace) "attack" (ob-id base))))
    (set! commands (append commands (list (chadd f) m))))
  
  
  (define hb (find-id ownspace (ob-id base)))
  (when (and hb ((ship-con hb) . < . (- last-base-con 100)))
    (define m (message (next-id) (space-time ownspace) #f (format "Outpost Health: ~a" (inexact->exact (round (ship-con hb))))))
    (set! commands (append commands (list m)))
    (set! last-base-con (ship-con hb)))
  
  (define eb (find-id ownspace (ob-id destroyer)))
  (when (and eb (< (ship-con eb) (- last-enemy-base-con 50)))
    (define m (message (next-id) (space-time ownspace) #f (format "Empire Destroyer Health: ~a" (inexact->exact (round (ship-con eb))))))
    (set! commands (append commands (list m)))
    (set! last-enemy-base-con (ship-con eb)))
  
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

