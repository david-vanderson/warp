#lang racket/base

(require racket/class
         racket/draw
         racket/math)

(require "defs.rkt")

(provide (all-defined-out))

(define shipshash (make-hash))

(define (get-ship-bitmap ship)
  (define type (stats-type (ship-stats ship)))
  (hash-ref shipshash type))

(define (load-ship type)
  (define b (make-bitmap 1 1))
  (send b load-file (string-append "images/" type ".png") 'png/alpha)
  (hash-set! shipshash type b))

(load-ship "space-suit")

(load-ship "blue-station")
(load-ship "blue-frigate")
(load-ship "red-frigate")
(load-ship "blue-fighter")
(load-ship "red-fighter")


(define (make-ship type name faction
                   #:posvel? (posvel? #t)
                   #:x (x 0) #:y (y 0) #:r (r pi/2)
                   #:dx (dx 0) #:dy (dy 0) #:dr (dr 0)
                   #:start-ship? (start-ship? #f)
                   #:npc? (npc? #f)
                   #:npc-helm? (npc-helm? #t)
                   #:helm-fore? (helm-fore? #f)
                   #:npc-weapons? (npc-weapons? #t)
                   #:npc-tactical? (npc-tactical? #t)
                   #:in-hangar (in-hangar '()))
  (define s ((if (equal? "space-suit" type) spacesuit spaceship)
             (next-id) 0 (if posvel? (posvel 0 x y r dx dy dr) #f)
             #f
             (multipod (next-id) (crewer (next-id) #f #f) #f #f #f #f 0 start-ship? '())
             '()
             '()))
  
  (for ((hangship in-hangar))
    (set-obj-posvel! hangship #f)
    (define new-strat (strategy 0 "return" (ob-id s)))
    (set-ship-ai-strategy! hangship (list new-strat)))
  
  (case type
    (("space-suit")
     ;(type name faction power bat maxbat con maxcon radius mass thrust rthrust)
     (set-ship-stats! s (stats (next-id) type name faction 0 0 0 1 1 7 1 0 0))
     (set-ship-pods!
      s (list
         (multipod (next-id) (observer (next-id) #f #f) 0 0 #f #f 0 #f '()))))
    (("blue-station")
     (define mb 0)
     (set-ship-stats! s (stats (next-id) type name faction 10 mb mb 1000 1000 26 1000 0 0))
     (set-ship-pods!
      s `(,(multipod (next-id) (observer (next-id) #f #f) 0 0 #f #f 0 #f '())
          ,(hangarpod (next-id) (hangar (next-id) #f #f) pi 13 #f #f 0 #f '() in-hangar)
          ,@(for/list ((d (list 0 90 180 270)))
              (weapon (next-id) (weapons (next-id) #f (and npc? npc-weapons?) #f)
                      (degrees->radians d) 26 (degrees->radians d) (* 0.8 pi) MAX_POD_ENERGY))
          ,@(for/list ((d (list 45 135 225 315)))
              (tactical (next-id) (tactics (next-id) #f (and npc? npc-tactical?) #f)
                        (degrees->radians d) 28 (degrees->radians d) (* 0.8 pi) MAX_POD_ENERGY)))))
    (("blue-frigate" "red-frigate")
     (define mb 100)
     (set-ship-stats! s (stats (next-id) type name faction 10 mb mb 100 100 18 100 20 0.3))
     (set-ship-pods!
      s (list
         (helm (next-id) (pilot (next-id) #f (and npc? npc-helm?) r helm-fore? #f #f) 0 0 #f #f MAX_POD_ENERGY)
         (multipod (next-id) (observer (next-id) #f #f) 0 10 #f #f 0 #f '())
         (hangarpod (next-id) (hangar (next-id) #f #f) pi 10 #f #f 0 #f '() in-hangar)
         (weapon (next-id) (weapons (next-id) #f (and npc? npc-weapons?) #f)
                 (degrees->radians 21.8) 21.5 0 (* 0.8 pi) MAX_POD_ENERGY)
         (tactical (next-id) (tactics (next-id) #f (and npc? npc-tactical?) #f)
                   (degrees->radians -21.8) 21.5 0 (* 0.8 pi) MAX_POD_ENERGY))))
    (("blue-fighter" "red-fighter")
     (define mb 100)
     (set-ship-stats! s (stats (next-id) type name faction 1 mb mb 50 50 6 10 40 1))
     (set-ship-pods!
      s (list
         (helm (next-id) (pilot (next-id) #f (and npc? npc-helm?) r helm-fore? #f #f) 0 0 #f #f MAX_POD_ENERGY)
         (multipod (next-id) (observer (next-id) #f #f) 0 3 #f #f 0 #f '())
         (weapon (next-id) (weapons (next-id) #f (and npc? npc-weapons?) #f)
                 0 6.5 0 (* 0.8 pi) MAX_POD_ENERGY)))))
  s)
