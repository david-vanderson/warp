#lang racket/base

(require racket/class
         racket/draw
         racket/math)

(require "defs.rkt")

(provide (all-defined-out))

(define ships (make-hash))

(define (get-ship-bitmap ship)
  (define type (stats-type (ship-stats ship)))
  (hash-ref ships type))

(define (load-ship type)
  (define b (make-bitmap 1 1))
  (send b load-file (string-append "images/" type ".png") 'png/alpha)
  (hash-set! ships type b))

(load-ship "space-suit")

(load-ship "blue-frigate")
(load-ship "red-frigate")
(load-ship "blue-fighter")
(load-ship "red-fighter")


(define (make-ship type name faction
                   #:posvel? (posvel? #t)
                   #:x (x 0) #:y (y 0) #:r (r 0)
                   #:dx (dx 0) #:dy (dy 0) #:dr (dr 0)
                   #:start-ship? (start-ship? #f)
                   #:npc? (npc? #f)
                   #:npc-helm? (npc-helm? #t)
                   #:helm-fore? (helm-fore? #f)
                   #:npc-weapons? (npc-weapons? #t)
                   #:npc-tactical? (npc-tactical? #t)
                   #:in-hangar (in-hangar '()))
  (define s (ship (next-id) 0 (if posvel? (posvel 0 x y r dx dy dr) #f)
                  #f
                  (multipod (next-id) (crewer (next-id) #f #f) #f #f #f #f 0 start-ship? '())
                  '()
                  '()))
  
  (case type
    (("space-suit")
     (set-ship-stats! s (stats (next-id) type name faction 0 0 7 1 0 0))
     (set-ship-pods!
      s (list
         (multipod (next-id) (observer (next-id) #f #f) 0 0 #f #f 0 #f '()))))
    (("blue-frigate" "red-frigate")
     (set-ship-stats! s (stats (next-id) type name faction 10 100 18 100 20 0.3))
     (set-ship-pods!
      s (list
         (helm (next-id) (pilot (next-id) #f (and npc? npc-helm?) r helm-fore? #f #f) 0 0 #f #f 100)
         (multipod (next-id) (observer (next-id) #f #f) 0 10 #f #f 0 #f '())
         (hangarpod (next-id) (hangar (next-id) #f #f) 0 -10 #f #f 0 #f '() in-hangar)
         (weapon (next-id) (weapons (next-id) #f (and npc? npc-weapons?) #f)
                 (degrees->radians 21.8) 21.5 0 (* 0.8 pi) 0)
         (tactical (next-id) (tactics (next-id) #f (and npc? npc-tactical?) #f)
                   (degrees->radians -21.8) 21.5 0 (* 0.8 pi) 0))))
    (("blue-fighter" "red-fighter")
     (set-ship-stats! s (stats (next-id) type name faction 10 100 6 10 40 1))
     (set-ship-pods!
      s (list
         (helm (next-id) (pilot (next-id) #f (and npc? npc-helm?) r helm-fore? #f #f) 0 0 #f #f 0)
         (multipod (next-id) (observer (next-id) #f #f) 0 3 #f #f 0 #f '())
         (weapon (next-id) (weapons (next-id) #f (and npc? npc-weapons?) #f)
                 0 6.5 0 (* 0.8 pi) 0)))))
  s)
