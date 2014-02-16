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

(load-ship "blue-frigate")
(load-ship "blue-fighter")


(define (make-ship type name faction
                   #:posvel? (posvel? #t) #:x (x 0) #:y (y 0) #:r (r 0)
                   #:start-ship? (start-ship? #f)
                   #:npc? (npc? #f)
                   #:npc-helm? (npc-helm? #f)
                   #:npc-weapons? (npc-weapons? #f)
                   #:npc-tactical? (npc-tactical? #f))
  (define s (ship (next-id) 0 (if posvel? (posvel 0 x y r 0 0 0) #f)
                  (stats (next-id) type name faction 10 100)
                  (multipod (next-id) (crewer (next-id) #f #f) #f #f #f #f 0 (not start-ship?) '())
                  '()))
  (set-ship-pods!
   s
   (case type
     (("blue-frigate")
      (list
       (helm (next-id) (pilot (next-id) #f (or npc? npc-helm?) r #f #f) 0 0 #f #f 0)
       (multipod (next-id) (observer (next-id) #f #f) 0 10 #f #f 0 #t '())
       (hangarpod (next-id) (hangar (next-id) #f #f) 0 -10 #f #f 0 #f '() '())
       (weapon (next-id) (weapons (next-id) #f (or npc? npc-weapons?) #f)
               (degrees->radians 21.8) 21.5 0 (* 0.8 pi) 0)
       (tactical (next-id) (tactics (next-id) #f (or npc? npc-tactical?) #f)
                 (degrees->radians -21.8) 21.5 0 (* 0.8 pi) 0)))
     (("blue-fighter")
      (list
       (helm (next-id) (pilot (next-id) #f (or npc? npc-helm?) r #f #f) 0 0 #f #f 0)
       (multipod (next-id) (observer (next-id) #f #f) 0 10 #f #f 0 #t '())
       (weapon (next-id) (weapons (next-id) #f (or npc? npc-weapons?) #f)
               0 6.5 0 (* 0.8 pi) 0)))
     ))
  s)
