#lang racket/base

(require racket/class
         racket/draw
         racket/math)

(require "defs.rkt")
(require "utils.rkt")

(provide (all-defined-out))

(define shipshash (make-hash))

(define (get-ship-bitmap ship)
  (define type (stats-type (ship-stats ship)))
  (hash-ref shipshash type))

(define (load-ship type)
  (hash-set! shipshash type (load-bitmap type)))

(define (load-ships)
  (load-ship "space-suit")
  (load-ship "blue-station")
  (load-ship "red-station")
  (load-ship "blue-frigate")
  (load-ship "red-frigate")
  (load-ship "blue-fighter")
  (load-ship "red-fighter")
  (load-ship "red-destroyer")
  (load-ship "blue-cruiser")
  )


(define (normal-lounge)
  (lounge -1 "Lounge" #f #f 0.0 0.0 #f #f 0.0 0.0 '() '()))

(define (normal-hangar angle dist in-hangar)
  (hangar -1 "Hangar" #f #f angle dist #f #f 0.0 0.0 '() '() in-hangar))


(define (make-spacesuit name ship)
  (define pv (obj-posvel ship))
  (make-ship "space-suit"
             name
             (ship-faction ship)
             #:x (posvel-x pv) #:y (posvel-y pv)
             #:dx (+ (posvel-dx pv) (random-between -50 50))
             #:dy (+ (posvel-dy pv) (random-between -50 50))))


(define (make-ship type name faction
                   #:posvel? (posvel? #t)
                   #:x (x 0.0) #:y (y 0.0) #:r (r pi/2)
                   #:dx (dx 0.0) #:dy (dy 0.0) #:dr (dr 0.0)
                   #:start-ship? (start-ship? #f)
                   #:npc? (npc? #t)
                   #:npc-helm? (npc-helm? #t)
                   #:helm-fore? (helm-fore? #f)
                   #:npc-weapons? (npc-weapons? #t)
                   #:npc-tactical? (npc-tactical? #t)
                   #:in-hangar (in-hangar '())
                   #:cargo (cargo '()))
  (define s ((if (equal? "space-suit" type) spacesuit spaceship)
             ;(stats pods hangar crew ai-strategy dmgfx cargo)
             -1 0 (if posvel? (posvel 0 x y r dx dy dr) #f)
             #f  ; fill the stats later
             #f  ; fill pods later
             '()  ; empty ai-strategy
             '()  ; empty dmgfx
             cargo))
  
  (for ((hangship (in-list in-hangar)))
    (set-obj-posvel! hangship #f))
  
  (case type
    (("space-suit")
     ;type name faction power bat maxbat con maxcon radius mass thrust rthrust radar start?
     (set-ship-stats! s (stats -1 type name faction 0.0 0.0 0.0 1.0 1.0 5.0 1.0 0.0 0.0 200.0 start-ship?))
     (set-ship-pods! s (list (lounge -1 "Lounge" #f #f 0.0 0.0 #f #f 0.0 0.0 '() '()))))
    (("blue-station" "red-station")
     (define mb 500.0)
     (define mc 500.0)
     (set-ship-stats! s (stats -1 type name faction 10.0 mb mb mc mc 26.0 1000.0 50.0 0.4 200.0 start-ship?))
     (set-ship-pods!
      s `(,(normal-lounge)
          ,(normal-hangar pi 13.0 in-hangar)
          ;,(pod -1 "Pilot" #f (and npc? npc-helm?) 0.0 13.0 #f #f 100.0 100.0
          ;      (list (steer -1 '() r) (fthrust -1 '() #f)))
          ,@(for/list ((d (in-list (list 0 90 180 270))))
              (pod -1 "W" #f (and npc? npc-weapons?)
                   (degrees->radians d) 26.0 (degrees->radians d) (* 0.8 pi) 100.0 100.0
                   (list (pbolt -1 '() 15.0))))
          ,@(for/list ((d (in-list (list 45 135 225 315))))
              (pod -1 "T" #f (and npc? npc-tactical?)
                   (degrees->radians d) 28.0 (degrees->radians d) (* 0.8 pi) 100.0 100.0
                   (list (shbolt -1 '() 50.0)))))))
    (("blue-frigate" "red-frigate")
     (set-ship-stats! s (stats -1 type name faction
                               ;power bat maxbat con maxcon radius mass thrust rthrust
                               5.0 50.0 50.0 100.0 100.0 18.0 100.0 20.0 0.3 250.0 start-ship?))
     (set-ship-pods!
      s `(,(normal-lounge)
          ,(normal-hangar pi 10.0 in-hangar)
          ,(pod -1 "Pilot" #f (and npc? npc-helm?) 0.0 3.0 #f #f 100.0 100.0
                (list (steer -1 '() r) (fthrust -1 '() #f) (dock -1 '() #f)))
          ,(pod -1 "W" #f (and npc? npc-weapons?)
                (degrees->radians 21.8) 21.5 (/ pi 6) pi/2 100.0 100.0
                (list (pbolt -1 '() 10.0)))
          ,(pod -1 "W" #f (and npc? npc-weapons?)
                (degrees->radians -21.8) 21.5 (- (/ pi 6)) pi/2 100.0 100.0
                (list (pbolt -1 '() 10.0))))))
    (("blue-fighter" "red-fighter")
     (define mb 100.0)
     (set-ship-stats! s (stats -1 type name faction 1.0 mb mb 50.0 50.0 6.0 20.0 40.0 1.0 300.0 start-ship?))
     (set-ship-pods!
      s `(,(normal-lounge)
          ,(pod -1 "Pilot" #f (and npc? npc-helm?) 0.0 5.0 0.0 (/ pi 6) 100.0 100.0
                (list (steer -1 '() r) (fthrust -1 '() #f)
                      (dock -1 '() #f) (pbolt -1 '() 5.0))))))
    )
  s)
