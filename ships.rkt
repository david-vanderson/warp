#lang racket/base

(require racket/class
         mode-lambda/static
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

(define (load-ships sd)
  (define shiplist
    '("space-suit"
      "probe"
      "asteroid_87"
      "asteroid_43"
      "blue-station"
      "red-station"
      "blue-frigate"
      "red-frigate"
      "blue-fighter"
      "red-fighter"
      "red-destroyer"
      "blue-cruiser"))
  (for ((name shiplist))
    (add-sprite!/file sd
                      (string->symbol name)
                      (string-append "images/" name ".png")))
  )


(define (normal-lounge)
  (lounge (next-id) "Lounge" #f #f 0.0 0.0 #f #f 0.0 0.0 '() '()))

(define (normal-hangar angle dist in-hangar)
  (hangar (next-id) "Hangar" #f #f angle dist #f #f 0.0 0.0 '() '() in-hangar))


(define (make-spacesuit name ship)
  (define pv (obj-posvel ship))
  (define theta (angle-add (obj-r ship) pi/2))
  (define r (random-between -50 50))
  (make-ship "space-suit"
             name
             (ship-faction ship)
             #:x (posvel-x pv) #:y (posvel-y pv)
             #:dx (+ (posvel-dx pv) (* r (cos theta)))
             #:dy (+ (posvel-dy pv) (* r (sin theta)))))


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
                   #:power (power #f)
                   #:bat (bat #f)
                   #:con (con #f)
                   #:e (e 50.0)
                   #:in-hangar (in-hangar '())
                   #:cargo (cargo '()))
  (define s ((if (equal? "space-suit" type) spacesuit spaceship)
             ;(stats pods hangar crew ai-strategy dmgfx cargo)
             (next-id) 0 (if posvel? (posvel 0 x y r dx dy dr) #f)
             #f  ; must fill the stats later
             '()  ; fill pods later if any
             '()  ; empty ai-strategy
             '()  ; empty dmgfx
             cargo))
  
  (for ((hangship (in-list in-hangar)))
    (set-obj-posvel! hangship #f))
  
  (case type
    (("space-suit")
     ;type name faction power bat maxbat con maxcon radius mass thrust rthrust radar drag start?
     (set-ship-stats! s (stats (next-id) type name faction 0.0 0.0 0.0 1.0 1.0 5.0 1.0 0.0 0.0 200.0 0.4 start-ship?))
     (set-ship-pods! s (list (normal-lounge))))
    (("probe")
     ;type name faction power bat maxbat con maxcon radius mass thrust rthrust radar drag start?
     (set-ship-stats! s (stats (next-id) type name faction 0.0 0.0 0.0 10.0 10.0 7.0 1.0 100.0 1.0 1000.0 0.4 #f))
     (set-ship-pods! s (list (pod (next-id) "P" #f #f 0.0 3.0 #f #f e e
                                  (list (steer (next-id) '() r) (fthrust (next-id) '() #t))))))
    (("asteroid_87")
     (set-ship-pods! s (list (normal-lounge)))
     (set-ship-stats! s (stats (next-id) type name faction 1.0 100.0 100.0 10000.0 10000.0 43.0 10000.0 0.0 0.0 200.0 0.0 start-ship?)))
    (("asteroid_43")
     (set-ship-pods! s (list (normal-lounge)))
     (set-ship-stats! s (stats (next-id) type name faction 1.0 100.0 100.0 5000.0 5000.0 21.0 5000.0 0.0 0.0 200.0 0.0 start-ship?)))
    (("blue-station" "red-station")
     (define mb 500.0)
     (define mc 500.0)
     (set-ship-stats! s (stats (next-id) type name faction 10.0 mb mb mc mc 26.0 1000.0 50.0 0.4 200.0 0.4 start-ship?))
     (set-ship-pods!
      s `(,(normal-lounge)
          ,(normal-hangar pi 13.0 in-hangar)
          ;,(pod (next-id) "Pilot" #f (and npc? npc-helm?) 0.0 13.0 #f #f 100.0 100.0
          ;      (list (steer (next-id) '() r) (fthrust (next-id) '() #f)))
          ,@(for/list ((d (in-list (list 0 90 180 270))))
              (pod (next-id) "W" #f (and npc? npc-weapons?)
                   (degrees->radians d) 26.0 (degrees->radians d) (* 0.8 pi) 100.0 100.0
                   (list (pbolt (next-id) '() 15.0 #t))))
          ,@(for/list ((d (in-list (list 45 135 225 315))))
              (pod (next-id) "S" #f (and npc? npc-tactical?)
                   (degrees->radians d) 28.0 (degrees->radians d) (* 0.8 pi) 100.0 100.0
                   (list (shbolt (next-id) '() 50.0 #t)))))))
    (("blue-frigate" "red-frigate")
     (set-ship-stats! s (stats (next-id) type name faction
                               ;power bat maxbat con maxcon radius mass thrust rthrust radar drag start
                               10.0 50.0 50.0 100.0 100.0 18.0 100.0 20.0 0.3 300.0 0.4 start-ship?))
     (set-ship-pods!
      s `(,(normal-lounge)
          ,(normal-hangar pi 10.0 in-hangar)
          ,(pod (next-id) "Pilot" #f (and npc? npc-helm?) 0.0 3.0 #f #f 100.0 100.0
                (list (steer (next-id) '() r) (fthrust (next-id) '() #f) (dock (next-id) '() #f)))
          ,(pod (next-id) "W" #f (and npc? npc-weapons?)
                (degrees->radians 21.8) 21.5 (/ pi 6) pi/2 100.0 100.0
                (list (pbolt (next-id) '() 5.0 #t)))
          ,(pod (next-id) "W" #f (and npc? npc-weapons?)
                (degrees->radians -21.8) 21.5 (- (/ pi 6)) pi/2 100.0 100.0
                (list (pbolt (next-id) '() 5.0 #t))))))
    (("blue-fighter" "red-fighter")
     (define p (if power power 1.0))
     (define b (if bat bat 100.0))
     (define c (if con con 20.0))
     (set-ship-stats! s (stats (next-id) type name faction p b b c c 6.0 20.0 40.0 1.0 200.0 0.4 start-ship?))
     (set-ship-pods!
      s `(,(normal-lounge)
          ,(pod (next-id) "Pilot" #f (and npc? npc-helm?) 0.0 5.0 0.0 (/ pi 6) 100.0 100.0
                (list (steer (next-id) '() r) (fthrust (next-id) '() #f)
                      (dock (next-id) '() #f) (pbolt (next-id) '() 5.0 #f))))))
    )
  s)
