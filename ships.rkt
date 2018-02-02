#lang racket/base

(require racket/class
         racket/draw
         mode-lambda/static
         racket/math)

(require "defs.rkt")
(require "utils.rkt")

(provide (all-defined-out))

(struct ship-info (file size) #:mutable #:prefab)

(define ship-list
  (hash "spacesuit" (ship-info "spacesuit" #f)
        "probe" (ship-info "probe" #f)
        "missile" (ship-info "missile" 10.0)
        "cannonball" (ship-info "asteroid_43" 10.0)
        "asteroid_87" (ship-info "asteroid_87" #f)
        "asteroid_43" (ship-info "asteroid_43" #f)
        "blue-station" (ship-info "blue-station" #f)
        "red-station" (ship-info "red-station" #f)
        "blue-frigate" (ship-info "blue-frigate" #f)
        "red-frigate" (ship-info "red-frigate" #f)
        "blue-fighter" (ship-info "blue-fighter" #f)
        "red-fighter" (ship-info "red-fighter" #f)
        "red-destroyer" (ship-info "red-destroyer" #f)
        "blue-cruiser" (ship-info "blue-cruiser" #f)
        
        "missile1" (ship-info "missile1" #f)
        "missile2" (ship-info "missile2" #f)
        "missile3" (ship-info "missile3" #f)
        "probe1" (ship-info "probe1" #f)
        "probe2a" (ship-info "probe2a" #f)
        "probe2b" (ship-info "probe2b" #f)
        "probe3" (ship-info "probe3" #f)
    ))

(define (load-ships sd)
  (for (((name si) (in-hash ship-list)))
    (define bm (read-bitmap (string-append "images/" (ship-info-file si) ".png")
                            'png/alpha))
    (when (not (ship-info-size si))
      (set-ship-info-size! si (max (send bm get-width) (send bm get-height))))
    (add-sprite!/value sd (string->symbol (ship-info-file si)) bm)))


(define (make-spacesuit name ship)
  (define pv (obj-posvel ship))
  (define theta (angle-add (obj-r ship) pi/2))
  (define r (random-between -50 50))
  (make-ship "spacesuit"
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
                   #:ai? (ai? #f)
                   #:con (con #f)
                   #:start-time (start-time 0)
                   #:life (life #f)
                   #:hangar (in-hangar #f)
                   #:cargo (cargo '()))
  (define args (list
                (next-id) start-time (if posvel? (posvel 0 x y r dx dy dr) #f)
                #f  ; must fill the stats later
                '()  ; tools
                '()  ; players
                in-hangar  ; hangar
                cargo
                '()  ; empty dmgfx
                ai?
                '()  ; empty ai-strategy
                ))

  (when in-hangar
    (for ((hangship (in-list in-hangar)))
      (set-obj-posvel! hangship #f)))

  (case type
    (("spacesuit")
     (define s (apply spacesuit args))
     ;type name faction con maxcon radius mass radar drag start?
     (set-ship-stats! s (stats (next-id) type name faction 1.0 1.0 5.0 1.0 200.0 0.4 start-ship?))
     s)
    (("missile")
     (define s (apply missile args))
     ;type name faction con maxcon radius mass radar drag start?
     (set-ship-stats! s (stats (next-id) type name faction 1.0 1.0 5.0 1.0 50.0 0.5 #f))
     (set-ship-tools!
      s (list (tool (next-id) 'engine 100.0 #t '())
              (tool (next-id) 'turnleft 2.0 #f '())
              (tool (next-id) 'turnright 2.0 #f '())
              (tool (next-id) 'steer 2.0 r '())
              (tool (next-id) 'endrc #f life '())))
     s)
    (("cannonball")
     (define s (apply cannonball args))
     ;type name faction con maxcon radius mass radar drag start?
     (set-ship-stats! s (stats (next-id) type name faction life life 5.0 1.0 0.0 0.0 #f))
     (set-ship-tools!
      s (list (tool (next-id) 'endrc #f 0 '())))
     s)
    (("probe")
     (define s (apply probe args))
     ;type name faction con maxcon radius mass radar drag start?
     (set-ship-stats! s (stats (next-id) type name faction 10.0 10.0 7.0 1.0 1000.0 0.4 #f))
     (set-ship-tools!
      s (list (tool (next-id) 'engine 100.0 #t '())
              (tool (next-id) 'turnleft 1.0 #f '())
              (tool (next-id) 'turnright 1.0 #f '())
              (tool (next-id) 'endrc #f life '())))
     s)
    (("asteroid_87")
     (define s (apply spaceship args))
     (set-ship-stats! s (stats (next-id) type name faction 10000.0 10000.0 43.0 10000.0 200.0 0.0 start-ship?))
     s)
    (("asteroid_43")
     (define s (apply spaceship args))
     (set-ship-stats! s (stats (next-id) type name faction 5000.0 5000.0 21.0 5000.0 200.0 0.0 start-ship?))
     s)
    (("blue-station" "red-station")
     (define s (apply spaceship args))
     (define mb 500.0)
     (define mc 500.0)
     (set-ship-stats! s (stats (next-id) type name faction mc mc 26.0 1000.0 200.0 0.4 start-ship?))
     (set-ship-tools!
      s (list (tool (next-id) 'pbolt 5.0 #f '())
              (tool (next-id) 'probe 30.0 #f '())
          ; need missile
          ; need shield
          ))
    #;(set-ship-pods!
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
                   (list (shbolt (next-id) '() 50.0 #t))))))
    s)
    (("blue-frigate" "red-frigate")
     (define s (apply spaceship args))
     (set-ship-stats! s (stats (next-id) type name faction
                               ;con maxcon radius mass radar drag start
                               100.0 100.0 18.0 100.0 300.0 0.4 start-ship?))
     (set-ship-tools!
      s (list (tool (next-id) 'engine 20.0 #f '())
              (tool (next-id) 'steer 0.3 #f '())
              (tool (next-id) 'turnleft 0.3 #f '())
              (tool (next-id) 'turnright 0.3 #f '())
              (tool (next-id) 'pbolt 5.0 #f '())
              (tool (next-id) 'dock #f #t '())
              ))
     s)
    (("blue-fighter" "red-fighter")
     (define s (apply spaceship args))
     (define c (if con con 20.0))
     (set-ship-stats! s (stats (next-id) type name faction c c 6.0 20.0 200.0 0.4 start-ship?))
     (set-ship-tools!
      s (list (tool (next-id) 'engine 50.0 #f '())
              (tool (next-id) 'turnleft 1.5 #f '())
              (tool (next-id) 'turnright 1.5 #f '())
              (tool (next-id) 'pbolt 5.0 #f '())
              (tool (next-id) 'dock #f #t '())
              ))
     s)
    #;(("blue-cruiser" "red-destroyer")
     (define s (apply spaceship args))
     s)
    (else
     ;(error (string-append "Tried to create an unknown ship type " type "\n")))
     (define s (apply spaceship args))
     (set-ship-stats! s (stats (next-id) type name faction 1.0 1.0 50.0 1.0 200.0 0.4 start-ship?))
     (set-ship-tools!
      s (list (tool (next-id) 'engine 100.0 #f '())
              (tool (next-id) 'turnleft 1.0 #f '())
              (tool (next-id) 'turnright 1.0 #f '())))
     s)))
