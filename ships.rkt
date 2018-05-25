#lang racket/base

(require racket/class
         racket/draw
         mode-lambda/static
         racket/math)

(require "defs.rkt")
(require "utils.rkt")

(provide (all-defined-out))

(struct ship-info (file size bm) #:mutable #:prefab)

(define ship-list
  (hash "spacesuit" (ship-info "spacesuit" #f #f)
        "probe" (ship-info "probe" #f #f)
        "missile" (ship-info "missile" 10.0 #f)
        "cannonball" (ship-info "asteroid_43" 10.0 #f)
        "asteroid_87" (ship-info "asteroid_87" #f #f)
        "asteroid_43" (ship-info "asteroid_43" #f #f)
        "blue-station" (ship-info "blue-station" #f #f)
        "red-station" (ship-info "red-station" #f #f)
        "blue-frigate" (ship-info "blue-frigate" #f #f)
        "red-frigate" (ship-info "red-frigate" #f #f)
        "blue-fighter" (ship-info "blue-fighter" #f #f)
        "red-fighter" (ship-info "red-fighter" #f #f)
        "red-destroyer" (ship-info "red-destroyer" #f #f)
        "blue-cruiser" (ship-info "blue-cruiser" #f #f)
        
        ;"missile1" (ship-info "missile1" 16.0 #f)
        ;"missile2" (ship-info "missile2" #f #f)
        ;"missile3" (ship-info "missile3" #f #f)
        ;"probe1" (ship-info "probe1" #f #f)
        ;"probe2a" (ship-info "probe2a" #f #f)
        ;"probe2b" (ship-info "probe2b" #f #f)
        ;"probe3" (ship-info "probe3" #f #f)
    ))


(for (((name si) (in-hash ship-list)))
  (define bm (read-bitmap (string-append "images/" (ship-info-file si) ".png")
                          'png/alpha))
  (set-ship-info-bm! si bm)
  (when (not (ship-info-size si))
    (set-ship-info-size! si (max (send bm get-width) (send bm get-height)))))

(define (load-ships sd)
  (for (((name si) (in-hash ship-list)))
    (add-sprite!/value sd (string->symbol (ship-info-file si)) (ship-info-bm si))))


(define (make-spacesuit name ship)
  (define a (if ((random) . > . 0.5) pi/2 (- pi/2)))
  (define theta (angle-add (obj-r ship) a))
  (define r 40.0)
  (make-ship "spacesuit"
             name
             (ship-faction ship)
             #:x (obj-x ship) #:y (obj-y ship)
             #:dx (+ (obj-dx ship) (* r (cos theta)))
             #:dy (+ (obj-dy ship) (* r (sin theta)))))


(define (make-ship type name faction
                   #:posvel? (posvel? #t)
                   #:x (x 0.0) #:y (y 0.0) #:r (r pi/2)
                   #:dx (dx 0.0) #:dy (dy 0.0) #:dr (dr 0.0)
                   #:size (size (ship-info-size (hash-ref ship-list type)))
                   #:hit-radius (hit-radius #f)
                   #:drag (drag #f)
                   #:start-ship? (start-ship? #f)
                   #:ai? (ai? #f)
                   #:con (con 1.0)
                   #:start-time (start-time 0)
                   #:life (life 1.0)
                   #:radar (radar 100.0)
                   #:hangar (in-hangar #f)
                   #:cargo (cargo '()))
  (define args (list
                (next-id) start-time #t
                (if posvel?
                    (posvel 0
                            (exact->inexact x)
                            (exact->inexact y)
                            (exact->inexact r)
                            (exact->inexact dx)
                            (exact->inexact dy)
                            (exact->inexact dr))
                    #f)
                size
                (if hit-radius
                    hit-radius
                    (/ (exact->inexact size) 2.0))
                #f  ; must fill the stats later
                '()  ; tools
                '()  ; players
                in-hangar  ; hangar
                cargo
                0.0  ; no dmgfx
                ai?
                1000 ; ai-freq
                '()  ; empty ai-strategy
                0    ; placeholder ai-strat-time
                ))

  (when in-hangar
    (for ((hangship (in-list in-hangar)))
      (set-obj-posvel! hangship #f)))

  (case type
    (("spacesuit")
     (define s (apply spacesuit args))
     ;type name faction con maxcon mass radar drag start?
     (set-ship-stats! s (stats (next-id) type name faction 1.0 1.0 1.0 200.0 0.5 start-ship?))
     s)
    (("missile")
     (define s (apply missile args))
     (set-ship-ai-freq! s 500)
     ;type name faction con maxcon mass radar drag start?
     (set-ship-stats! s (stats (next-id) type name faction con con 1.0 radar 0.5 #f))
     (set-ship-tools!
      s (append (tools-pilot 100.0 #t 2.0 #:engine-visible? #f #:dock? #f)
                (list (tool-endrc life))))
     s)
    (("cannonball")
     (define s (apply cannonball args))
     ;type name faction con maxcon mass radar drag start?
     (set-ship-stats! s (stats (next-id) type name faction life life 1.0 radar 0.0 #f))
     (set-ship-tools!
      s (list (tool-endrc 0.0)))
     s)
    (("probe")
     (define s (apply probe args))
     ;type name faction con maxcon mass radar drag start?
     (set-ship-stats! s (stats (next-id) type name faction 10.0 10.0 1.0 1000.0 0.4 #f))
     (set-ship-tools!
      s (append (tools-pilot 100.0 #t 1.0 #:engine-visible? #f #:dock? #f)
                (list (tool-endrc life))))
     s)
    (("asteroid_87")
     (define s (apply spaceship args))
     (set-ship-stats! s (stats (next-id) type name faction 10000.0 10000.0 10000.0 200.0 drag start-ship?))
     s)
    (("asteroid_43")
     (define s (apply spaceship args))
     (set-ship-stats! s (stats (next-id) type name faction 5000.0 5000.0 5000.0 200.0 drag start-ship?))
     s)
    (("blue-station" "red-station")
     (define s (apply spaceship args))
     (define mb 500.0)
     (define mc 500.0)
     (set-ship-stats! s (stats (next-id) type name faction mc mc 10000.0 200.0 0.5 start-ship?))
     (set-ship-tools!
      s (list ;(tool-pbolt 10.0)
              (tool-probe 30.0)
          ; need missile
          ; need shield
          ))
    s)
    (("blue-frigate" "red-frigate")
     (define s (apply spaceship args))
     (set-ship-stats! s (stats (next-id) type name faction
                               ;con maxcon mass radar drag start
                               200.0 200.0 100.0 300.0 0.5 start-ship?))
     (set-ship-tools!
      s (append (tools-pilot 20.0 #f 0.3)
                (list (tool-pbolt 10.0))))
     s)
    (("blue-fighter" "red-fighter")
     (define s (apply spaceship args))
     (define c (if con con 20.0))
     (set-ship-stats! s (stats (next-id) type name faction c c 20.0 300.0 0.5 start-ship?))
     (set-ship-tools!
      s (append (tools-pilot 50.0 #f 1.5)
                (list (tool-pbolt 8.0))))
     s)
    (else
     ;(error (string-append "Tried to create an unknown ship type " type "\n")))
     (define s (apply spaceship args))
     (set-ship-stats! s (stats (next-id) type name faction 1.0 1.0 1.0 200.0 0.5 start-ship?))
     (set-ship-tools!
      s (tools-pilot 100.0 #f 1.0))
     s)))
