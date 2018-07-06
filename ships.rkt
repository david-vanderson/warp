#lang racket/base

(require racket/class
         racket/draw
         mode-lambda/static
         racket/math)

(require "defs.rkt")
(require "utils.rkt")

(provide (all-defined-out))

(struct ship-info (filename size bm engine-bms) #:mutable #:prefab)

(define ship-list
  (for/hash ((name '("spacesuit" 
                     "probe"
                     "missile"
                     "cannonball"
                     "asteroid_87"
                     "asteroid_43"
                     "blue-station"
                     "red-station"
                     "blue-frigate"
                     "red-frigate"
                     "blue-fighter"
                     "red-fighter"
                     "red-destroyer"
                     "blue-cruiser"
                     "red-cruiser"
        
                     ;"missile1" (ship-info "missile1" 16.0 #f)
                     ;"missile2" (ship-info "missile2" #f #f)
                     ;"missile3" (ship-info "missile3" #f #f)
                     ;"probe1" (ship-info "probe1" #f #f)
                     ;"probe2a" (ship-info "probe2a" #f #f)
                     ;"probe2b" (ship-info "probe2b" #f #f)
                     ;"probe3" (ship-info "probe3" #f #f)
                     )))
    (define size #f)
    (define filename name)
    (cond
      ((equal? name "blue-fighter")
       (set! size 20.0))
      ((equal? name "missile")
       (set! size 10.0))
      ((equal? name "cannonball")
       (set! filename "asteroid_43")
       (set! size 10.0)))
    (values name (ship-info filename size #f '()))))

(define (engine-filename base i k (ext ".png"))
  (string-append base "-e" (number->string i) (number->string k) ext))

(for (((name si) (in-hash ship-list)))
  (define basename (string-append "images/" (ship-info-filename si)))
  (define bm (read-bitmap (string-append basename ".png") 'png/alpha))
  (set-ship-info-bm! si bm)
  (when (not (ship-info-size si))
    (set-ship-info-size! si (max (send bm get-width) (send bm get-height))))
  (for ((i (in-naturals 1)))
    #:break (not (file-exists? (engine-filename basename i 1)))
    (set-ship-info-engine-bms! si
                               (append (ship-info-engine-bms si) (list (list))))
    (for ((k (in-naturals 1)))
      (define filename (engine-filename basename i k))
      #:break (not (file-exists? filename))
      (define bm (read-bitmap filename 'png/alpha))
      (set-ship-info-engine-bms! si
        (for/list ((bms (ship-info-engine-bms si))
                   (z (in-naturals 1)))
          (if (equal? i z)
              (append bms (list bm))
              bms))))))

(define (load-ships sd)
  (for (((name si) (in-hash ship-list)))
    (define sym (string->symbol name))
    (add-sprite!/value sd sym (ship-info-bm si))
    (set-ship-info-bm! si sym)
    (set-ship-info-engine-bms! si
      (for/list ((engine-bms (ship-info-engine-bms si))
                 (i (in-naturals 1)))
        (for/list ((bm engine-bms)
                   (k (in-naturals 1)))
          (define sym (string->symbol (engine-filename name i k "")))
          (add-sprite!/value sd sym bm)
          sym)))))


(define (make-spacesuit name ship)
  (define ang (random-between -1.0 1.0))
  (define a (+ ang (if ((random) . > . 0.5) pi/2 (- pi/2))))
  (define theta (angle-add (obj-r ship) a))
  (define r (random-between 30.0 50.0))
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
                   #:drag (drag 0.0)
                   #:hp-bar? (hp-bar? #t)
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
                hp-bar?
                size
                (if hit-radius
                    hit-radius
                    (/ (exact->inexact size) 2.0))
                #f  ; must fill the stats later
                '()  ; tools
                '()  ; players
                in-hangar  ; hangar
                '()  ; overlays
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
     (set-ship-hp-bar?! s #f)
     ;type name faction con maxcon mass radar drag start?
     (set-ship-stats! s (stats (next-id) type name faction 1.0 1.0 1.0 150.0 0.5 start-ship?))
     s)
    (("missile")
     (define s (apply missile args))
     (set-ship-hp-bar?! s #f)
     (set-ship-ai-freq! s 500)
     ;type name faction con maxcon mass radar drag start?
     (set-ship-stats! s (stats (next-id) type name faction con con 1.0 radar 0.5 #f))
     (set-ship-tools!
      s (append (tools-pilot 100.0 #t 2.0 #:engine-visible? #f #:dock? #f)
                (list (tool-endrc life))))
     s)
    (("cannonball")
     (define s (apply cannonball args))
     (set-ship-hp-bar?! s #f)
     ;type name faction con maxcon mass radar drag start?
     (set-ship-stats! s (stats (next-id) type name faction con con 1.0 radar 0.0 #f))
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
     (set-ship-stats! s (stats (next-id) type name faction con con 10000.0 200.0 drag start-ship?))
     s)
    (("asteroid_43")
     (define s (apply spaceship args))
     (set-ship-stats! s (stats (next-id) type name faction con con 5000.0 200.0 drag start-ship?))
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
