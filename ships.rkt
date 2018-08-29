#lang racket/base

(require racket/class
         racket/draw
         mode-lambda/static
         racket/math)

(require "defs.rkt")
(require "utils.rkt")

(provide (all-defined-out))

(struct ship-info (filename size bm engine-bms) #:mutable #:prefab)
; each bm is actually (cons sym bm)

(define ship-list #f)
(define ship-list-sema (make-semaphore 1))

(define (engine-filename base i k (ext ".png"))
  (string-append base "-e" (number->string i) (number->string k) ext))

(define (engine-path base i k (ext ".png"))
  (build-path IMAGEDIR (engine-filename base i k ext)))

; both client and server call this, but we only want it to happen once
; also they could be threads in the same process
(define (load-ships!)
  (semaphore-wait ship-list-sema)
  (when (not ship-list)
    (set! ship-list
          (for/hash ((name '("spacesuit" 
                             "probe"
                             "missile"
                             "mine"
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
              ((equal? name "red-fighter")
               (set! size 24.0))
              ((equal? name "missile")
               (set! size 10.0))
              ((equal? name "mine")
               (set! size 16.0))
              ((equal? name "cannonball")
               (set! filename "asteroid_43")
               (set! size 10.0)))
            (values name (ship-info filename size #f '()))))
    
    (for (((name si) (in-hash ship-list)))
      (define sym (string->symbol name))
      (define basename (ship-info-filename si))
      (define bm (read-bitmap (build-path IMAGEDIR (string-append basename ".png")) 'png/alpha))      
      (set-ship-info-bm! si (cons sym bm))
      (when (not (ship-info-size si))
        (set-ship-info-size! si (max (send bm get-width) (send bm get-height))))
      
      (for ((i (in-naturals 1)))
        #:break (not (file-exists? (engine-path basename i 1)))
        ; i goes over different sized engine animations
        (set-ship-info-engine-bms! si (append (ship-info-engine-bms si) (list (list))))
        (for ((k (in-naturals 1)))
          ; k goes over the frames of a single animation
          (define filename (engine-path basename i k))
          #:break (not (file-exists? filename))
          (define bm (read-bitmap filename 'png/alpha))
          (define sym (string->symbol (engine-filename name i k "")))
          (set-ship-info-engine-bms! si
                                     (for/list ((bms (ship-info-engine-bms si))
                                                (z (in-naturals 1)))
                                       (if (equal? i z)
                                           (append bms (list (cons sym bm)))
                                           bms)))))))
  (semaphore-post ship-list-sema))

(define (add-ship-sprites! sd)
  (for (((name si) (in-hash ship-list)))
    (define c (ship-info-bm si))
    (add-sprite!/value sd (car c) (cdr c))
    (for* ((i (in-list (ship-info-engine-bms si)))
           (k (in-list i)))
      (add-sprite!/value sd (car k) (cdr k)))))
  

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
             #:dy (+ (obj-dy ship) (* r (sin theta)))
             #:hull 1 #:drag 0.5 #:mass 1
             #:radar 150 #:visible 150))


(define (make-ship type name faction
                   #:x (x 0.0) #:y (y 0.0) #:r (r pi/2)
                   #:dx (dx 0.0) #:dy (dy 0.0) #:dr (dr 0.0)
                   #:size (size (ship-info-size (hash-ref ship-list type)))
                   #:mass [mass #f]
                   #:drag (drag 0.0)
                   #:price (price #f)
                   #:invincible? (invincible? #f)
                   #:start-ship? (start-ship? #f)
                   #:ai (ai #f)
                   #:hull (hull 1.0)
                   #:radar (radar 300.0)
                   #:visible (visible 200.0)
                   #:hangar (hangar #f)
                   #:cargo (cargo '())
                   #:overlays [overlays '()]
                   #:tools [tools '()]
                   #:ai-strats [ai-strats '()])
  (define args (list
                (next-id) 0 #t 1.0  ; obj-start-time gets set on chadd
                (posvel 0
                        (exact->inexact x)
                        (exact->inexact y)
                        (exact->inexact r)
                        (exact->inexact dx)
                        (exact->inexact dy)
                        (exact->inexact dr))
                type name faction
                (exact->inexact hull)
                (exact->inexact hull)
                (exact->inexact (if mass mass (* size size)))
                (exact->inexact drag)
                start-ship?
                (exact->inexact radar)
                (exact->inexact visible)
                (if price (inexact->exact (round price)) #f)
                invincible?
                (inexact->exact (round size))
                (/ (exact->inexact size) 2.0)
                tools
                '()  ; players
                hangar
                overlays
                cargo
                0.0  ; no dmgfx
                ai
                0    ; ai-time
                1000 ; ai-freq
                ai-strats
                0    ; placeholder ai-strat-time, set to space-time when chadd
                ))

  (define constructor
    (case type
      (("spacesuit") spacesuit)
      (("missile") missile)
      (("cannonball") cannonball)
      (("mine") mine)
      (("probe") probe)
      (else spaceship)))
  
  (define s (apply constructor args))

  (when (ship-hangar s)
    (for ((hangship (in-list (ship-hangar s))))
      (set-obj-posvel! hangship #f)))

  s)