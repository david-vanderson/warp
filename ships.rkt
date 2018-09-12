#lang racket/base

(require racket/class
         racket/draw
         racket/string
         mode-lambda/static
         racket/math)

(require "defs.rkt")
(require "utils.rkt")

(provide (all-defined-out))

(struct bm (sym size bitmap) #:prefab)
; size is the size we want the bitmap to be

(struct engine (x y w r) #:prefab)
; describes each engine a ship has (gfx only, no effect on gameplay)
; x,y is pixel offset of front edge of output from center of ship
; w is pixel width of output (scaling)
; r is orientation of output (0 is normal)

(struct ship-info (bm engines) #:prefab)
; engines is a list of engine structs


(define ship-list #f)
(define engine-list #f)
(define ship-list-sema (make-semaphore 1))

(define (engine-name base i k)
  (string-append "engine-" base "-" (number->string i) (number->string k)))

(define (engine-path base i k (ext ".png"))
  (build-path IMAGEDIR (string-append (engine-name base i k) ext)))

; both client and server call this, but we only want it to happen once
; also they could be threads in the same process
(define (load-ships!)
  (semaphore-wait ship-list-sema)
  (when (not ship-list)
    (set! ship-list
          (for/hash ((s (list
                         (list "spacesuit" #f)
                         (list "probe" #f)
                         (list "missile" #f)
                         (list "mine" #f)
                         (list "mine2" #f)  ; frame 2
                         (list "red-cannonball" #f)
                         (list "red-cannonball2" #f)  ; frame 2
                         (list "blue-cannonball" #f)
                         (list "blue-cannonball2" #f)  ; frame 2
                         (list "asteroid" #f)
                         
                         (list "blue-station" #f)
                         ; need blue destroyer
                         (list "blue-cruiser" #f
                               (engine -20 18.5 16 0)
                               (engine -20 -18.5 16 0))
                         (list "blue-frigate" #f
                               (engine -10.5 13.5 16 0)
                               (engine -10.5 -13.5 16 0))
                         (list "blue-fighter" 20
                               (engine -10 0 16 0))
                         
                         (list "red-station" #f)
                         (list "red-destroyer" #f
                               (engine -33 -1 18 0)
                               (engine -29.5 24 13 0)
                               (engine -17.5 -26 10.5 0))
                         (list "red-cruiser" #f
                               (engine -20.5 4.5 8 0)
                               (engine -20.5 -4.5 8 0))
                         (list "red-frigate" #f
                               (engine -13.5 14.5 10 -.4)
                               (engine -13.5 -14.5 10 .4))
                         (list "red-fighter" 24
                               (engine -11.5 0 10 0))
                         
                         (list "green-frigate" #f
                               (engine -14.5 0 9.5 0))
                         (list "green-fighter" 24
                               (engine -16.5 9 8 0)
                               (engine -16.5 -9 8 0))
                         
                         (list "orange-frigate" #f
                               (engine -14.5 0 9.5 0))
                         (list "orange-fighter" 24
                               (engine -16.5 9 8 0)
                               (engine -16.5 -9 8 0))
                         )))
            (define name (car s))
            (define size (cadr s))
            (define sym (string->symbol name))
            (define b (read-bitmap (build-path IMAGEDIR (string-append name ".png"))
                                   'png/alpha))
            (when (not size)
              (set! size (max (send b get-width) (send b get-height))))
            (values name (ship-info (bm sym size b) (cddr s)))))

    (set! engine-list
          (for/hash ((name '("blue-fire" 
                             "red-fire"
                             "rings")))
            (values name
                    (for/list ((i (in-naturals 1)))
                      #:break (not (file-exists? (engine-path name i 1)))
                      ; i goes over different sized engine animations
                      (for/list ((k (in-naturals 1)))
                        ; k goes over the frames of a single animation
                        (define filename (engine-path name i k))
                        #:break (not (file-exists? filename))
                        (define b (read-bitmap filename 'png/alpha))
                        (define sym (string->symbol (engine-name name i k)))
                        (bm sym #f b))))))
    )
     
  (semaphore-post ship-list-sema))

(define (add-ship-sprites! sd)
  (for (((name si) (in-hash ship-list)))
    (define bm (ship-info-bm si))
    (add-sprite!/value sd (bm-sym bm) (bm-bitmap bm)))
  (for (((name lst) (in-hash engine-list)))
    (for* ((o (in-list lst))
           (bm (in-list o)))
      (add-sprite!/value sd (bm-sym bm) (bm-bitmap bm)))))


(define (engine-frame-sym engine-name size frame)
  (define lst (hash-ref engine-list engine-name #f))
  (cond
    ((not lst) #f)
    (else
     (define frames (list-ref lst (- (min size (length lst)) 1)))
     (define bm (list-ref frames (remainder frame (length frames))))
     (bm-sym bm))))
  

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
                   #:size (size (bm-size (ship-info-bm (hash-ref ship-list type))))
                   #:engine-name [engine-name #f]
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
                engine-name
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
                ; for radius take half the visual size
                ; - then subtract a bit for tighter collisions
                (- (/ (exact->inexact size) 2.0) 1.5)
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
      (("red-cannonball" "blue-cannonball") cannonball)
      (("mine") mine)
      (("probe") probe)
      (else spaceship)))
  
  (define s (apply constructor args))

  (when (not engine-name)
    (define newname
      (cond
        ((string-contains? type "blue") "rings")
        ((string-contains? type "red") "red-fire")
        ((string-contains? type "green") "blue-fire")
        ((string-contains? type "orange") "blue-fire")
        (else #f)))
    (set-ship-engine-name! s newname))

  (when (ship-hangar s)
    (for ((hangship (in-list (ship-hangar s))))
      (set-obj-posvel! hangship #f)))

  s)
