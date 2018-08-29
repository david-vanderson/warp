#lang racket/base

(require racket/math
         racket/class
         racket/draw)

(require "../defs.rkt"
         "../utils.rkt"
         "../ships.rkt"
         "../order.rkt"
         "../physics.rkt"
         "../effect.rkt"
         "../upgrade.rkt")

(provide (all-defined-out))

(define (testing-scenario oldspace oldtick oldmessage old-on-player-restart)
  (define players (if oldspace (space-players oldspace) '()))
  (for ((p players)) (set-player-faction! p #f))

  (define ownspace (space (next-id) 0 10000 10000 players '()
                          `(,(standard-quit-scenario-button))))

  (define (new-blue-fighter (x 0) (y 0) (r pi/2) #:price [price #f])
    (make-ship "blue-fighter" "Rebel Fighter" "Rebel"
               #:ai #f #:x x #:y y #:r r #:price price
               #:hull 1000 #:mass 20 #:drag 0.4 #:start-ship? #t
               #:tools (append (tools-pilot 50.0 #f 1.5)
                               (list (tool-missile 5.0 10.0)
                                     (tool-warp 200.0 8.0)
                                     (tool-pbolt 5.0)))))
  
  (define (new-red-fighter (x 0) (y 0))
    (make-ship "red-fighter" "Empire Fighter" "Empire"
               #:ai #f #:x x #:y y
               #:hull 500 #:mass 20 #:drag 0.4
               #:tools (append (tools-pilot 50.0 #f 1.5)
                               (list (tool-missile 5.0 10.0)
                                     (tool-pbolt 5.0)))))

  (define bf (for/list ((i 10)) (new-blue-fighter (* 50 i) 50)))
  (define rf (for/list ((i 50)) (new-red-fighter  (* 50 i) 150)))
  (define a (make-ship "asteroid_87" "a" "Empire" #:x 95 #:y 95 #:dx -0.001))

  (define b1 (make-ship "blue-station" "b1" "Rebel" #:x 0 #:y 0 #:ai #f
                        #:hangar '() #:radar 500
                        #:tools (append (tools-pilot 50.0 #f 1.5)
                                        (list
                                         (tool-pbolt 10.0)
                                         (tool-missile 5.0 10.0)
                                         (tool-mine 25.0)
                                         (tool-factory 100
                                           (list (new-blue-fighter #:price 1)
                                                 (new-blue-fighter #:price 5)
                                                 (new-blue-fighter #:price 75)))))))
  (define b2 (make-ship "blue-station" "b2" "Rebel" #:x 900 #:y 0 #:ai #f
                        #:hangar '()))

  (for ((f bf))
    (set-ship-ai-strategy! f
      (list (strategy (space-time ownspace) "return" (ob-id b1)))))

  (define f (new-blue-fighter 800 0))

  (define r (new region%))
  (send r set-polygon
        '((-500 . -500)
          (-500 . 500)
          (500 . 500)
          (500 . -500)))
  (define-values (rx ry rw rh) (send r get-bounding-box))

  (define nebulas '())
  (define sep 500.0)
  (define sep/2 (/ sep 2))
  (for* ((x (in-range (+ rx sep/2) (- rw sep/2) sep))
         (y (in-range (+ ry sep/2) (- rh sep/2) sep))
         #:when (send r in-region? x y))
    (define xd (random-between (* sep -0.25) (* sep 0.25)))
    (define yd (random-between (* sep -0.25) (* sep 0.25)))
    (define dr (* (if ((random) . < . 0.5) -1 1) (random-between 0.01 0.04)))
    (define t (inexact->exact (round (random-between 0 100000))))
    (define n (nebula (next-id) t #t 1.0 (posvel 0 (+ x xd) (+ y yd) 0.0 0.0 0.0 dr) sep))
    (prepend! nebulas n))

  ;(define n (nebula (next-id) 0 #t (posvel 0 100.0 100.0 0.0 0.0 0.0 0.02) 500.0))
  
  (set-space-objects! ownspace
                      (append 
                       (list #;b1 b2 f
                             (new-red-fighter 300 0)
                             (new-red-fighter 800 200))
                       nebulas
                       (space-objects ownspace)))
  
  (define real-orders (space 0 0 0 0 '() '() '()))  ; only care about orders
  
  ; return a list of changes
  (define (on-tick ownspace qt change-scenario!)
    (define changes '())

    (for ((p (space-players ownspace)))
      (when (not (player-faction p))
        ; new player
        (append! changes (chfaction (ob-id p) "Rebel"))
        (append! changes (chmov (ob-id p) (ob-id f) #f))))

    (for ((fo (space-orders real-orders)))
      (check ownspace (car fo) (cadr fo)))
    
    (append! changes (order-changes ownspace real-orders))

    #;(for ((i 10))
      (define x (* (random) 5000))
      (define y (* (random) 5000))
      (define p (plasma (next-id) (space-time ownspace) #t
                        (posvel (space-time ownspace)
                                x
                                y
                                0
                                0
                                0
                                0)
                        10))
      (append! changes (chadd p #f)))
    
    changes)
  
  (define (on-message space cmd change-scenario!)
    (define o (find-id space space (anncmd-id cmd)))
    (when (and o (ann-button? o))
      (case (ann-button-msg o)
        (("quit-scenario") (change-scenario!))))
    '())
  
  (values ownspace on-tick on-message #f))

