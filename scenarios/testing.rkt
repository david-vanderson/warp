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

  (define ownspace (space (next-id) 0 8000 8000 players '()
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
                                         (tool-probe 10.0)
                                         (tool-factory 100
                                           (list (new-blue-fighter #:price 1)
                                                 (new-blue-fighter #:price 5)
                                                 (new-blue-fighter #:price 75)))))))
  (define b2 (make-ship "blue-station" "b2" "Rebel" #:x 900 #:y 0 #:ai #f
                        #:hangar '()))

  (for ((f bf))
    (set-ship-ai-strategy! f
      (list (strategy (space-time ownspace) "return" (ob-id b1)))))

  (define f (new-blue-fighter 0 0))

  #;(define nebulas (nebula-polygon (cons -1000 -1000)
                                  (cons -1000 1000)
                                  (cons 1000 1000)
                                  (cons 1000 -1000)))

  ;(define n (nebula (next-id) 0 #t (posvel 0 100.0 100.0 0.0 0.0 0.0 0.02) 500.0))
  
  (set-space-objects! ownspace
                      (append 
                       (list b1 b2 ;f
                             (new-red-fighter 300 0)
                             (new-red-fighter 800 200))
                       ;nebulas
                       (space-objects ownspace)))
  
  (define real-orders (space 0 0 0 0 '() '() '()))  ; only care about orders
  
  ; return a list of changes
  (define (on-tick ownspace qt change-scenario!)
    (define changes '())

    (for ((p (space-players ownspace)))
      (when (not (player-faction p))
        ; new player
        (append! changes (chfaction (ob-id p) "Observer"))
        (append! changes (chmov (ob-id p) (ob-id b1) #f))))

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

