#lang racket/base

(require racket/math)

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
  (for ((p players)) (set-player-faction! p "Rebel"))

  (define ownspace (space (next-id) 0 10000 10000 players '()
                          `(,(standard-quit-scenario-button))))
  
  #;(set-space-objects! ownspace
                      (append (space-objects ownspace)
                              (for/list (((name si) (in-hash ship-list))
                                         (x (in-range -1000 1000 100)))
                                (make-ship name name "Rebel" #:x x #:start-ship? #t))))

  (define (new-blue-fighter (x 0) (y 0) (r pi/2))
    (define s (make-ship "blue-fighter" "a" "a" #:ai? #f #:x x #:y y #:r r))
    (set-ship-stats! s (stats (next-id) "blue-fighter" "Rebel Fighter" "Rebel"
                              ;con maxcon mass radar drag start-ship?
                              1000.0 1000.0 20.0 300.0 0.4 #t))
    (set-ship-tools!
     s (append (tools-pilot 50.0 #f 1.5)
               (list ;(tool-missile 5.0 10.0)
                     ;(tool-warp 200.0 8.0)
                     (tool-pbolt 5.0))))
    s)
  
  (define (new-red-fighter (x 0) (y 0))
    (define s (make-ship "red-fighter" "a" "a" #:ai? #t #:x x #:y y))
    (set-ship-stats! s (stats (next-id) "red-fighter" "Empire Fighter" "Empire"
                              ;con maxcon mass radar drag start
                              500.0 500.0 20.0 300.0 0.4 #f))
    (set-ship-tools!
     s (append (tools-pilot 50.0 #f 1.5)
               (list ;(tool-missile 5.0 10.0)
                     (tool-pbolt 5.0))))
    s)

  (define bf (for/list ((i 10)) (new-blue-fighter (* 50 i) 50)))
  (define rf (for/list ((i 50)) (new-red-fighter  (* 50 i) 150)))
  (define a (make-ship "asteroid_87" "a" "Empire" #:x 95 #:y 95 #:dx -0.001))

  (define b1 (make-ship "blue-station" "b1" "Rebel" #:x 0 #:y 0 #:ai? #f #:hangar '()))
  (define b2 (make-ship "blue-station" "b2" "a" #:x 125 #:y 100 #:ai? #f #:hangar '()))

  (for ((f bf))
    (set-ship-ai-strategy! f
      (list (strategy (space-time ownspace) "return" (ob-id b1)))))
  
  (set-space-objects! ownspace
                      (append 
                       (list (new-blue-fighter 100 100)
                             a)
                       (space-objects ownspace)))
  
  (define real-orders (space 0 0 0 0 '() '() '()))  ; only care about orders
  
  ; return a list of changes
  (define (on-tick ownspace qt change-scenario!)
    (define changes '())

    (for ((p (space-players ownspace)))
      (when (not (player-faction p))
        ; new player
        (append! changes (chfaction (ob-id p) "Rebel"))))

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

