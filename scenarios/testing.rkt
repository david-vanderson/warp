#lang racket/base

(require racket/math)

(require "../defs.rkt"
         "../utils.rkt"
         "../ships.rkt"
         "../order.rkt"
         "../upgrade.rkt")

(provide (all-defined-out))

(define (testing-scenario oldspace oldtick oldmessage)
  (define ai? #t)
  (define players (if oldspace (space-players oldspace) '()))
  (for ((p players)) (set-player-faction! p "Rebel"))

  (define ownspace (space 0 2000 2000 players '()
                          `(,(standard-quit-scenario-tab-button))))
  
  (define (new-blue-fighter)
    (define s (make-ship "blue-fighter" "a" "a" #:ai? #t))
    (set-ship-stats! s (stats (next-id) "blue-fighter" "Rebel Fighter" "Rebel"
                              ;con maxcon radius mass radar drag start-ship?
                              5.0 50.0 6.0 20.0 300.0 0.4 #f))
    (set-ship-tools!
     s (list (tool (next-id) 'engine 50.0 #f '())
             (tool (next-id) 'turnleft 1.5 #f '())
             (tool (next-id) 'turnright 1.5 #f '())
             (tool (next-id) 'steer 1.5 (obj-r s) '())
             (tool (next-id) 'pbolt 5.0 #f '())
             (tool (next-id) 'dock #f #f '())
             ))
    (set-obj-posvel! s #f)
    s)
  
  (define (new-red-fighter)
    (define s (make-ship "red-fighter" "a" "a" #:ai? #t))
    (set-ship-stats! s (stats (next-id) "red-fighter" "Empire Fighter" "Empire"
                              ;con maxcon radius mass radar drag start
                              20.0 20.0 6.0 20.0 300.0 0.4 #f))
    (set-ship-tools!
     s (list (tool (next-id) 'engine 50.0 #f '())
             (tool (next-id) 'turnleft 1.5 #f '())
             (tool (next-id) 'turnright 1.5 #f '())
             (tool (next-id) 'pbolt 5.0 #f '())
             (tool (next-id) 'dock #f #t '())
             ))
    (set-obj-posvel! s #f)
    s)
  
  
  (define cruiser (make-ship "blue-cruiser" "z" "z" #:x 0 #:y 0 #:ai? #t
                             #:hangar (list (new-blue-fighter))))
  (set-ship-stats! cruiser (stats (next-id) "blue-cruiser" "Rebel Cruiser" "Rebel"
                                  ;con maxcon radius mass radar drag start?
                                  150.0 150.0 15.0 100.0 500.0 0.4 #t))
  (set-ship-tools!
   cruiser (list (tool (next-id) 'engine 30.0 #f '())
                 (tool (next-id) 'steer 1.0 #f '())
                 (tool (next-id) 'turnleft 1.0 #f '())
                 (tool (next-id) 'turnright 1.0 #f '())
                 (tool (next-id) 'pbolt 5.0 #f '())
                 (tool (next-id) 'dock #f #t '())
                 (tool (next-id) 'probe 10.0 #f '())
                 (tool (next-id) 'missile 5.0 #f '())
                 (tool (next-id) 'warp '(150.0 100.0 0.0) #f '())
                 ))
  
  (define base (make-ship "blue-station" "a" "a" #:x -1000 #:y 0  #:ai? #t #:hangar '()))
  (set-ship-stats! base (stats (next-id) "blue-station" "Rebel Outpost" "Rebel"
                               ;con maxcon radius mass radar drag start-ship?
                               1000.0 1000.0 26.0 1000.0 10000.0 0.4 #t))
  (set-ship-tools!
   base (list (tool (next-id) 'pbolt 5.0 #f '())
              (tool (next-id) 'probe 30.0 #f '())
          ; need missile
          ))
  
  
  (define destroyer (make-ship "red-destroyer" "b" "b" #:x 100 #:y 0 #:r pi #:ai? #t
                               #:hangar '()))
  (set-ship-stats! destroyer (stats (next-id)
                                    ;type name faction
                                    "red-destroyer" "Empire Destroyer" "Empire"
                                    ;con maxcon radius mass radar drag start?
                                    1000.0 1000.0 23.0 500.0 1000.0 0.4 #f))
  (set-ship-tools!
   destroyer `(,(tool (next-id) 'engine 6.0 #f '())
               ,(tool (next-id) 'turnleft 0.1 #f '())
               ,(tool (next-id) 'turnright 0.1 #f '())
               ,(tool (next-id) 'steer 0.1 pi '())
               ,(tool (next-id) 'pbolt 5.0 #f '())
               ,(tool (next-id) 'missile 5.0 #f '())
               ))
  
  (set-ship-ai-strategy! destroyer
                         (list (strategy (space-time ownspace) "attack-only" (ob-id base))))

  (define f (new-blue-fighter))
  (set-obj-posvel! f (posvel 0 100 100 pi/2 0 0 0))
    
  (set-space-objects! ownspace (append (space-objects ownspace)
                                       (list cruiser #;base destroyer #;f)))
  
  (define real-orders (space 0 0 0 '() '() '()))  ; only care about orders
  
  ; return a list of changes
  (define (on-tick ownspace change-scenario!)
    (define changes '())

    (for ((p (space-players ownspace)))
      (when (not (player-faction p))
        ; new player
        (append! changes (chfaction (ob-id p) "Rebel"))))

    (for ((fo (space-orders real-orders)))
      (check ownspace (car fo) (cadr fo)))
    
    (append! changes (order-changes ownspace real-orders))
    
    changes)
  
  (define (on-message space cmd change-scenario!)
    (define o (find-id space (command-id cmd)))
    (when (and o (ann-button? o))
      (case (ann-button-msg o)
        (("quit-scenario") (change-scenario!))))
    '())
  
  (values ownspace on-tick on-message))


