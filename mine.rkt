#lang racket/base

(require racket/math)

(require "defs.rkt"
         "utils.rkt"
         "quadtree.rkt"
         "ships.rkt")

(provide (all-defined-out))


; return list of changes
(define (reduce-mine! space m damage)
  (define changes '())

  (set-ship-con! m (- (ship-con m) damage))
  
  (when ((ship-con m) . <= . 0)
    (set-obj-alive?! m #f)
    
    ; explode
    (when (server?)
      (define e (make-explosion space (obj-x m) (obj-y m) 20.0 120.0 100.0 200.0))
      (append! changes (chadd e #f))))
  
  changes)


;; client/server

(define (lay-mine! cmd space stack who)
  (define ship (get-ship stack))
  (define tool (ship-tool ship 'mine))
  (cond
    ((not (ship-flying? ship))
     (printf "~a discarding message (not flying) ~v\n" who cmd)
     (values #f '()))
    ((not tool)
     (printf "~a discarding message (no mine tool) ~v\n" who cmd)
     (values #f '()))
    (else
     (define changes '())
     (when (server?)
       (define a (angle-add (obj-r ship) pi))
       (define p (make-ship "mine" "Mine" "_mine"
                            #:hull (tool-val tool)
                            #:radar 75 #:drag 0.6
                            #:tools (tools-pilot 15.0 #f #f)))
       (define d (+ (ship-radius ship) (* 2.0 (ship-radius p))
                    (random-between 0 1)))  ; random so things don't end up exactly aligned
       (set-obj-posvel! p (posvel (space-time space)
                                  (+ (obj-x ship) (* d (cos a)))
                                  (+ (obj-y ship) (* d (sin a)))
                                  a
                                  (obj-dx ship)
                                  (obj-dy ship)
                                  0))
       
       (append! changes (chadd p #f)))
     
     (values #f changes))))

