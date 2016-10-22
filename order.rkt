#lang racket/base

(require "defs.rkt"
         "utils.rkt")

(provide (all-defined-out))

(define (set-space-orders-for! space faction ot)
  (define (pred fo) (not (equal? (car fo) faction)))
  (set-space-orders! space (cons (list faction ot)
                                 (filter pred (space-orders space)))))


; client
(define (for-orders ot doall? f)
  (let loop ((ot ot) (depth 0) (highlight #t))
    (when (or doall?
              (and (order? ot) (not (ord-done? ot))))
      (f ot depth (and highlight (not (ord-done? ot)))))
    (cond
      ((order? ot) (ord-done? ot))
      ((equal? 'seq (ordercomb-type ot))
       (for/and ((ot (in-list (ordercomb-orders ot))))
         (define d (loop ot (+ depth 1) highlight))
         (when (not d) (set! highlight #f))
         (or doall? d))
       highlight)
      (else
       (error "client got an unknown order combinator\n")))))

; replace all order-f with #f so this ordertree can be serialized
(define (scrub ot)
  (cond
    ((order? ot)
     (struct-copy order ot (f #f)))
    (else
     (struct-copy ordercomb ot
                  (orders
                   (for/list ((ot (in-list (ordercomb-orders ot))))
                     (scrub ot)))))))


; run order functions and update ord-done?
; return #t if all orders are done, and #f if not
; can short circuit
(define (check space faction ot)
  (define d
    (cond
      ((order? ot) ((order-f ot) space faction ot))
      ((equal? 'seq (ordercomb-type ot))
       (for/and ((ot (in-list (ordercomb-orders ot))))
         (check space faction ot)))
      (else
       (error "check: got an unknown order combinator\n"))))
  (set-ord-done?! ot d)
  d)


; make a waypoint scouting order
(define (scout-waypoint text x y r)
  (define pv (posvel 0 x y 0 0 0 0))
  (define pvobj (obj #f #f pv))
  (order #f text (list (ann-circle (next-id) 0 pv r text))
         (lambda (space faction o)
           (for/first ((s (in-list (space-objects space)))
                       #:when (and (ship? s) (equal? faction (ship-faction s))
                                   ((distance pvobj s) . < . r)))
             (set-ord-done?! o #t)
             (set-order-f! o (lambda (s f o) #t))
             #t))))
