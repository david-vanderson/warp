#lang racket/base

(require "defs.rkt"
         "utils.rkt")

(provide (all-defined-out))

(define (set-space-orders-for! space faction ot)
  (define (pred fo) (not (equal? (car fo) faction)))
  (define otherorders (filter pred (space-orders space)))
  (if ot
      (set-space-orders! space (cons (list faction ot) otherorders))
      (set-space-orders! space otherorders)))

(define (get-space-orders-for space faction)
  (define fo (findf (lambda (fo) (equal? faction (car fo))) (space-orders space)))
  (if fo (cadr fo) #f))


; return a list of chorders
(define (order-changes space order-space)
  (define changes '())
  ; for all orders in order-space, check that the matching faction's orders are the same
  (for ((fo (space-orders order-space)))
    (define fac (car fo))
    (define new (scrub (cadr fo)))
    (define old (get-space-orders-for space fac))
    (when (not (equal? new old))
      (printf "not equal? new old\nnew: ~v\nold: ~v\n" new old)
      (append! changes (chorders fac new))))
  
  ; clean up any leftover old orders
  (for ((fo (space-orders space)))
    (define fac (car fo))
    (when (not (get-space-orders-for order-space fac))
      (append! changes (chorders fac #f))))
  changes)


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
