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
    (when (and (or doall? (not (ord-done? ot)))
               ((string-length (ord-text ot)) . > . 0))
      (f ot depth (and highlight (not (ord-done? ot)))))
    (cond
      ((ordertime? ot)
       (loop (ordertime-ot ot) (+ depth 1) highlight)
       (ord-done? ot))
      ((order? ot)
       (ord-done? ot))
      ((equal? 'seq (ordercomb-type ot))
       (for/and ((ot (in-list (ordercomb-orders ot))))
         (define d (loop ot (+ depth 1) highlight))
         (when (not d) (set! highlight #f))
         (or doall? d))
       highlight)
      ((equal? 'and (ordercomb-type ot))
       (for/fold ((ret #t)) ((ot (in-list (ordercomb-orders ot))))
         (define d (loop ot (+ depth 1) highlight))
         (and ret d)))
      (else
       (error "client got an unknown order combinator\n")))))

; replace all order-f with #f so this ordertree can be serialized
(define (scrub ot)
  (cond
    ((ordertime? ot)
     (struct-copy ordertime ot
                  (f #:parent order #f)
                  (ot (scrub (ordertime-ot ot)))))
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
      ((order? ot)
       (if ((order-f ot) space faction ot)
           #t
           #f))
      ((equal? 'seq (ordercomb-type ot))
       (for/and ((ot (in-list (ordercomb-orders ot))))
         (check space faction ot)))
      ((equal? 'and (ordercomb-type ot))
       (for/and ((ot (in-list (ordercomb-orders ot))))
         (check space faction ot)))
      (else
       (error "check: got an unknown ord\n"))))
  (set-ord-done?! ot d)
  d)


; make a waypoint scouting order
(define (scout-waypoint text x y r)
  (define pv (posvel 0 x y 0 0 0 0))
  (define pvobj (obj #f #f pv))
  (order #f text (list (ann-circle (next-id) 0 pv #f text r))
         (lambda (space faction o)
           (for/first ((s (in-list (space-objects space)))
                       #:when (and (ship? s) ((faction-check faction (ship-faction s)) . > . 0)
                                   ((distance pvobj s) . < . r)))
             (set-order-f! o (lambda (s f o) #t))
             #t))))

; kill a particular ship
(define (kill text id)
  (order #f text (list (ann-ship (next-id) 0 #f #f text id))
         (lambda (space faction o)
           (not (find-id space id)))))

; keep alive
(define (alive text id)
  (order #f text (list (ann-ship (next-id) 0 #f #f text id))
         (lambda (space faction o)
           (find-id space id))))

; make a timout order
(define (timeout text start total ot)
  (define (f space faction o)
    (define left (- (ordertime-subtotal o)
                    (- (space-time space) (ordertime-start o))))
    (when (and (left . > . 0)
               (not (ord-done? o))
               (check space faction (ordertime-ot o)))
      ; time left and order completed, mark done
      (set-ord-done?! o #t)
      ; don't lock the order so that you can see the time still tick down
      )
    (when (left . < . 0)
      ; no time left, lock order and stop countdown
      (set-ord-text! o (format (ord-text o) "--:--"))
      (set-order-f! o (lambda (s f o) (ord-done? o))))
    (ord-done? o))
  (ordertime #f text '() f total start ot))

