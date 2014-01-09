#lang racket/base

(require "defs.rkt"
         "utils.rkt"
         "pilot.rkt"
         "weapons.rkt"
         "tactics.rkt"
         "physics.rkt")

(provide (all-defined-out))

(define (update-posvel! space pvu pvutime)
  (define o (find-id space (pvupdate-id pvu)))
  (when o
    (set-obj-posvel! o (pvupdate-pv pvu))
    (while (pvutime . < . (space-time space))
      ;(printf "client ticking forward pvu\n")
      (update-physics! space o (/ TICK 1000.0))
      (set! pvutime (+ pvutime TICK))))
  (when (not o)
    (printf "couldn't find obj id ~a\n" (pvupdate-id pvu))))


; on the server, you could get conflicting commands
; (two players trying to take the same open role)
; return #f if we didn't apply the change (should be reported)
; return a list of the changes that actually got made
; example: incoming change is weapons clicked
;          outgoing change(s) are new plasmas, nobody cares about the click
; ctime is the scenario time of the change or #f
; - needed when the client has moved space forward and is now picking up old changes
(define (apply-change! space c ctime)
  (cond
    ((role-change? c)
     (define p (role-change-player c))
     (define from (find-stack space (role-change-from c)))
     (define to (find-id space (role-change-to c)))
     (cond ((join-role! to p)
            (when from (leave-role! from p))
            (list c))
           (else #f)))
    ((role? c)
     ; find our role
     (define stack (find-stack space (obj-id (role-player c))))
     (cond
       ((weapons? c) (update-weapons c space stack))
       ((tactics? c) (update-tactics c space stack))
       ((pilot? c) (update-pilot c space stack))))
    ((chadd? c)
     ;(printf "client adding ~v\n" (chadd-o c))
     (while (ctime . < . (space-time space))
       ;(printf "client ticking forward ~v\n" (chadd-o c))
       (update-physics! space (chadd-o c) (/ TICK 1000.0))
       (set! ctime (+ ctime TICK)))
     (set-space-objects! space (cons (chadd-o c) (space-objects space))))
    (else
     (error "apply-change! hit ELSE clause ~v" c))))
