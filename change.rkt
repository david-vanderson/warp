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
    (printf "pvu - couldn't find obj id ~a\n" (pvupdate-id pvu))))


(define (copy-role r)
  (cond
    ((observer? r) (struct-copy observer r))
    ((hangar? r) (struct-copy hangar r))
    ((crewer? r) (struct-copy crewer r))
    (else (error "copy-role hit ELSE clause, role:\n" r))))


(define (join-role! r p)
  ;(printf "player ~v joining role ~v\n" p r)
  (cond
    ((and (role? r) (not (role-player r)))
     (set-role-player! r p)
     #t)
    ((multirole? r)
     (define new-role (copy-role (multirole-role r)))
     (set-role-player! new-role p)
     (set-multirole-roles!
      r (cons new-role (multirole-roles r)))
     #t)
    ((not r)
     #t)  ; can always join no-role
    (else #f)))


(define (leave-role! stack p)
  (define r (car stack))  ; will always be a role?
  (define con (cadr stack))  ; could be multirole?
  ;(printf "player ~v leaving role ~v\n" p r)
  (cond
    ((multirole? con)
     (define seen #f)  ; always remove the first instance of player
     (set-multirole-roles!
      con (filter (lambda (xr) (or (not (equal? (role-player xr) p))
                                   (begin0 seen (set! seen #t))))
                  (multirole-roles con))))
    (else (set-role-player! r #f))))


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
    ((chdam? c)
     (define o (find-id space (chdam-id c)))
     (when o
       (damage-object! space o (chdam-damage c)))
     (when (not o)
       (printf "chdam - couldn't find obj id ~a\n" (chdam-id c))))
    (else
     (error "apply-change! hit ELSE clause ~v" c))))
