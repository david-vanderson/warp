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


(define (join-role! space roleid p)
  (define r (find-id space roleid))
  ;(printf "player ~v joining role ~v\n" p r)
  (cond
    ((not r) #t)  ; can always join no-role
    ((multipod? r)
     (define new-role (copy-role (pod-role r)))
     (set-role-player! new-role p)
     (set-multipod-roles! r (cons new-role (multipod-roles r)))
     #t)
    ((and (role? r) (not (role-player r)))
     (set-role-player! r p)
     #t)
    (else #f)))


(define (leave-role! space roleid p)
  (define stack (find-stack space roleid))
  (define role (car stack))  ; will always be a role?
  (define mp (cadr stack))  ; could be multipod?
  ;(printf "player ~v leaving role ~v\n" p r)
  (cond
    ((multipod? mp)
     (define seen #f)  ; always remove the first instance of player
     (set-multipod-roles!
      mp (filter (lambda (xr) (or (not (equal? (ob-id (role-player xr)) (ob-id p)))
                                   (begin0 seen (set! seen #t))))
                  (multipod-roles mp))))
    (else (set-role-player! role #f))))


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
     (cond ((join-role! space (role-change-to c) p)
            (when (role-change-from c) (leave-role! space (role-change-from c) p))
            (list c))
           (else #f)))
    ((role? c)
     ; find our role
     (define stack (find-stack space (ob-id (role-player c))))
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
    ((chmov? c)
     (define o (find-id space (chmov-id c)))
     (define from (find-id space (chmov-from c)))
     (cond
       (from
        (set-hangarpod-ships! from
                              (remove o (hangarpod-ships from)
                                      (lambda (a b) (equal? (ob-id a) (ob-id b))))))
       (else
        (set-space-objects! space (remove o (space-objects space)
                                          (lambda (a b) (equal? (ob-id a) (ob-id b)))))))
     
     (define to (find-id space (chmov-to c)))
     (cond
       (to
        (set-hangarpod-ships! to (cons o (hangarpod-ships to))))
       (else
        (set-space-objects! space (cons o (space-objects space))))))
    (else
     (error "apply-change! hit ELSE clause" c))))
