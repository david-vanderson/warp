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


; make the change in space, and return a list of additional changes
;
; Each change does one or both of:
; - affect space
; - produce other additional changes
; 
; the additional changes do NOT include descriptions of the effect made on space
;
; examples:
; 1) change is weapons clicked
;    - no effect, additional changes are the new plasma
; 2) change is add ship
;    - effect is new ship, no additional changes
; 3) change is damage ship
;    - effect is damage ship, possible additional changes if the ship explodes
;
; on the server, you could get conflicting commands
; (two players trying to take the same open role)
; return #f if we didn't apply the change (should be reported)
;
; ctime is the scenario time of the change or #f
; - needed when the client has moved space forward and is now picking up old changes
;
; who is a string? for message reporting
;
(define (apply-change! space c ctime who)
  ;(printf "~a applying change ~v\n" who c)
  (cond
    ((role-change? c)
     (define p (role-change-player c))
     (cond ((join-role! space (role-change-to c) p)
            (when (role-change-from c) (leave-role! space (role-change-from c) p))
            '())
           (else (printf "~a didn't apply change ~v\n" who c))))
    ((role? c)
     ; find our role
     (define stack (find-stack space (ob-id c)))
     (cond
       ((not stack)  ; ship died as role message was on wire?
        (printf "~a discarding message ~v\n" who c))
       ((weapons? c) (change-weapons c space stack))
       ((tactics? c) (change-tactics c space stack))
       ((pilot? c) (change-pilot c space stack))))
    ((chadd? c)
     ;(printf "~a adding ~v\n" who (chadd-o c))
     (while (ctime . < . (space-time space))
       ;(printf "~a ticking forward ~v\n" who (chadd-o c))
       (update-physics! space (chadd-o c) (/ TICK 1000.0))
       (set! ctime (+ ctime TICK)))
     (set-space-objects! space (cons (chadd-o c) (space-objects space)))
     '())
    ((chdam? c)
     (define o (find-id space (chdam-id c)))
     (cond (o
            (damage-object! space o (chdam-damage c)))
           (else
            (printf "~a chdam - couldn't find obj id ~a\n" who (chdam-id c))
            '())))
    ((chmov? c)
     (define o (find-id space (chmov-id c)))
     (set-obj-posvel! o (chmov-pv c))
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
        (set-space-objects! space (cons o (space-objects space)))))
     '())
    ((cherg? c)
     (define o (find-id space (cherg-id c)))
     (cond (o
            (set-pod-energy! o (+ (pod-energy o) (cherg-e c)))
            '())
           (else
            (printf "~a cherg - couldn't find obj id ~a\n" who (cherg-id c))
            '())))
    ((new-strat? c)
     (define o (find-id space (new-strat-ship-id c)))
     (cond (o
            (set-ship-ai-strategy! o (new-strat-strat c))
            '())
           (else
            (printf "~a new-strat - couldn't find obj id ~a\n" who (new-strat-ship-id c))
            '())))
    (else
     (error "apply-change! hit ELSE clause" c))))


(define (apply-all-changes! space changes ctime who)
  (if (null? changes)
      '()
      (apply append
             (for/list ((c changes))
               (define new-changes (apply-change! space c ctime who))
               (cons c (apply-all-changes! space new-changes ctime who))))))
