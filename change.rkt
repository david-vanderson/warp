#lang racket/base

(require racket/list)

(require "defs.rkt"
         "utils.rkt"
         "pilot.rkt"
         "weapons.rkt"
         "tactics.rkt"
         "physics.rkt")

(provide (all-defined-out))

(define (update-posvel! space pvu pvutime)
  (define o (find-top-id space (pvupdate-id pvu)))
  (when o
    (set-obj-posvel! o (pvupdate-pv pvu))
    (while (pvutime . < . (space-time space))
      ;(printf "client ticking forward pvu\n")
      (update-physics! space o (/ TICK 1000.0))
      (set! pvutime (+ pvutime TICK)))))


(define (add-player-to-multipod! p mp newid)
  (define new-role (copy (pod-role mp)))
  (set-ob-id! new-role newid)
  (set-role-player! new-role p)
  (set-multipod-roles! mp (cons new-role (multipod-roles mp))))


(define (join-role! space roleid p test? newid)
  (define r (find-id space roleid))
  ;(printf "player ~v joining role ~v\n" p roleid)
  (cond
    ((not roleid) #t)  ; can always join no-role
    ((not r) #f)  ; tried to join a role that no longer exists
    ((multipod? r)
     ; might have double-clicked, so only join if we're not already there
     (define already-in?
       (findf (lambda (xr) (= (ob-id p) (ob-id (role-player xr))))
              (multipod-roles r)))
     (cond
       (already-in? #f)
       (else
        (when (not test?)
          (add-player-to-multipod! p r newid))
        #t)))
    ((and (role? r) (not (role-player r)))
     (when (not test?)
       (set-role-player! r p))
     #t)
    (else #f)))


(define (leave-role! space roleid p test?)
  ;(printf "player ~v leaving role ~v\n" p roleid)
  (define stack (find-stack space roleid))
  (cond
    ; picking an initial role
    ((not roleid)
     (define existing-player (find-id space (ob-id p)))
     (cond ((not existing-player) #t)
           (else #f)))  ; can't join if already joined
    
    ; tried to leave a role that no longer exists
    ((not stack) #f)
    
    ; player wasn't in the role they're trying to leave
    ((or (not (role-player (car stack)))
         (not (= (ob-id p) (ob-id (role-player (car stack))))))
     #f)
    
    (else
     (define role (car stack))  ; will always be a role?
     (define mp (cadr stack))  ; could be multipod?
     (cond
       ((multipod? mp)
        (when (not test?)
          (set-multipod-roles! mp (remove role (multipod-roles mp))))
        #t)
       (else
        (when (not test?)
          (set-role-player! role #f))
        #t)))))


(define (moveout space o from-id)
  (define from (find-id space from-id))
  (cond
    (from
     (when (hangarpod? from)
       (set-hangarpod-ships! from (remove o (hangarpod-ships from))))
     (when (spaceship? from)
       (set-ship-cargo! from (remove o (ship-cargo from)))))
    (else
     (set-space-objects! space (remove o (space-objects space))))))

(define (movein space o dest-id)
  (define to (find-id space dest-id))
  (cond
    (to
     (when (hangarpod? to)
       (set-hangarpod-ships! to (append (hangarpod-ships to) (list o))))
     (when (spaceship? to)
       (set-ship-cargo! to (append (ship-cargo to) (list o)))))
    (else
     (set-space-objects! space (append (space-objects space) (list o))))))


; make the change in space
; return 2 values:
; first is a boolean of whether we should forward a copy of this change on
; second is a list of additional changes caused by the first change
; - the server should apply them recursively
;
; If the server should forward the incoming change on, copy it to make
; sure that it doesn't get mutated before being sent.  Example is chadd
; a new ship, before that message is passed on the ai changes it.
;
; Each change does one or both of:
; - affect space (so return a copy as first value)
; - produce additional changes (returned as second value)
; 
; the additional changes do NOT include descriptions of the effect made on space
;
; examples:
; 1) change is weapons clicked
;    - no effect
;    - additional changes (the new plasma)
; 2) change is add ship
;    - effect is new ship
;    - no additional changes
; 3) change is damage ship
;    - effect is damage ship
;    - additional changes if the ship explodes
;
; on the server, you could get conflicting commands
; (two players trying to take the same open role)
; (or even the same player clicking a button twice before the server sees it)
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
     (define changes '())
     (define p (role-change-player c))
     (cond
       ((and (join-role! space (role-change-to c) p #t (role-change-newid c))
             (leave-role! space (role-change-from c) p #t))
        
        (when (and (role-change-from c)
                   (spacesuit? (get-ship (find-stack space (role-change-from c)))))
          (when (server?)
            ; leaving a space suit, remove the suit
            (define suitrm (chrm (ob-id (get-ship (find-stack space (role-change-from c))))))
            (set! changes (append changes (list suitrm)))))
        
        (join-role! space (role-change-to c) p #f (role-change-newid c))
        (leave-role! space (role-change-from c) p #f)
        
        (values #t changes))
       (else
        (printf "~a didn't apply role-change ~v\n" who c)
        (values #f '()))))
    ((role? c)
     ; find our role
     (define stack (find-stack space (ob-id c)))
     (cond
       ((not stack)  ; ship died as role message was on wire?
        (printf "~a discarding message (no stack) ~v\n" who c)
        (values #f '()))
       ((weapons? c) (change-weapons c space stack who))
       ((tactics? c) (change-tactics c space stack who))
       ((pilot? c) (change-pilot c space stack who))))
    ((chadd? c)
     ;(printf "~a adding ~v\n" who (chadd-o c))
     (while (and (ctime . < . (space-time space)) (obj-posvel (chadd-o c)))
       ;(printf "~a ticking forward ~v\n" who (chadd-o c))
       (update-physics! space (chadd-o c) (/ TICK 1000.0))
       (set! ctime (+ ctime TICK)))
     (movein space (chadd-o c) (chadd-to c))
     (values #t '()))
    ((chrm? c)
     ;(printf "~a removing ~v\n" who (find-id space (chrm-id c)))
     (set-space-objects! space
                         (filter-not (lambda (o) (equal? (ob-id o) (chrm-id c)))
                                     (space-objects space)))
     (values #t '()))
    ((chdam? c)
     (define o (find-id space (chdam-id c)))
     (cond (o
            (values #t (damage-object! space o (chdam-damage c))))
           (else
            (printf "~a chdam - couldn't find obj id ~a\n" who (chdam-id c))
            (values #t '()))))
    ((chmov? c)
     (define o (find-id space (chmov-id c)))
     (cond (o
            (set-obj-posvel! o (chmov-pv c))
            (moveout space o (chmov-from c))
            (movein space o (chmov-to c))
            (values #t '()))
           (else
            (printf "~a chmov - couldn't find obj id ~a\n" who (chmov-id c))
            (values #t '()))))
    ((cherg? c)
     (define o (find-id space (cherg-id c)))
     (cond (o
            (set-pod-energy! o (+ (pod-energy o) (cherg-e c)))
            (values #t '()))
           (else
            (printf "~a cherg - couldn't find obj id ~a\n" who (cherg-id c))
            (values #t '()))))
    ((new-strat? c)
     (define o (find-id space (new-strat-ship-id c)))
     (cond (o
            (set-ship-ai-strategy! o (new-strat-strats c))
            (values #t '()))
           (else
            (printf "~a new-strat - couldn't find obj id ~a\n" who (new-strat-ship-id c))
            (values #t '()))))
    ((chstats? c)
     (define o (find-id space (chstats-id c)))
     (cond (o
            (set-ship-stats! o (chstats-newstats c))
            ;(printf "ship ~a now has stats ~v\n" (ship-name o) (ship-stats o))
            (values #t '()))
           (else
            (printf "~a chstats - couldn't find obj id ~a\n" who (chstats-id c))
            (values #t '()))))
    ((message? c)
     (set-obj-start-time! c (space-time space))
     (set-space-objects! space (cons c (space-objects space)))
     (values #t '()))
    (else
     (error "apply-change! hit ELSE clause" c))))


(define (apply-all-changes! space changes ctime who)
  (if (null? changes)
      '()
      (apply append
             (for/list ((c (in-list changes)))
               (define-values (forward? new-changes) (apply-change! space c ctime who))
               (append (if forward? (list (copy c)) '())
                       (apply-all-changes! space new-changes ctime who))))))


(define (change-all-ids! structs)
  (for ((s (in-list structs)))
    (when (and (ob? s) (not (player? s)))
      (set-ob-id! s (next-id)))
    (when (struct? s)
      (define fields (rest (vector->list (struct->vector s))))
      (change-all-ids! (flatten fields)))))

(define (change-ids! changes)
  (for ((c (in-list changes)))
    (cond
      ((chadd? c)
       ;(printf "rewriting ~v\n" c)
       (change-all-ids! (list (chadd-o c)))
       ;(printf "to        ~v\n" c)
       )
      ((role-change? c)
       (set-role-change-newid! c (next-id)))
       )))
