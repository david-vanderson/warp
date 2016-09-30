#lang racket/base

(require racket/list)

(require "defs.rkt"
         "utils.rkt"
         "ships.rkt"
         "pilot.rkt"
         "pbolt.rkt"
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


(define (moveout space o from-id)
  (define from (find-id space from-id))
  (cond
    (from
     (when (hangar? from)
       (set-hangar-ships! from (remove o (hangar-ships from))))
     (when (spaceship? from)
       (set-ship-cargo! from (remove o (ship-cargo from)))))
    (else
     (set-space-objects! space (remove o (space-objects space))))))

(define (movein space o dest-id)
  (define to (find-id space dest-id))
  (cond
    (to
     (when (hangar? to)
       (set-hangar-ships! to (append (hangar-ships to) (list o))))
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
    ((command? c)
     (define s (find-stack space (command-id c)))
     (define o (if s (car s) #f))
     (cond
       ((not o)
        (printf "~a dropping command ~v\n" who c)
        (values #f '()))
       ((dmg? o)
        (set-dmg-fixing?! o (command-cmd c))
        (values #t '()))
       ((pbolt? o)
        (change-pbolt! (command-cmd c) space s who))
       ((or (dock? o) (steer? o) (fthrust? o) (shbolt? o))
        (change-pilot-tool! (command-cmd c) space s who))
       ((pod? o) (equal? (command-cmd c) "npc-off")
        (set-pod-npc?! o #f)
        (values #t '()))
       (else
        (error 'apply-change! "~a hit ELSE clause for command ~v\n" who c)))) 
    ((chrole? c)
     (define changes '())
     (define p (chrole-player c))
     (define oldstack (find-stack space (ob-id p)))  ; stack before removing player
     (define o (find-id space (chrole-to c)))
     (cond
       ((or (not (chrole-to c))  ; moving to starting screen
            (equal? (chrole-to c) "spacesuit")  ; jumping ship
            (and (pod? o) (not (pod-player o))))  ; moving to empty pod

        ; remove existing player
        (when oldstack
          (define pod (get-pod oldstack))
          (cond
            ((lounge? pod)
             (set-lounge-crew! pod (remove p (lounge-crew pod)))
             (when (and (server?) (spacesuit? (get-ship oldstack)))
               ; leaving a space suit, remove the suit
               (append! changes (chrm (ob-id (get-ship oldstack))))))
            ((hangar? pod)
             (set-hangar-crew! pod (remove p (hangar-crew pod))))
            ((pod? pod)
             (set-pod-player! pod #f))
            (else
             (error 'apply-change "~a hit ELSE clause for removing player ~v\n" who c))))

        (define forward? #t)
        
        ; put player in new place
        (cond
          ((not (chrole-to c))
           (void))  ; moving to start screen
          ((equal? (chrole-to c) "spacesuit")  ; jumping ship
           (set! forward? #f)  ; don't forward this message
           (when oldstack
             (define s (get-ship (reverse oldstack)))
             (when (not (spacesuit? s))
               (define ss (make-spacesuit (player-name p) s))
               (set-lounge-crew! (ship-lounge ss) (list p))
               (define sspv (obj-posvel ss))
               ; push spacesuit away from parent ship
               (define t (atan0 (posvel-dy sspv) (posvel-dx sspv)))
               (define r (+ 1 (hit-distance s ss)))
               (set-posvel-x! sspv (+ (posvel-x sspv) (* r (cos t))))
               (set-posvel-y! sspv (+ (posvel-y sspv) (* r (sin t))))
               (define rc (chrole p #f))
               (append! changes rc (chadd ss #f)))))
          ((lounge? o)
           (set-lounge-crew! o (append (lounge-crew o) (list p))))
          ((hangar? o)
           (set-hangar-crew! o (append (hangar-crew o) (list p))))
          ((pod? o)
           (set-pod-player! o p))
          (else
           (error 'apply-change "~a hit ELSE clause for placing player ~v\n" who c)))
        
        (values forward? changes))
       (else
        (printf "~a dropping chrole ~v\n" who c)
        (values #f '()))))
    ((chadd? c)
     ;(printf "~a adding ~v\n" who (chadd-o c))
     (cond
       ((dmg? (chadd-o c))
        (define tool (find-id space (chadd-to c)))
        (cond (tool
               (set-tool-dmgs! tool (cons (chadd-o c) (tool-dmgs tool)))
               (values #t '()))
              (else
               (printf "~a chadd - couldn't find tool id ~a\n" who (chadd-to c))
               (values #f '()))))
       (else
        (while (and (ctime . < . (space-time space)) (obj-posvel (chadd-o c)))
               ;(printf "~a ticking forward ~v\n" who (chadd-o c))
               (update-physics! space (chadd-o c) (/ TICK 1000.0))
               (set! ctime (+ ctime TICK)))
        (movein space (chadd-o c) (chadd-to c))
        (values #t '()))))
    ((chrm? c)
     ;(printf "~a removing ~v\n" who (find-id space (chrm-id c)))
     (set-space-objects! space (remove-id (chrm-id c) (space-objects space)))
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
  (when (server?) (change-ids! changes))
  (if (null? changes)
      '()
      (apply append
             (for/list ((c (in-list changes)))
               (define-values (forward? new-changes) (apply-change! space c ctime who))
               (append (if forward? (list (copy c)) '())
                       (apply-all-changes! space new-changes ctime who))))))


(define (change-ids! structs)
  (for ((s (in-list structs)))
    (when (and (ob? s) (not (player? s)))
        ;(printf "rewriting id on ~v\n" s)
        (set-ob-idi! s (next-id)))
    (when (struct? s)
      (define fields (rest (vector->list (struct->vector s))))
      (change-ids! (flatten fields)))))

