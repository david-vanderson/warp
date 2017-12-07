#lang racket/base

(require racket/list)

(require "defs.rkt"
         "utils.rkt"
         "ships.rkt"
         "pilot.rkt"
         "pbolt.rkt"
         "warp.rkt"
         "missile.rkt"
         "plasma.rkt"
         "shield.rkt"
         "probe.rkt"
         "order.rkt"
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
       ((warp? o)
        (change-warp! (command-cmd c) space s who))
       ((mtube? o)
        (change-mtube! (command-cmd c) space s who))
       ((ptube? o)
        (change-ptube! (command-cmd c) space s who))
       ((missile? o)
        (set-missile-course! o (command-cmd c))
        (values #t '()))
       ((or (dock? o) (steer? o) (fthrust? o) (shbolt? o))
        (change-pilot-tool! (command-cmd c) space s who))
       ((pod? o) (equal? (command-cmd c) "npc-off")
        (set-pod-npc?! o #f)
        (values #t '()))
       (else
        (error 'apply-change! "~a hit ELSE clause for command ~v\n" who c)))) 
    ((and (chrole? c) (equal? (chrole-to c) "spacesuit"))
     (define changes '())
     (define pid (chrole-playerid c))
     (define oldstack (find-stack space pid))
     (cond
       (oldstack
        (define p (car oldstack))
        (define s (get-ship (reverse oldstack)))
        (when (and s (not (spacesuit? s)))
          (define ss (make-spacesuit (player-name p) s))
          (define sspv (obj-posvel ss))
          ; push spacesuit away from parent ship
          (define t (atan0 (posvel-dy sspv) (posvel-dx sspv)))
          (define r (+ 3 (hit-distance s ss)))
          (set-posvel-x! sspv (+ (posvel-x sspv) (* r (cos t))))
          (set-posvel-y! sspv (+ (posvel-y sspv) (* r (sin t))))
          (append! changes (chadd ss #f) (chrole pid (ob-id (ship-lounge ss))))))
       (else
        (printf "~a dropping chrole ~v\n" who c)))
     (values #f changes))
    ((chrole? c)
     (define changes '())
     (define pid (chrole-playerid c))
     (define oldstack (find-stack space pid))  ; stack before removing player
     (define o (find-id space (chrole-to c)))
     (cond
       ((or (not (chrole-to c))  ; moving to starting screen
            (and (pod? o) (not (pod-player o))))  ; moving to empty pod
        
        ; remove existing player struct
        (when oldstack
          (define pod (get-pod oldstack))
          (cond
            ((lounge? pod)
             (set-lounge-crew! pod (remove-id pid (lounge-crew pod)))
             (when (and (server?) (spacesuit? (get-ship oldstack)))
               ; leaving a space suit, remove the suit
               (append! changes (chrm (ob-id (get-ship oldstack))))))
            ((hangar? pod)
             (set-hangar-crew! pod (remove-id pid (hangar-crew pod))))
            ((pod? pod)
             (set-pod-player! pod #f))
            (else
             (error 'apply-change "~a hit ELSE clause for removing player ~v\n" who c))))
        
        ; put player in new place
        (define p (findfid pid (space-players space)))
        (cond
          ((not p)
           (printf "~a chrole - couldn't find player struct to place ~v\n" who pid))
          ((not (chrole-to c))
           ; moving to starting screen - nothing
           )
          ((lounge? o)
           (set-lounge-crew! o (append (lounge-crew o) (list p))))
          ((hangar? o)
           (set-hangar-crew! o (append (hangar-crew o) (list p))))
          ((pod? o)
           (set-pod-player! o p))
          (else
           (error 'apply-change "~a hit ELSE clause for placing player ~v\n" who c)))
        
        (values #t changes))
       (else
        (printf "~a dropping chrole ~v\n" who c)
        (values #f '()))))
    ((chfaction? c)
     (define pid (chfaction-playerid c))
     (define p (findfid pid (space-players space)))
     (when p (set-player-faction! p (chfaction-newf c)))
     (values #t '()))
    ((chorders? c)
     ;(printf "~a chorders ~v\n" who c)
     (set-space-orders-for! space (chorders-faction c) (chorders-ot c))
     (values #t '()))
    ((chadd? c)
     (define o (chadd-o c))
     ;(printf "~a adding ~v\n" who o)
     (cond
       ((dmg? o)
        (define tool (find-id space (chadd-to c)))
        (cond (tool
               (set-tool-dmgs! tool (cons o (tool-dmgs tool)))
               (values #t '()))
              (else
               (printf "~a chadd - couldn't find tool id ~a\n" who (chadd-to c))
               (values #f '()))))
       ((player? o)
        (define ep (findfid (ob-id o) (space-players space)))
        (cond
          (ep
           (printf "~a chadd - trying to add an existing player ~v\n" who o)
           (values #f '()))
          (else
           (printf "~a adding player ~v\n" who o)
           (set-space-players! space (cons o (space-players space)))
           (values #t '()))))
       (else
        (while (and (ctime . < . (space-time space))
                    (obj-posvel o))  ; some objs have a #f posvel when inside other things
               ;(printf "~a ticking forward ~v\n" who (chadd-o c))
               (update-physics! space o (/ TICK 1000.0))
               (set! ctime (+ ctime TICK)))
        (movein space o (chadd-to c))
        (values #t '()))))
    ((chrm? c)
     (define p (findfid (chrm-id c) (space-players space)))
     (cond
       (p
        (printf "~a removing player ~v\n" who p)
        (set-space-players! space (remove-id (chrm-id c) (space-players space))))
       (else
        ;(printf "~a removing ~v\n" who (find-id space (chrm-id c)))
        (define s (find-stack space (chrm-id c)))
        (cond ((space? (cadr s))
               (set-space-objects! space (remove-id (chrm-id c) (space-objects space))))
              ((ship? (cadr s))
               (set-ship-cargo! (cadr s) (remove-id (chrm-id c) (ship-cargo (cadr s)))))
              ((tool? (cadr s))
               (set-tool-dmgs! (cadr s) (remove-id (chrm-id c) (tool-dmgs (cadr s)))))
              (else
               (printf "~a chrm - hit else clause for obj id ~a\n" who (chrm-id c)))
               )))
     (values #t '()))
    ((chdam? c)
     (define o (find-id space (chdam-id c)))
     (define d (chdam-damage c))
     (cond (o
            (values #t
                    (cond ((plasma? o) (reduce-plasma! space o d) '())
                          ((missile? o) (reduce-missile! space o d))
                          ((shield? o) (reduce-shield! space o d) '())
                          ((ship? o) (reduce-ship! space o d)))))
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
               (append (if forward? (list (copy-prefab c)) '())
                       (apply-all-changes! space new-changes ctime who))))))
