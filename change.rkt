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

(define (adj! o fto who rem?)
  (define-values (ss! ss)
    (cond ((ship? fto)
           (cond ((player? o) (values set-ship-players! ship-players))
                 ((upgrade? o) (values set-ship-cargo! ship-cargo))
                 ((dmgfx? o) (values set-ship-dmgfx! ship-dmgfx))
                 (else (values set-ship-hangar! ship-hangar))))
          ((space? fto)
           (cond ((player? o) (values set-space-players! space-players))
                 (else (values set-space-objects! space-objects))))
          ((tool? fto) (values set-tool-dmgs! tool-dmgs))
          (else
           (error "adj! hit else" o fto))))
  #;(printf "~a ~a ~v\n-- ~v\n"
          who
          (if rem? "rem!" "add!")
          (prefab-struct-key o)
          (prefab-struct-key fto))
  (if rem?
      (ss! fto (remove-id (ob-id o) (ss fto)))
      (ss! fto (cons o (ss fto)))))

(define (rem! o from who)
  (adj! o from who #t))

(define (add! o to who)
  (adj! o to who #f))

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
     (define p (if (and s (player? (car s))) (car s) #f))
     (define ship (if s (get-ship s) #f))
     (define rcship (if p (find-id space (player-rcid p)) #f))
     (define tool (if s (ship-tool (or rcship ship) (command-cmd c)) #f))
     (cond
       ((not s)
        (printf "~a dropping command (no stack) ~v\n" who c)
        (values #f '()))
       ((not tool)
        (printf "~a dropping command (no tool) ~v\n" who c)
        (values #f '()))
       (else
        (case (tool-name tool)
          ((pbolt)
           (change-pbolt! c space s (command-arg c) who))
          ((dock)
           (cond
             ((equal? 'launch (command-arg c))
              (launch! c space s who))
             (else
              (set-tool-rc! tool (command-arg c))
              (values #t '()))))
          ((endrc)
           (cond (p
                  (define o (find-id space (player-rcid p)))
                  (set-player-rcid! p #f)
                  (define changes '())
                  (when (and (server?) (missile? o))
                    ; -1 means missile explodes
                    (append! changes (chdam (ob-id o) -1)))
                  (when (and (server?) (probe? o))
                    (append! changes
                             (command (ob-id o) 'engine #f)
                             (command (ob-id o) 'turnleft #f)
                             (command (ob-id o) 'turnright #f)))
                  (values #t changes))
                 (else
                  (set-tool-rc! tool (command-arg c))
                  (values #t '()))))
          ((probe)
           (launch-probe! c space s who))
          ((missile)
           (launch-missile! c space s who))
          (else
           (cond ((or rcship (not p))
                  ; either we are remote controlling something
                  ; or this command is not coming from a player
                  (set-tool-rc! tool (command-arg c))
                  (values #t '()))
                 (else
                  (define cmds (remove* (list (command-cmd c)) (player-commands p)))
                  (when (command-arg c)
                    (set! cmds (cons (command-cmd c) cmds)))
                  (set-player-commands! p cmds)
                  ;(printf "~a player ~a ~v\n" who (player-name p) (player-commands p))
                  (values #t '())))
           )))))
    ((chrc? c)
     (define p (find-id space (chrc-pid c)))
     (cond
       ((not p)
        (printf "~a dropping command (can't find player) ~v\n" who c)
        (values #f '()))
       (else
        (set-player-rcid! p (chrc-rcid c))
        (values #t '()))))
    ((chadd? c)
     (define to (if (chadd-to c)
                    (find-id space (chadd-to c))
                    space))
     (cond
       (to
        (define o (chadd-o c))
        (add! o to who)
        (while (and (ctime . < . (space-time space))
                    (obj-posvel o))  ; some objs have a #f posvel when inside other things
               ;(printf "~a ticking forward ~v\n" who (chadd-o c))
               (update-physics! space o (/ TICK 1000.0))
               (set! ctime (+ ctime TICK)))
        (values #t '()))
       (else
        (printf "~a dropping chadd (can't find chadd-to) ~v\n" who c)
        (values #f '()))))
    ((chrm? c)
     ; have to manage space-players specially
     (define p (findfid (chrm-id c) (space-players space)))
     (when p (rem! p space who))

     (define s (find-stack space (chrm-id c)))
     (cond
       (s
        (rem! (car s) (cadr s) who)
        (values #t '()))
       (else
        (printf "~a dropping chrm (can't find chrm-id) ~v\n" who c)
        (values #f '()))))
    ((chmov? c)
     (define to (if (chmov-to c)
                    (find-id space (chmov-to c))
                    space))
     (cond
       ((not to)
        (printf "~a dropping chmov (can't find chmov-to) ~v\n" who c)
        (values #f '()))
       (else
        (define p (findfid (chmov-id c) (space-players space)))
        (define s (find-stack space (chmov-id c)))
        (cond
          (s
           (define changes '())
           (rem! (car s) (cadr s) who)
           (when (and (server?) (player? (car s)) (spacesuit? (cadr s)))
             ; leaving a space suit, remove the suit
             (append! changes (chrm (ob-id (cadr s)))))
           (cond ((and (player? (car s)) (space? to))
                  (when (and (server?) (not (spacesuit? (get-ship s))))
                    ; jumping into spacesuit
                    (define ship (get-topship s))
                    (define ss (make-spacesuit (player-name p) ship))
                    (define sspv (obj-posvel ss))
                    ; push spacesuit away from parent ship
                    (define t (atan0 (posvel-dy sspv) (posvel-dx sspv)))
                    (define r (+ 3 (hit-distance ship ss)))
                    (set-posvel-x! sspv (+ (posvel-x sspv) (* r (cos t))))
                    (set-posvel-y! sspv (+ (posvel-y sspv) (* r (sin t))))
                    (append! changes (chadd ss #f) (chmov (ob-id p) (ob-id ss) #f))))
                 (else
                  (add! (car s) to who)
                  ; whenever a player moves somewhere, need to clear out all existing commands
                  (when (player? (car s))
                    (set-player-commands! (car s) '()))
                  (when (obj? (car s)) (set-obj-posvel! (car s) (chmov-pv c)))))
           (values #t changes))
          (p
           (add! p to who)
           ; player could have had commands from a previous ship/scenario
           (set-player-commands! p '())
           (values #t '()))
          (else
           (printf "~a dropping chmov (can't find chmov-id) ~v\n" who c)
           (values #f '()))))))
    ((chfaction? c)
     (define pid (chfaction-playerid c))
     (define p (findfid pid (space-players space)))
     (when p (set-player-faction! p (chfaction-newf c)))
     (values #t '()))
    ((chorders? c)
     ;(printf "~a chorders ~v\n" who c)
     (set-space-orders-for! space (chorders-faction c) (chorders-ot c))
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
    ((chstat? c)
     (define o (find-id space (chstat-id c)))
     (cond
       (o
        (case (chstat-what c)
          ((ai)
           (set-ship-ai?! o (chstat-val c))
           (printf "ship ~a turned off ai\n" (ship-name o))
           (values #t '()))
          ((toolval)
           (define t (ship-tool o (car (chstat-val c))))
           (cond (t (set-tool-val! t (cadr (chstat-val c)))
                    (values #t '()))
                 (else
                  (printf "~a chstat - couldn't find tool ~v\n" c)
                  (values #f '()))))
          (else
           (printf "~a chstat - didn't understand chstat-what ~v\n" c)
           (values #f '()))))
       (else
        (printf "~a chstat - couldn't find obj id ~a\n" who (chstat-id c))
        (values #f '()))))
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
