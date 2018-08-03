#lang racket/base

(require racket/list)

(require "defs.rkt"
         "utils.rkt"
         "ships.rkt"
         "pilot.rkt"
         "pbolt.rkt"
         "warp.rkt"
         "missile.rkt"
         "cannon.rkt"
         "plasma.rkt"
         "shield.rkt"
         "probe.rkt"
         "order.rkt"
         "physics.rkt")

(provide (all-defined-out))


(define (adj! o fto who rem? addf)
  (define-values (ss! ss add-to-space?)
    (cond ((ship? fto)
           (cond ((player? o) (values set-ship-playerids! ship-playerids #f))
                 ((upgrade? o) (values set-ship-cargo! ship-cargo #f))
                 (else (values set-ship-hangar! ship-hangar #f))))
          ((space? fto)
           (cond ((player? o) (values set-space-players! space-players #f))
                 (else (values set-space-objects! space-objects #t))))
          ((tool? fto) (values set-tool-dmgs! tool-dmgs #f))
          (else
           (error "adj! hit else" o fto))))
  #;(printf "~a ~a ~v\n-- ~v\n"
          who
          (if rem? "rem!" "add!")
          (prefab-struct-key o)
          (prefab-struct-key fto))
  (cond ((and (ship? fto) (player? o))
         (if rem?
             (ss! fto (remove (ob-id o) (ss fto)))
             (ss! fto (cons (ob-id o) (ss fto)))))
        (rem?
         (ss! fto (remove-id (ob-id o) (ss fto))))
        (else
         (ss! fto (cons o (ss fto)))
         (when (and addf add-to-space?)
           (addf o)))))

(define (rem! o from who)
  (adj! o from who #t #f))

(define (add! o to who addf)
  (adj! o to who #f addf))

; returns a list of changes you want whenever a player moves or leaves
(define (player-cleanup! space p #:endrc? (endrc? #t))
  (define changes '())
  (set-player-commands! p '())
  (set-player-cmdlevel! p (add1 (player-cmdlevel p)))
  ;(printf "player cleanup ~a cmdlevel ~a\n" (player-name p) (player-cmdlevel p))
  (when (client?)
    ((player-cleanup-client!) (ob-id p)))
  (when (and (server?) endrc?)
    (append! changes (endrc (ob-id p) (player-rcid p)))
    (append! changes (endcb (ob-id p) (player-cbid p))))
  changes)

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
; (or even the same player clicking a button twice before the server sees it)
;
; who is a string? for message reporting
; addf is #f or a function to call when we add something to space-objects
;
(define (apply-change! space c who
                       #:addf (addf #f)
                       #:on-player-restart (on-player-restart #f))
  ;(printf "~a (~a) applying change ~v\n" who (space-time space) c)
  (cond
    ((command? c)
     (define s (find-stack space space (command-id c)))
     (define p (if (and s (player? (car s))) (car s) #f))
     (define ship (if s (get-ship s) #f))
     (define rcship (if p (find-id space space (player-rcid p)) #f))
     (define tool (if s (ship-tool (or rcship ship) (command-cmd c)) #f))
     (cond
       ((not s)
        (printf "~a dropping command (no stack) ~v\n" who c)
        (values #f '()))
       ((not tool)
        (printf "~a dropping command (no tool) ~v\n" who c)
        (values #f '()))
       ((and p (not (= (command-level c) (player-cmdlevel p))))
        (printf "~a dropping command (need cmdlevel ~a) ~v\n" who (player-cmdlevel p) c)
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
           (set-tool-rc! tool (command-arg c))
           (values #t '()))
          ((probe)
           (launch-probe! c space s who))
          ((missile)
           (launch-missile! c space s who))
          ((cannon)
           (fire-cannon! c space s who))
          (else
           (cond
             ((and (equal? 'warp (command-cmd c))
                   (equal? 'stop (command-arg c)))
              (when ship
                (cancel-warp! space ship))
              (values #t '()))
             ((or rcship (not p))
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
     (define p (find-id space space (chrc-pid c)))
     (define o (find-id space space (chrc-rcid c)))
     (cond
       ((not p)
        (printf "~a dropping command (can't find player) ~v\n" who c)
        (values #f '()))
       ((not o)
        (printf "~a dropping command (can't find rcid) ~v\n" who c)
        (values #f '()))
       ((cannonball? o)
        (set-player-cbid! p (chrc-rcid c))
        (values #t '()))
       (else
        (set-player-rcid! p (chrc-rcid c))
        (player-cleanup! space p #:endrc? #f)
        (values #t '()))))
    ((endrc? c)
     (define changes '())
     (define p (findfid (endrc-pid c) (space-players space)))
     (define id (if p (player-rcid p) (endrc-rcid c)))
     (define o (find-id space space id))
     (when (and (server?) (missile? o))
       ; missile explodes
       (append! changes (chdam (ob-id o) (ship-maxcon o) #f)))
     (when (and (server?) (probe? o))
       (append! changes
                (command (ob-id o) #f 'endrc #f)
                (command (ob-id o) #f 'engine #f)
                (command (ob-id o) #f 'turnleft #f)
                (command (ob-id o) #f 'turnright #f)))
     (when p       
       (set-player-rcid! p #f)
       (player-cleanup! space p #:endrc? #f))
     (values #t changes))
    ((endcb? c)
     (define changes '())
     (define p (findfid (endcb-pid c) (space-players space)))
     (define id (if p (player-cbid p) (endcb-cbid c)))
     (define o (find-id space space id))
     (when (and (server?) (cannonball? o))
       ; find and explode player's cbid
       (append! changes (chdam (ob-id o) (ship-maxcon o) #f)))
     (when p
       (set-player-cbid! p #f))
     (values #t changes))
    ((chadd? c)
     (define to (if (chadd-to c)
                    (find-id space space (chadd-to c))
                    space))
     (cond
       (to
        (define o (chadd-o c))
        (add! o to who addf)
        (values #t '()))
       (else
        (printf "~a dropping chadd (can't find chadd-to) ~v\n" who c)
        (values #f '()))))
    ((chrm? c)
     (define changes '())
     (define forward? #t)
     ; have to manage space-players specially
     (define p (findfid (chrm-id c) (space-players space)))
     (define s (find-stack space space (chrm-id c)))
     (cond
       ((or s p)
        ; if both s and p, do s first because we need to remove a player
        ; from a ship before we remove the player
        (when s
          (cond
            ((and (space? (cadr s)) (not (player? (car s))))
             (set-obj-alive?! (car s) #f))
            (else
             (rem! (car s) (cadr s) who))))
        (when p
          (rem! p space who)
          (append! changes (player-cleanup! space p)))
        (values #t changes))
       (else
        (printf "~a dropping chrm (can't find chrm-id) ~v\n" who c)
        (values #f changes))))
    ((chmov? c)
     (define to (if (chmov-to c)
                    (find-id space space (chmov-to c))
                    space))
     (cond
       ((not to)
        (printf "~a dropping chmov (can't find chmov-to) ~v\n" who c)
        (values #f '()))
       (else
        (define p (findfid (chmov-id c) (space-players space)))
        (define s (find-stack space space (chmov-id c)))
        (cond
          (s
           (define changes '())

           ; remove (car s) from wherever it is
           (cond
             ((space? (cadr s))
              ; moving from space-objects, so copy, mark as dead
              (define t (copy (car s)))
              (set-obj-alive?! (car s) #f)
              (set! s (cons t (cdr s))))
             (else
              (rem! (car s) (cadr s) who)
              (when (and (server?) (player? (car s)) (spacesuit? (cadr s)))
                ; leaving a space suit, remove the suit
                (append! changes (chrm (ob-id (cadr s))))
                ; notify caller, they will accumulate pids and call scenario-on-player-restart
                (when on-player-restart
                  (on-player-restart (ob-id (car s))))
                )))

           ; add (car s) to where it should go
           (cond
             ((and (player? (car s)) (space? to))
              (when (and (server?) (not (spacesuit? (get-ship s))))
                ; jumping into spacesuit
                (define ship (get-topship s))
                (define ss (make-spacesuit (player-name p) ship))
                (define sspv (obj-posvel ss))
                ; push spacesuit away from parent ship
                (define t (atan0 (posvel-dy sspv) (posvel-dx sspv)))
                (define r (+ 3.0 (hit-distance ship ss)))
                (set-posvel-x! sspv (+ (posvel-x sspv) (* r (cos t))))
                (set-posvel-y! sspv (+ (posvel-y sspv) (* r (sin t))))
                (append! changes (chadd ss #f) (chmov (ob-id p) (ob-id ss) #f))))
             (else
              (when (obj? (car s)) (set-obj-posvel! (car s) (chmov-pv c)))
              (add! (car s) to who addf)
              ; whenever a player moves somewhere, need to clear out all existing commands
              (when (player? (car s))
                (append! changes (player-cleanup! space (car s))))
              (when (and (ship? (car s)) (ship? to))
                (define ship (car s))
                ; ship is docking, clean up warp if we have it
                (when (ship-tool ship 'warp)
                  (append! changes (command (ob-id ship) #f 'warp 'stop))))))
           (values #t changes))
          (p
           (add! p to who addf)
           ; player could have had commands from a previous ship/scenario
           (define changes (player-cleanup! space p))
           (values #t changes))
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
     (define o (find-id space space (chdam-id c)))
     (define d (chdam-damage c))
     (cond (o
            (values #t
                    (cond ((plasma? o) (reduce-plasma! space o d))
                          ((missile? o) (reduce-missile! space o d))
                          ((cannonball? o) (reduce-cannonball! space o d))
                          ((shield? o) (reduce-shield! space o d) '())
                          ((upgrade? o) (set-upgrade-life! o 0) '())
                          ((ship? o)
                           (define cs (reduce-ship! space o d))
                           (when (and (obj-alive? o) (client?) (chdam-fx c))
                             (set-ship-dmgfx! o (min 12.0 (+ (ship-dmgfx o) d))))
                           cs))))
           (else
            (printf "~a chdam - couldn't find obj id ~a\n" who (chdam-id c))
            (values #t '()))))
    ((new-strat? c)
     (define o (find-id space space (new-strat-ship-id c)))
     (cond (o
            (set-ship-ai-strategy! o (new-strat-strats c))
            (set-ship-ai-strat-time! o (space-time space))
            (values #t '()))
           (else
            (printf "~a new-strat - couldn't find obj id ~a\n" who (new-strat-ship-id c))
            (values #t '()))))
    ((chstats? c)
     (define o (find-id space space (chstats-id c)))
     (cond (o
            (set-ship-stats! o (chstats-newstats c))
            ;(printf "ship ~a now has stats ~v\n" (ship-name o) (ship-stats o))
            (values #t '()))
           (else
            (printf "~a chstats - couldn't find obj id ~a\n" who (chstats-id c))
            (values #t '()))))
    ((chstat? c)
     (define o (find-id space space (chstat-id c)))
     (cond
       (o
        (case (chstat-what c)
          ((ai)
           (set-ship-ai! o (chstat-val c))
           (values #t '()))
          ((toolval)
           (define t (ship-tool o (car (chstat-val c))))
           (cond (t (set-tool-val! t (cadr (chstat-val c)))
                    (values #t '()))
                 (else
                  (printf "~a chstat - couldn't find tool ~v\n" c)
                  (values #f '()))))
          ((overlay)
           (define others (filter-not (lambda (ov)
                                        (equal? (car ov) (car (chstat-val c))))
                                      (ship-overlays o)))
           (set-ship-overlays! o (if (cdr (chstat-val c))
                                     (cons (chstat-val c) others)
                                     others))
           (values #t '()))
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


(define (apply-all-changes! space changes who
                            #:addf (addf #f)
                            #:on-player-restart (on-player-restart #f))
  (if (null? changes)
      '()
      (apply append
             (for/list ((c (in-list changes)))
               (define-values (forward? new-changes)
                 (apply-change! space c who
                                #:addf addf
                                #:on-player-restart on-player-restart))
               (append (if forward? (list (copy c)) '())
                       (apply-all-changes! space new-changes who
                                           #:addf addf
                                           #:on-player-restart on-player-restart))))))
