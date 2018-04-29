#lang racket/base

(require racket/tcp)

(require "defs.rkt"
         "utils.rkt"
         "change.rkt"
         "physics.rkt"
         "quadtree.rkt"
         "pilot.rkt"
         "warp.rkt"
         "plasma.rkt"
         "pbolt.rkt"
         "missile.rkt"
         "cannon.rkt"
         "shield.rkt"
         "scenario.rkt"
         "scenarios/testing.rkt"
         "upgrade.rkt")

(provide start-server)

(struct client (player in-port out-port in-t out-t) #:mutable #:prefab)
(define (client-id c) (ob-id (client-player c)))

(define server-listener #f)
(define clients '())
(define ownspace #f)
(define (scenario-on-tick change-scenario!) '())
(define (scenario-on-message cmd change-scenario!) '())


; return a list of changes
(define (upgrade-hit-ship! space ship u)
  (define changes '())
  ;(printf "upgrade hit ship ~a ~a\n" (ship-name ship) (upgrade-type u))
  (define newstats (struct-copy stats (ship-stats ship)))
  (define sendstats #t)
  (define which
    (case (upgrade-type u)
      (("engines")
       (set! sendstats #f)
       (define t (ship-tool ship 'engine))
       (when t (append! changes (chstat (ob-id ship) 'toolval (list 'engine (* 1.1 (tool-val t))))))
       "engines")
      (("turning")
       (set! sendstats #f)
       (for ((tname '(turnleft turnright steer)))
         (define t (ship-tool ship tname))
         (when t
           (append! changes (chstat (ob-id ship) 'toolval (list tname (* 1.1 (tool-val t)))))))
       "turn speed")
      (("hull") (set-stats-maxcon! newstats (* 1.1 (stats-maxcon newstats))) "hull")
      (("radar") (set-stats-radar! newstats (* 1.1 (stats-radar newstats))) "radar")
      (else #f)))
  (cond (which
         (define m (message (next-id) (space-time space) #f (format "~a upgraded ~a" (ship-name ship) which)))
         (when sendstats (append! changes (chstats (ob-id ship) newstats)))
         (append! changes (list m (chdam (ob-id u) 1 #f))))
        (else
         (append! changes (chmov (ob-id u) (ob-id ship) #f))))
   
  changes)


; return a list of changes
(define (plasma-hit-ship! space ship p)
  (define changes '())
  ;(printf "plasma hit ship ~a (~a)\n" (ship-name ship) (ob-id ship))
    
  (define damage (plasma-damage space p))
  (append! changes (chdam (ob-id p) damage #f)
           (chdam (ob-id ship) damage #f))
  changes)


; return a list of changes
(define (missile-hit-ship! space ship m)
  (define changes '())
  ;(printf "missile hit ship ~a\n" (ship-name ship))
  
  (define damage (ship-maxcon m))
  (append! changes (list (chdam (ob-id ship) damage #t)
                         (chdam (ob-id m) damage #f)))
  changes)


(define (missile-hit-missile! space m1 m2)
  (define changes '())
  (define d (+ (ship-maxcon m1) (ship-maxcon m2)))
  (append! changes
           (chdam (ob-id m1) d #f)
           (chdam (ob-id m2) d #f))
  changes)


(define (cb-hit-cb! space cb1 cb2)
  (define changes '())
  (define d (+ (ship-maxcon cb1) (ship-maxcon cb2)))
  (append! changes
           (chdam (ob-id cb1) d #f)
           (chdam (ob-id cb2) d #f))
  changes)


(define (cb-hit-ship! space cb ship)
  (define changes '())
  ;(printf "cannon hit ship ~a\n" (ship-name ship))

  (define d (ship-maxcon cb))
  (append! changes (list (chdam (ob-id ship) d #t)
                         (chdam (ob-id cb) d #f)))
  changes)


; return a list of changes
(define (plasma-hit-shield! space s p)
  (define changes '())
  (when (and (not (shield-dead? space s))
             (not (plasma-dead? space p)))
    (define r (obj-r s))
    (define px (- (obj-x p) (obj-x s)))
    (define py (- (obj-y p) (obj-y s)))
    (define x (+ (* px (cos r)) (* py (sin r))))
    (define y (+ (* -1 px (sin r)) (* py (cos r))))
    ; x,y are now the position of the plasma in the coord space of the shield
    ; shield is along the y axis
    (define rad (plasma-radius space p))
    (define l (shield-length space s))
    (when (and (< (- rad) x rad)
               (< (- (- (/ l 2)) rad) y (+ (/ l 2) rad)))
      (define damage (min (plasma-damage space p) (shield-energy space s)))
      (append! changes (list (chdam (ob-id p) damage #f)
                             (chdam (ob-id s) damage #f)))))
  changes)


(define (perpv o1 m1 o2 m2)
  (define phi (theta o1 o2))
  (/ (+ (* (dmag o1) (cos (- (dtheta o1) phi)) (- m1 m2))
        (* 2 m2 (dmag o2) (cos (- (dtheta o2) phi))))
     (+ m1 m2)))


(define (ship-collide! s1 s2)
  (cond
    ((and (= 0 (dmag s1)) (= 0 (dmag s2)))
     ; ships aren't moving, but somehow collided, just push them back apart
     (define t (theta s1 s2))
     (define d (+ 1 (- (hit-distance s1 s2) (distance s1 s2))))
     (define pv1 (obj-posvel s1))
     (set-posvel-x! pv1 (- (posvel-x pv1) (* (/ d 2) (cos t))))
     (set-posvel-y! pv1 (- (posvel-y pv1) (* (/ d 2) (sin t))))
     (define pv2 (obj-posvel s2))
     (set-posvel-x! pv2 (+ (posvel-x pv2) (* (/ d 2) (cos t))))
     (set-posvel-y! pv2 (+ (posvel-y pv2) (* (/ d 2) (sin t)))))
    (else
     (define m1 (stats-mass (ship-stats s1)))
     (define m2 (stats-mass (ship-stats s2)))
     (define phi (theta s1 s2))
     (define perpv1 (perpv s1 m1 s2 m2))
     (define perpv2 (- (perpv s2 m2 s1 m1)))
     ;(printf "perpv1 ~a perpv2 ~a\n" perpv1 perpv2)
     
     (set-posvel-dx! (obj-posvel s1)
                     (+ (* perpv1 (cos phi))
                        (* (dmag s1) (sin (- (dtheta s1) phi)) (cos (+ phi pi/2)))))
     (set-posvel-dy! (obj-posvel s1)
                     (+ (* perpv1 (sin phi))
                        (* (dmag s1) (sin (- (dtheta s1) phi)) (sin (+ phi pi/2)))))
     
     
     (set-posvel-dx! (obj-posvel s2)
                     (+ (* perpv2 (cos phi))
                        (* (dmag s2) (sin (- (dtheta s2) phi)) (cos (+ phi pi/2)))))
     (set-posvel-dy! (obj-posvel s2)
                     (+ (* perpv2 (sin phi))
                        (* (dmag s2) (sin (- (dtheta s2) phi)) (sin (+ phi pi/2)))))
     
     ; push the ships out along their new velocities until they aren't colliding
     
     (define dv (abs (- (* (dmag s1) (cos (- (dtheta s1) phi)))
                        (* (dmag s2) (cos (- (dtheta s2) phi))))))
     
     (define dt (/ (+ 1 (- (hit-distance s1 s2) (distance s1 s2))) dv))
  
     ;(printf "dv ~a dt ~a\n" dv (- dt (/ TICK 1000.0)))
     (physics! (obj-posvel s1) (- dt (/ TICK 1000.0)))
     (physics! (obj-posvel s2) (- dt (/ TICK 1000.0)))))
  
  ; make sure we send the new posvels right away
  (set-posvel-t! (obj-posvel s1) 0)
  (set-posvel-t! (obj-posvel s2) 0))


; return a list of changes
(define (dock! s1 s2)  
  (define changes '())
  (append! changes (list (chmov (ob-id s1) (ob-id s2) #f)))
  changes)


(define (ship-hit-ship! space ship s)
  (define changes '())
  ;(printf "ship ~a hit ship ~a\n" (ship-name ship) (ship-name s))
  (cond
    ((and (spacesuit? ship) (spacesuit? s))
     #f)
    ((or (spacesuit? ship) (spacesuit? s))
     (when (spacesuit? ship)
       (define temp ship)
       (set! ship s)
       (set! s temp))
     (when ((faction-check (ship-faction ship) (ship-faction s)) . > . 0)
       ; don't need to explicitly remove the spacesuit because chmov does that
       (append! changes (chmov (car (ship-playerids s))
                               (ob-id ship) #f))))
    ((will-dock? ship s)       
     (append! changes (dock! ship s)))
    ((will-dock? s ship)
     (append! changes (dock! s ship)))
    (else
     (cond
       ((or (warping? ship) (warping? s))
        (when (warping? ship)
          (append! changes (command (ob-id ship) #f 'warp 'stop))
          (define t (theta ship s))
          (define d (+ 1.0 (- (hit-distance ship s) (distance ship s))))
          (when (warping? s)
            ; if other ship is also warping, it will do half the work
            (set! d (/ d 2.0)))
          (define pv1 (obj-posvel ship))
          (set-posvel-x! pv1 (- (posvel-x pv1) (* d (cos t))))            
          (set-posvel-y! pv1 (- (posvel-y pv1) (* d (sin t))))
          (set-posvel-dx! pv1 (* -10.0 (cos t)))
          (set-posvel-dy! pv1 (* -10.0 (sin t)))
          ; make sure we send the new posvel right away
          (set-posvel-t! pv1 0))
        (when (warping? s)
          (append! changes (command (ob-id s) #f 'warp 'stop))
          (define t (theta s ship))
          (define d (+ 1.0 (- (hit-distance ship s) (distance ship s))))
          (define pv1 (obj-posvel s))
          (set-posvel-x! pv1 (- (posvel-x pv1) (* d (cos t))))
          (set-posvel-y! pv1 (- (posvel-y pv1) (* d (sin t))))
          (set-posvel-dx! pv1 (* -10.0 (cos t)))
          (set-posvel-dy! pv1 (* -10.0 (sin t)))
          ; make sure we send the new posvel right away
          (set-posvel-t! pv1 0)))
       (else
        (ship-collide! ship s)))))
  changes)


(define (obj-radius space o)
  (cond
    ((ship? o) (ship-radius o))
    ((plasma? o) (plasma-radius space o))
    ((upgrade? o) (upgrade-radius space o))
    (else #f)))


; called once on every object
; return a list of changes
(define (upkeep! space o)
  (define changes '())
  (cond
    ((and (ship? o)
          (warping? o)
          (outside? space o))
     ; stop warp for any ships that hit the edge of space
     (append! changes (command (ob-id o) #f 'warp 'stop)))
    ((probe? o)
     (define t (ship-tool o 'endrc))
     (when (and (tool-rc t)
                ((tool-rc t) . <= . (/ (obj-age space o) 1000.0)))
       (define player (findf (lambda (x) (equal? (player-rcid x) (ob-id o)))
                             (space-players space)))
       (append! changes (endrc (and player (ob-id player)) (ob-id o)))))
    ((cannonball? o)
     (when ((obj-age space o) . > . 30000.0)
       (append! changes (chdam (ob-id o) (ship-maxcon o) #f))))
    ((missile? o)
     (define t (ship-tool o 'endrc))
     (when ((tool-rc t) . <= . (/ (obj-age space o) 1000.0))
       (append! changes (chdam (ob-id o) (ship-maxcon o) #f)))))
  changes)

; numeric priority that controls the order that objects are seen by collide!
(define (priority o)
  (cond ((plasma? o) 0)
        ((cannonball? o) 1)
        ((missile? o) 2)
        ((upgrade? o) 3)
        ((spaceship? o) 4)
        ((probe? o) 4)
        ((spacesuit? o) 4)
        (else (printf "priority unknown for ~v\n" o) 5)))


; called on every pair of objects that might be colliding
; called only once for each pair
; (priority a) <= (priority b)
; return a list of changes
(define (collide! space a b)
  (set! num-collide (+ 1 num-collide))
  (cond
    ((plasma? a)
     (cond ((or (spaceship? b)
                (probe? b)
                (missile? b)
                (cannonball? b))
            (when ((distance a b) . < . (+ (ship-radius b) (plasma-radius space a)))
              (plasma-hit-ship! space b a)))))
    ((cannonball? a)
     (cond ((cannonball? b)
            (when ((distance a b) . < . (hit-distance a b))
              (cb-hit-cb! space a b)))
           ((or (spaceship? b)
                (missile? b)
                (probe? b))
            (when ((distance a b) . < . (hit-distance a b))
              (cb-hit-ship! space a b)))))
    ((missile? a)
     (cond ((missile? b)
            (when ((distance a b) . < . (hit-distance a b))
              (missile-hit-missile! space a b)))
           ((or (spaceship? b)
                (probe? b))
            (when ((distance a b) . < . (hit-distance a b))
              (missile-hit-ship! space b a)))))
    ((upgrade? a)
     (cond ((spaceship? b)
            (when ((distance a b) . < . (+ (ship-radius b) (upgrade-radius space a)))
              (upgrade-hit-ship! space b a)))))
    ((or (spaceship? a)
         (probe? a)
         (spacesuit? a))
     (cond
       ((or (spaceship? b)
            (probe? b)
            (spacesuit? b))
        (when ((distance a b) . < . (hit-distance a b))
          (ship-hit-ship! space a b)))))))

(define num-collide 0)

; Is object o still in the running for collisions?
(define (live? space o)
  (cond
    ((ship? o)
     (and (ship-flying? o)
          ((ship-con o) . > . 0)))
    ((plasma? o)
     (not (plasma-dead? space o)))
    ((upgrade? o)
     (and (obj-posvel o)
          (upgrade-alive? space o)))
    (else
     (printf "live? called on ~v\n" o)
     #f)))

; return a list of final changes
(define (update-effects! space)
  (define changes '())

  (define num (length (space-objects space)))
  (set! num-collide 0)
  (define time-collide 0)

  ;(timeit time-collide
  (define qt (qt-new 0 0 (space-width space) (space-height space)))
  (for ((o (space-objects space)))
    (define precs (upkeep! space o))
    (define cs (apply-all-changes! space precs (space-time space) "server"))
    (append! changes cs)
    
    (define rad (obj-radius space o))
    (when rad
      (qt-add! qt o (obj-x o) (obj-y o) rad)))

  (define (coll! a b)
    (when (and (live? space a)
               (live? space b))
      (define precs (if ((priority a) . <= . (priority b))
                        (collide! space a b)
                        (collide! space b a)))
      (when (not (void? precs))
        (define cs (apply-all-changes! space precs (space-time space) "server"))
        (append! changes cs))))
  
  (qt-collide! qt coll!)
  ;)

  ;(printf "collision test took ~a ms (~a : ~a)\n" time-collide num num-collide)

  ; find out if any player's rc objects went away
  (for ((p (space-players space))
        #:when (and (player-rcid p)
                    (not (find-id space space (player-rcid p)))))
    (define cs (apply-all-changes! space (list (endrc (ob-id p) #f)) (space-time space) "server"))
    (append! changes cs))
  
  changes)


; return a list of commands
(define (run-ai! space)
  (define changes '())
  
  (define stacks (search space space ai-ship? #t))

  ; if we haven't seen this ship before, set ai runtime to 0
  (for ((s (in-list stacks)))
    (define ship (car s))
    (when (equal? #t (ship-ai? ship))
      (set-ship-ai?! ship 0))
    
    (when ((- (space-time space) (ship-ai? ship)) . > . (ship-ai-freq ship))
      (set-ship-ai?! ship (space-time space))  ; set runtime

      ;(printf "running ai for ship ~a\n" (ship-name ship))
      
      ; run this ship's ai
      (when (and (ship-tool ship 'engine) (ship-tool ship 'steer))
        (when (not (missile? ship))
          (append! changes (pilot-ai-strategy! space s)))
        (when (ship-flying? (get-ship s))
          (append! changes (pilot-ai-fly! space s))))

      (when (ship-tool ship 'pbolt)
        (append! changes (pbolt-ai! space s)))

      (when (ship-tool ship 'cannon)
        (append! changes (cannon-ai! space s)))
      (when (cannonball? ship)
        (append! changes (cannonball-ai! space s)))

      #;(when (findf shbolt? (pod-tools p))
        (append! changes (shbolt-ai! space s)))

      (when (ship-tool ship 'missile)
        (append! changes (missile-ai! space s)))
      ))
  
  changes)


(define updates '())
     
(define (remove-client cid)
  (define c (findf (lambda (o) (= cid (client-id o))) clients))
  (cond
    ((not c)
     (printf "already removed client ~a\n" cid))
    (else
     (printf "removing client ~v\n" (client-player c))
     (define m (message (next-id) (space-time ownspace) #f
                        (format "Player Left: ~a" (player-name (client-player c)))))
     (append! updates
              (apply-all-changes! ownspace
                                  (list (chrm (client-id c)) m)
                                  (space-time ownspace) "server"))

     (close-input-port (client-in-port c))
     (with-handlers ((exn:fail:network? (lambda (exn) #f)))
       (close-output-port (client-out-port c)))
     (kill-thread (client-in-t c))
     (kill-thread (client-out-t c))
     (set! clients (remove c clients)))))

(define (send-to-client c v)
  (thread-send (client-out-t c) v))


(define previous-physics-time #f)

(define time-new-clients 0)
(define time-commands 0)
(define time-tick 0)
(define time-effects 0)
(define time-ai 0)
(define time-output 0)

(define-syntax-rule (timeit var e ...)
  (begin
    (define t (current-milliseconds))
    e ...
    (set! var (- (current-milliseconds) t))))

(define-syntax-rule (outputtime v ...)
  (begin
    (when (v . > . 1)
      (printf "~a : server ~a ~a\n" (space-time ownspace) 'v v)) ...))
    
(define (server-loop)
  (define current-time (current-milliseconds))
  (when (not previous-physics-time)
    (set! previous-physics-time current-time))
  
  ; process new clients
  (timeit time-new-clients
  (when (tcp-accept-ready? server-listener)
    (printf "server accept-ready\n")
    (define-values (in out) (tcp-accept server-listener))
    (set-tcp-nodelay! out #t)
    (define cid (next-id))
    (define c (client (player cid #f #f 0 '() #f #f)
                      in out
                      (make-in-thread cid in)
                      (make-out-thread cid out)))
    (send-to-client c (client-player c))  ; assign an id
    (append! clients (list c)))
  )

  ; process commands
  (timeit time-commands
  (let loop ()
    (define v (thread-try-receive))
    (when v
      (define cid (car v))
      (define u (cdr v))
      (cond
        ((not u)
         (remove-client cid))
        (else
         (when (and (update-time u) ((- (space-time ownspace) (update-time u)) . > . 70))
           (printf "~a : client ~a is behind ~a\n" (space-time ownspace) cid
                   (- (space-time ownspace) (update-time u))))
         (for ((ch (in-list (update-changes u))))
           (cond
             ((player? ch)
              (define c (findf (lambda (o) (= cid (client-id o))) clients))
              (set-player-name! (client-player c) (player-name ch))
              (send-to-client c (copy ownspace))  ; send full state
              (append! updates
                       (apply-all-changes! ownspace (list (chadd (client-player c) #f)) (space-time ownspace) "server"))
              (define m (message (next-id) (space-time ownspace) #f (format "New Player: ~a" (player-name ch))))
              (append! updates (apply-all-changes! ownspace (list m) (space-time ownspace) "server")))
             ((anncmd? ch)
              (scenario-on-message ownspace ch change-scenario!))
             (else
              (define command-changes
                (apply-all-changes! ownspace (list ch) (space-time ownspace) "server"))
              (append! updates command-changes))))))
      (loop)))
  )
  
  ; simulation tick
  (when (TICK . < . (- current-time previous-physics-time))
    ; physics
    (timeit time-tick
    (set! previous-physics-time (+ previous-physics-time TICK))
    (set-space-time! ownspace (+ (space-time ownspace) TICK))
    (for ((o (space-objects ownspace)))
      (update-physics! ownspace o (/ TICK 1000.0))
      (when (ship? o) (update-ship! ownspace o (/ TICK 1000.0))))
    )
      

    ; collisions
    ; update-effects! returns already-applied changes
    (timeit time-effects
    (append! updates (update-effects! ownspace))
    )

    ; scenario hook
    (append! updates (apply-all-changes! ownspace (scenario-on-tick ownspace change-scenario!)
                                       (space-time ownspace) "server"))

    ; ai
    (timeit time-ai
    (append! updates (apply-all-changes! ownspace (run-ai! ownspace)
                                         (space-time ownspace) "server"))
    )
    
    
    (timeit time-output
    ; send any 0-time posvels and least-recently sent
    (define oldest #f)
    (define pvupdates '())
    ;(printf "pvts (~a) :" (space-time ownspace))
    (for ((o (space-objects ownspace))
          #:when (ship? o))
      (define pv (obj-posvel o))
      ;(printf " ~a" (posvel-t pv))
      (cond ((= 0 (posvel-t pv))
             (set-posvel-t! pv (space-time ownspace))
             (set! pvupdates (cons (pvupdate (ob-id o) pv) pvupdates)))
            ((or (not oldest)
                 (< (- (space-time ownspace) (posvel-t (obj-posvel oldest)))
                    (- (space-time ownspace) (posvel-t pv))))
             (set! oldest o))))
    ;(printf "\n")

    (when oldest
      (define old-t (- (space-time ownspace) (posvel-t (obj-posvel oldest))))
      (when (and (old-t . > . 5000)
                 (time-for (space-time ownspace) 1000))
        (printf "server oldest posvel is ~a\n" old-t))
      (set-posvel-t! (obj-posvel oldest) (space-time ownspace))
      (set! pvupdates (cons (pvupdate (ob-id oldest) (obj-posvel oldest)) pvupdates)))
  
    ; make total update message
    (define u (update (space-time ownspace) updates pvupdates))
  
    ; reset this before trying to send, so we can accumulate
    ; client-disconnect updates if there's an error
    (set! updates '())
  
    ;(printf "server queuing time ~v\n" (update-time u))
    (define msg (copy u))
    (for ((c clients))
      (send-to-client c msg))
    )
    )
  
  ; sleep so we don't hog the whole racket vm
  (define sleep-time (- (+ previous-physics-time TICK 1)
                        (current-milliseconds)))
  (cond
    ((sleep-time . > . 0)
     (sleep (/ sleep-time 1000.0)))
    (else
     (printf "~a : server skipping sleep ~a\n" (space-time ownspace) sleep-time)
     (outputtime time-new-clients
                 time-commands
                 time-tick
                 time-effects
                 time-ai
                 time-output)
     ))
  
  (server-loop))



(define (change-scenario! (scenario sc-pick))
  (define-values (newspace on-tick on-message) (scenario ownspace scenario-on-tick scenario-on-message))
  ;(printf "start ownspace ~v\n" new-space)
  (set! ownspace newspace)
  (set! scenario-on-tick on-tick)
  (set! scenario-on-message on-message)
  (define msg (copy ownspace))
  (for ((c clients))
    (send-to-client c msg)))


(define (start-server (port PORT) #:scenario (scenario sc-pick))
  (change-scenario! scenario)
  (set! server-listener (tcp-listen port 4 #t))
  (printf "waiting for clients...\n")
  (server-loop))


(module+ main
  (start-server
   ;#:scenario testing-scenario
   ))
