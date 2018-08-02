#lang racket/base

(require racket/tcp
         racket/async-channel)

(require "defs.rkt"
         "utils.rkt"
         "change.rkt"
         "physics.rkt"
         "quadtree.rkt"
         "pilot.rkt"
         "warp.rkt"
         "plasma.rkt"
         "explosion.rkt"
         "pbolt.rkt"
         "missile.rkt"
         "cannon.rkt"
         "shield.rkt"
         "scenario.rkt"
         "scenarios/testing.rkt"
         "upgrade.rkt")

(provide start-server)

; client connected, we are waiting for their player name
(define CLIENT_STATUS_NEW 0)
; client needs whole ownspace, don't send any updates until then
; - changing the scenario puts everybody here
(define CLIENT_STATUS_WAITING_FOR_SPACE 1)
; normal operation, send updates
(define CLIENT_STATUS_OK 2)

(struct client (status player in-port out-port in-t out-t) #:mutable #:prefab)
(define (client-id c) (ob-id (client-player c)))

(define server-listener #f)
(define clients '())
(define ownspace #f)
(define (scenario-on-tick change-scenario!) '())
(define (scenario-on-message cmd change-scenario!) '())
(define scenario-on-player-restart #f)

; debugging
(define spacebox #f)


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
         (define m (message (next-id) (space-time space) #t #f (format "~a upgraded ~a" (ship-name ship) which)))
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
  (define changes '())
  (define s1dx (obj-dx s1))
  (define s1dy (obj-dy s1))
  (define s2dx (obj-dx s2))
  (define s2dy (obj-dy s2))

  (when (and (= 0.0 (dmag s1)) (= 0.0 (dmag s2)))
    ; ships aren't moving, but somehow collided, maybe launched at the same time?
    ; fake as if the less massive one was moving towards the other
    (let ((s1 s1) (s2 s2))
      (when ((ship-mass s2) . < . (ship-mass s1))
        (define t s1)
        (set! s1 s2)
        (set! s2 t))
      (define t (theta s1 s2))
      (define pv1 (obj-posvel s1))
      (define d (- (+ 1.0 (hit-distance s1 s2))
                   (distance s1 s2)))
      (set-posvel-dx! pv1 (* d (cos t)))
      (set-posvel-dy! pv1 (* d (sin t)))))
     
  (define m1 (stats-mass (ship-stats s1)))
  (define m2 (stats-mass (ship-stats s2)))
  (define phi (theta s1 s2))
  (define perpv1 (perpv s1 m1 s2 m2))
  (define perpv2 (- (perpv s2 m2 s1 m1)))
  ;(printf "perpv1 ~a perpv2 ~a\n" perpv1 perpv2)
  
  (set-posvel-dx! (obj-posvel s1)
                  (* 0.9 (+ (* perpv1 (cos phi))
                            (* (dmag s1) (sin (- (dtheta s1) phi)) (cos (+ phi pi/2))))))
  (set-posvel-dy! (obj-posvel s1)
                  (* 0.9 (+ (* perpv1 (sin phi))
                            (* (dmag s1) (sin (- (dtheta s1) phi)) (sin (+ phi pi/2))))))
  
  
  (set-posvel-dx! (obj-posvel s2)
                  (* 0.9 (+ (* perpv2 (cos phi))
                            (* (dmag s2) (sin (- (dtheta s2) phi)) (cos (+ phi pi/2))))))
  (set-posvel-dy! (obj-posvel s2)
                  (* 0.9 (+ (* perpv2 (sin phi))
                            (* (dmag s2) (sin (- (dtheta s2) phi)) (sin (+ phi pi/2))))))
  
  ; make sure we send the new posvels right away
  (set-posvel-t! (obj-posvel s1) 0)
  (set-posvel-t! (obj-posvel s2) 0)

  ; damage ships by how much their velocities changed
  (define s1xch (- s1dx (obj-dx s1)))
  (define s1ych (- s1dy (obj-dy s1)))
  (define s1dam (/ (sqrt (+ (* s1xch s1xch) (* s1ych s1ych))) 4.0))
  (append! changes (chdam (ob-id s1) s1dam #t))

  (define s2xch (- s2dx (obj-dx s2)))
  (define s2ych (- s2dy (obj-dy s2)))
  (define s2dam (/ (sqrt (+ (* s2xch s2xch) (* s2ych s2ych))) 4.0))
  (append! changes (chdam (ob-id s2) s2dam #t))

  changes)


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
     (when (warping? ship)
       (append! changes (command (ob-id ship) #f 'warp 'stop)))
     (when (warping? s)
       (append! changes (command (ob-id s) #f 'warp 'stop)))
     ; get relative velocity of s with respect to ship
     (define vx (- (obj-dx s) (obj-dx ship)))
     (define vy (- (obj-dy s) (obj-dy ship)))
     ; get position vector from ship to s
     (define sx (- (obj-x s) (obj-x ship)))
     (define sy (- (obj-y s) (obj-y ship)))
     ; dot velocity to position
     (define dot (+ (* vx sx) (* vy sy)))
     (when (dot . <= . 0.0)
       ; only collide if the ships are moving towards each other
       (append! changes (ship-collide! ship s)))))
  changes)


(define (obj-radius space o)
  (cond
    ((ship? o) (ship-radius o))
    ((plasma? o) (plasma-radius space o))
    ((explosion? o) (explosion-radius o))
    ((upgrade? o) (upgrade-radius space o))
    (else #f)))


(define (add-all! qt objs)
  (for ((o objs)
        #:when (obj-alive? o))
    (define rad (obj-radius ownspace o))
    (when rad
      (qt-add! qt o (obj-x o) (obj-y o) rad))))


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
  (cond ((explosion? o) 0)
        ((plasma? o) 1)
        ((cannonball? o) 2)
        ((missile? o) 3)
        ((upgrade? o) 4)
        ((spaceship? o) 5)
        ((probe? o) 5)
        ((spacesuit? o) 5)
        (else (printf "priority unknown for ~v\n" o) 6)))


; called on every pair of objects that might be colliding
; called only once for each pair
; (priority a) <= (priority b)
; return a list of changes
(define (collide! space a b dt)
  (cond
    ((explosion? a)
     (cond ((plasma? b)
            (when ((distance a b) . < . (+ (plasma-radius space b) (explosion-radius a)))
              (list (chdam (ob-id b) (explosion-damage a dt) #t))))
           ((or (cannonball? b)
                (missile? b)
                (spaceship? b)
                (probe? b))
            (when ((distance a b) . < . (+ (ship-radius b) (explosion-radius a)))
              (list (chdam (ob-id b) (explosion-damage a dt) #t))))))
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


; return a list of final changes
(define (update-effects! space dt)
  (define changes '())

  (for ((o (space-objects space))
        #:when (obj-alive? o))
    (define precs (upkeep! space o))
    (define cs (apply-all-changes! space precs "server"))
    (append! changes cs))

  ; find out if any player's rc objects went away
  (for ((p (space-players space))
        #:when (and (player-rcid p)
                    (not (find-id space space (player-rcid p)))))
    (define cs (apply-all-changes! space (list (endrc (ob-id p) #f)) "server"))
    (append! changes cs))

  changes)


; return a list of commands
(define (run-ai! space qt)
  (define updates '())
  
  (define stacks (search space space ai-ship? #t))
  (define delay 0)

  ; if we haven't seen this ship before, set ai runtime to 0
  (for ((s (in-list stacks)))
    (define changes '())
    (define ship (car s))
    (when (equal? #t (ship-ai? ship))
      (set-ship-ai?! ship 0))
    
    (when (and (obj-alive? ship)
               ((- (space-time space) (ship-ai? ship)) . > . (ship-ai-freq ship)))
      (set-ship-ai?! ship (+ (space-time space) delay))  ; set runtime
      (set! delay (+ TICK delay))  ; push ais away from running in the same tick

      ;(printf "running ai for ship ~a\n" (ship-name ship))
      
      ; run this ship's ai
      (when (and (ship-tool ship 'engine)
                 (ship-tool ship 'steer))
        (when (not (missile? ship))
          (append! changes (pilot-ai-strategy! space qt s)))
        (when (and (ship-flying? ship)
                   (ship-strategy ship))
          (append! changes (pilot-ai-fly! space qt s))))

      (when (ship-tool ship 'pbolt)
        (append! changes (pbolt-ai! qt s)))

      (when (ship-tool ship 'cannon)
        (append! changes (cannon-ai! qt s)))
      (when (cannonball? ship)
        (append! changes (cannonball-ai! qt s)))

      (when (ship-tool ship 'missile)
        (append! changes (missile-ai! space qt s)))
      )

    ; if the ai does something that adds to space-objects (like launching)
    ; then add those to the quadtree so later ais see it
    (define (addf o)
      (add-all! qt (list o)))
    (append! updates (apply-all-changes! ownspace changes "server" #:addf addf)))

  ;(printf "ran ~a ai\n" (/ delay TICK))
  
  updates)


(define updates '())
     
(define (remove-client cid)
  (define c (findf (lambda (o) (= cid (client-id o))) clients))
  (when c
    (printf "server (~a) removing client ~a\n" (length clients) (client-id c))
    (define m (message (next-id) (space-time ownspace) #t #f
                       (format "Player Left: ~a" (player-name (client-player c)))))
    (append! updates
             (apply-all-changes! ownspace
                                 (list (chrm (client-id c)) m)
                                 "server"))

     (close-input-port (client-in-port c))
     (with-handlers ((exn:fail:network? (lambda (exn) #f)))
       (close-output-port (client-out-port c)))
     (kill-thread (client-in-t c))
     (kill-thread (client-out-t c))
     (set! clients (remove c clients))))

; for debugging to artificially introduce lag from server->client
;(define delay-ch (make-async-channel))
;(thread
; (lambda ()
;   (define delay 250.0)
;   (define count 0)
;   (let loop ()
;     (define vs (async-channel-get delay-ch))
;     (define d (- delay (- (current-milliseconds) (car vs))))
;     ;(printf "delaying ~a\n" d)
;     (when (d . > . 0)
;       (sleep (/ d 1000.0)))
;     (thread-send (cadr vs) (caddr vs))
;     (set! count (+ 1 count))
;     #;(when (= 0 (modulo count 600))
;       (if (equal? 0.0 delay)
;           (set! delay 400.0)
;           (set! delay 0.0))
;       (printf "delay set to ~a\n" delay))
;     (loop))))

(define (send-to-client c v)
  (thread-send (client-out-t c) v)
    ;(async-channel-put delay-ch (list (current-milliseconds) (client-out-t c) v))
  )


(define previous-physics-time #f)

    
(define (server-loop)

  (define time-tick 0)
  (define time-commands 0)
  (define time-effects 0)
  (define time-collide 0)
  (define time-hook 0)
  (define time-ai 0)
  (define time-output 0)
  
  (define current-time (current-milliseconds))
  (when (not previous-physics-time)
    (set! previous-physics-time current-time))
  
  ; process new clients
  (when (tcp-accept-ready? server-listener)    
    (define-values (in out) (tcp-accept server-listener))
    (when (and in out)
      (set-tcp-nodelay! out #t)
    (define cid (next-id))
    (define c (client CLIENT_STATUS_NEW
                      (player cid #f #f 0 '() #f #f)
                      in out
                      (make-in-thread cid in (current-thread))
                      (make-out-thread cid out (current-thread))))
    (printf "server (~a) accepting new client ~a\n" (length clients) cid)
    (send-to-client c (serialize (client-player c)))  ; assign an id
    (prepend! clients c))
  )
  
  ; simulation tick
  (define tick? #t)
  (when ((- current-time previous-physics-time) . < . TICK)
    ;(printf "server woke up too early, no tick\n")
    (set! tick? #f))
    
  (when tick?
    (define dt (/ TICK 1000.0))
    
    ; physics
    (timeit time-tick
    (set! previous-physics-time (+ previous-physics-time TICK))
    (set-space-time! ownspace (+ (space-time ownspace) TICK))
    (for ((o (space-objects ownspace)))
      (update-physics! ownspace o dt)
      (update-stats! ownspace o dt))
    )

    ; process player commands
    (timeit time-commands
    (let loop ()
      (define v (thread-try-receive))
      (when v
        (define cid (car v))
        (define u (cdr v))
        (cond
          ((not u)
           (remove-client cid))
          ((and (update? u)
                (not (null? (update-changes u)))
                (player? (car (update-changes u))))
           (define c (findf (lambda (o) (= cid (client-id o))) clients))
           (define name (player-name (car (update-changes u))))
           (set-player-name! (client-player c) name)
           (set-client-status! c CLIENT_STATUS_WAITING_FOR_SPACE)
           (append! updates
                    (apply-all-changes! ownspace (list (chadd (client-player c) #f)) "server"))
           (define m (message (next-id) (space-time ownspace) #t #f
                              (format "New Player: ~a" name)))
           (append! updates (apply-all-changes! ownspace (list m) "server")))
          
          ((update? u)
           (cond
             ((not (equal? (space-id ownspace) (update-id u)))
              (printf "server dropping update id ~a from ~a for old space (needed ~a)\n"
                      (update-id u) cid (space-id ownspace)))
             (else
              (when (and (update-time u) ((- (space-time ownspace) (update-time u)) . > . 100))
                (printf "~a : client ~a is behind ~a\n" (space-time ownspace) cid
                        (- (space-time ownspace) (update-time u))))
              (for ((ch (in-list (update-changes u))))
                (cond
                  ((anncmd? ch)
                   (define changes
                     (scenario-on-message ownspace ch change-scenario!))
                   (append! updates (apply-all-changes! ownspace changes "server")))
                  (else
                   (define pids '())
                   (define command-changes
                     (apply-all-changes! ownspace (list ch) "server"
                                         #:on-player-restart
                                         (lambda (pid) (append! pids pid))))
                   (append! updates command-changes)
                   (when scenario-on-player-restart
                     (for ((pid (in-list pids)))
                       (define changes (scenario-on-player-restart ownspace pid))
                       (append! updates (apply-all-changes! ownspace changes "server"))))
                   ))))))
          (else
           (printf "server got unexpected data ~v\n" u)))
        (loop)))
    )

    ; update-effects! returns already-applied changes
    (timeit time-effects
    (append! updates (update-effects! ownspace dt))
    )

    (define qt (qt-new 0 0 (space-width ownspace) (space-height ownspace)))

    ; need to delay adding new things to quadtree during qt-collide!
    (define objs-added '())
    (define (addf o)
      (set! objs-added (cons o objs-added)))
    
    (timeit time-collide
    ; add everything to quadtree
    (add-all! qt (space-objects ownspace))

    (define (coll! a b)
      (when (and (obj-alive? a)
                 (obj-alive? b))
        (define precs (if ((priority a) . <= . (priority b))
                          (collide! ownspace a b dt)
                          (collide! ownspace b a dt)))
        (when (not (void? precs))
          (define cs (apply-all-changes! ownspace precs "server" #:addf addf))
          (append! updates cs))))
    
    (qt-collide! qt coll!)

    ; add all new things to the quadtree
    (add-all! qt objs-added)
    (set! objs-added '())
    )

    ; scenario hook
    (timeit time-hook
    (append! updates (apply-all-changes! ownspace
                                         (scenario-on-tick ownspace qt change-scenario!)
                                         "server" #:addf addf))
    (add-all! qt objs-added)
    (set! objs-added '())
    )

    ; ai
    (timeit time-ai
    ; run-ai! returns already-applied changes
    (append! updates (run-ai! ownspace qt))
    )
    ;(outputtime "server"
    ;             (space-time ownspace)
    ;             time-ai)

    ; cull dead
    (set-space-objects! ownspace (filter obj-alive? (space-objects ownspace)))
    
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
             (prepend! pvupdates (pvupdate (ob-id o) pv)))
            #;((or (not oldest)
                 (< (- (space-time ownspace) (posvel-t (obj-posvel oldest)))
                    (- (space-time ownspace) (posvel-t pv))))
             (set! oldest o))))
    ;(printf "\n")

    #;(when oldest
      (define old-t (- (space-time ownspace) (posvel-t (obj-posvel oldest))))
      (when (and (old-t . > . 5000)
                 (time-for (space-time ownspace) 1000))
        (printf "server oldest posvel is ~a\n" old-t))
      (set-posvel-t! (obj-posvel oldest) (space-time ownspace))
      (set! pvupdates (cons (pvupdate (ob-id oldest) (obj-posvel oldest)) pvupdates)))
  
    ; make total update message
    (define u (update (space-id ownspace) (space-time ownspace) updates pvupdates))
  
    ; reset this before trying to send, so we can accumulate
    ; client-disconnect updates if there's an error
    (set! updates '())
  
    ;(printf "~a server queuing time ~v\n" (current-milliseconds) (update-time u))
    ; to-bytes also serves to copy the info in u so it doesn't change
    ; because send-to-client is asynchronous
    (define msg (serialize u))
    (for ((c clients)
                 #:when (= (client-status c) CLIENT_STATUS_OK))
      (send-to-client c msg))
    )
    )

  ; send to any clients that need a whole ownspace
  ; - either new clients or the scenario changed
  (define msg #f)
  (for ((c clients)
               #:when (= (client-status c) CLIENT_STATUS_WAITING_FOR_SPACE))
      (when (not msg)
      (set! msg (serialize ownspace)))
      ;(printf "server sending ownspace to client ~a ~a\n"
      ;        (client-id c) (player-name (client-player c)))
    (send-to-client c msg)
    (set-client-status! c CLIENT_STATUS_OK))

  ; housekeeping
  (flush-output)
  (collect-garbage 'incremental)
  
  ; sleep so we don't hog the whole racket vm
  (define sleep-time (- (+ previous-physics-time TICK 1)
                        (current-milliseconds)))
  (cond
    ((sleep-time . > . 0)
     (sleep (/ sleep-time 1000.0)))
    (else
     (printf "~a : server sleep-time ~a num objects ~a\n"
             (space-time ownspace) sleep-time (length (space-objects ownspace)))
     (outputtime "server"
                 (space-time ownspace)
                 time-commands
                 time-tick
                 time-effects
                 time-collide
                 time-hook
                 time-ai
                 time-output)
     ))
  
  (server-loop))



(define (change-scenario! (scenario sc-pick))
  (define-values (newspace on-tick on-message on-player-restart)
    (scenario ownspace scenario-on-tick scenario-on-message scenario-on-player-restart))
  ;(printf "start ownspace ~v\n" new-space)
  (set! ownspace newspace)
  (set! scenario-on-tick on-tick)
  (set! scenario-on-message on-message)
  (set! scenario-on-player-restart on-player-restart)

  ; junk any updates we've already processed on the old space
  (set! updates '())

  (for ((c clients))
    (set-client-status! c CLIENT_STATUS_WAITING_FOR_SPACE))

  ; debugging
  (when spacebox
    (set-box! spacebox ownspace)))


(define (start-server (port PORT) #:scenario (scenario sc-pick) #:spacebox (spbox #f))
  (change-scenario! scenario)
  (set! spacebox spbox)
  (set! server-listener (tcp-listen port 100 #t))
  (printf "waiting for clients...\n")
  (server-loop))


(module+ main
  (start-server
   ;#:scenario testing-scenario
   ))
