#lang racket/base

(require racket/tcp
         racket/math
         racket/list
         racket/port
         ffi/unsafe)

(require "defs.rkt"
         "utils.rkt"
         "change.rkt"
         "physics.rkt"
         "pilot.rkt"
         "weapons.rkt"
         "pbolt.rkt"
         "missile.rkt"
         "plasma.rkt"
         "shield.rkt"
         "ships.rkt"
         "dmg.rkt"
         "order.rkt"
         "scenario.rkt"
         "upgrade.rkt")

(provide start-server)

(struct client (player in out) #:mutable #:prefab)
(define (client-id c) (ob-id (client-player c)))

(define server-listener #f)
(define clients '())
(define ownspace #f)
(define (scenario-on-tick change-scenario!) '())
(define (scenario-on-message cmd change-scenario!) '())



(define IPPROTO_TCP 6)
(define TCP_NODELAY 1)

(define setsockopt_tcp_nodelay
  (get-ffi-obj "setsockopt" #f
               (_fun (socket enabled?) ::
                     (socket : _int)
                     (_int = IPPROTO_TCP)
                     (_int = TCP_NODELAY)
                     (enabled-ptr : (_ptr i _int)
                                  = (if enabled? 1 0))
                     (_int = (compiler-sizeof 'int))
                     -> (result : _int)
                     -> (if (zero? result)
                            (void)
                            (error 'set-tcp-nodelay! "failed")))))

(define scheme_get_port_socket
  (get-ffi-obj "scheme_get_port_socket" #f
               (_fun (port) ::
                     (port : _racket)
                     (socket : (_ptr o _intptr))
                     -> (result : _int)
                     -> (and (positive? result) socket))))

; set-tcp-nodelay! : tcp-port boolean -> void
(define (set-tcp-nodelay! port enabled?)
  (let ([socket (scheme_get_port_socket port)])
    (setsockopt_tcp_nodelay socket enabled?)))




; return a list of changes
(define (upgrade-hit-ship! space ship u)
  (define changes '())
  (when (and (not ((ship-con ship) . <= . 0))
             ((distance ship u) . < . (+ (ship-radius ship) (upgrade-radius space u))))
    ;(printf "upgrade hit ship ~a (~a)\n" (ship-name ship) (obj-id ship))
    (define newstats (copy (ship-stats ship)))
    (define which
      (case (upgrade-type u)
        (("power") (set-stats-power! newstats (* 1.1 (stats-power newstats))) "reactor")
        (("thrust") (set-stats-thrust! newstats (* 1.1 (stats-thrust newstats))) "engines")
        (("bat") (set-stats-maxbat! newstats (* 1.1 (stats-maxbat newstats))) "reserve")
        (("con") (set-stats-maxcon! newstats (* 1.1 (stats-maxcon newstats))) "hull")
        (("radar") (set-stats-radar! newstats (* 1.1 (stats-radar newstats))) "radar")))
    (define m (message (next-id) (space-time space) #f (format "~a upgraded ~a" (ship-name ship) which)))
    
    (append! changes (list (chstats (ob-id ship) newstats) m (chrm (ob-id u)))))
  changes)


; return a list of changes
(define (plasma-hit-ship! space ship p)
  (define changes '())
  (when (and (not ((ship-con ship) . <= . 0))
             (not (plasma-dead? space p))
             (not (equal? (plasma-ownship-id p) (ob-id ship)))
             ((distance ship p) . < . (+ (ship-radius ship) (plasma-radius space p))))
    ;(printf "plasma hit ship ~a (~a ~a)\n" (ship-name ship) (plasma-ownship-id p) (obj-id ship))

    (when ((random) . > . 0.8)
      (append! changes (dmg-ship ship (distance ship p) (angle-frto (obj-r ship) (theta ship p)))))
    
    (define damage (plasma-energy space p))
    (define e (effect (next-id) (space-time space)
                      (struct-copy posvel (obj-posvel p)
                                   (dx (posvel-dx (obj-posvel ship)))
                                   (dy (posvel-dy (obj-posvel ship))))
                      (plasma-radius space p) 300))
    (append! changes (list (chdam (ob-id p) damage)
                           (chdam (ob-id ship) damage)
                           (chadd e #f))))
  changes)


; return a list of changes
(define (missile-hit-plasma! space m p)
  (define changes '())
  (when (and (not (missile-dead? space m))
             (not (plasma-dead? space p))
             ((distance m p) . < . (+ (missile-radius m) (plasma-radius space p))))
    (printf "missile hit plasma\n")

    (define damage (plasma-energy space p))
    (append! changes (chdam (ob-id m) -1)  ; missile should detonate
             (chdam (ob-id p) damage)))
  changes)


; return a list of changes
(define (missile-hit-ship! space ship m)
  (define changes '())
  (when (and (not ((ship-con ship) . <= . 0))
             (not (missile-dead? space m))
             ((distance ship m) . < . (+ (ship-radius ship) (missile-radius m))))
    (printf "missile hit ship ~a\n" (ship-name ship))

    (define damage 50.0)
    (define e (effect (next-id) (space-time space)
                      (struct-copy posvel (obj-posvel m)
                                   (dx (posvel-dx (obj-posvel ship)))
                                   (dy (posvel-dy (obj-posvel ship))))
                      10.0 500))
    (append! changes (list (chdam (ob-id ship) damage)
                           (chdam (ob-id m) damage)
                           (chadd e #f))))
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
      (define damage (min (plasma-energy space p) (shield-energy space s)))
      (append! changes (list (chdam (ob-id p) damage)
                             (chdam (ob-id s) damage)))))
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
  (define hangar (ship-hangar s2))
  (append! changes (list (chmov (ob-id s1) #f (ob-id hangar) #f)))
  (define d (find-id s1 dock? #f))
  (append! changes (list (command (ob-id d) #f)))
  (define f (find-id s1 fthrust? #f))
  (when f (append! changes (list (command (ob-id f) #f))))
  changes)

(define (pickup! ship suit)
  (define rc (chrole (ob-id (car (lounge-crew (ship-lounge suit)))) (ob-id (ship-lounge ship))))
  (define rm (chrm (ob-id suit)))
  (list rc rm))

(define (ship-hit-ship! space ship s)
  (define changes '())
  (when (and ((ship-con ship) . > . 0)  ; could have died
             ((ship-con s) . > . 0)  ; could have died
             (ship-flying? ship) (ship-flying? s)  ; could have docked
             ((distance ship s) . < . (hit-distance ship s)))
        ;(printf "ship ~a hit ship ~a\n" (ship-name ship) (ship-name s))
    (cond
      ((and (spacesuit? ship) (spacesuit? s))
       #f)
      ((spacesuit? ship)
       (when (equal? (ship-faction ship) (ship-faction s))
         (append! changes (pickup! s ship))))
      ((spacesuit? s)
       (when (equal? (ship-faction ship) (ship-faction s))
         (append! changes (pickup! ship s))))
      ((will-dock? ship s)
       (append! changes (dock! ship s)))
      ((will-dock? s ship)
       (append! changes (dock! s ship)))
      (else
       (ship-collide! ship s))))
  changes)

; return a list of final changes
(define (update-effects! space)
  (define changes '())
  (define objects (space-objects space))
  (define ships (filter ship? objects))
  
  (define spaceships (filter spaceship? ships))
  (define spacesuits (filter spacesuit? ships))
  
  (define plasmas (filter plasma? objects))
  (define shields (filter shield? objects))
  (define upgrades (filter upgrade? objects))
  (define missiles (filter missile? objects))

  (for ((m missiles))
    (when (missile-should-detonate? space m)
      (define cs (apply-all-changes!
                  space (list (chdam (ob-id m) -1)) (space-time space) "server"))
      (append! changes cs))
    (for ((p plasmas))
      (define cs (apply-all-changes!
                  space (missile-hit-plasma! space m p) (space-time space) "server"))
      (append! changes cs))
    (for ((ship spaceships))
      (define cs (apply-all-changes!
                  space (missile-hit-ship! space ship m) (space-time space) "server"))
      (append! changes cs)))
  
  (for ((p plasmas))
    (for ((shield shields))
      (define cs (apply-all-changes!
                  space (plasma-hit-shield! space shield p) (space-time space) "server"))
      (append! changes cs))
    (for ((ship spaceships))
      (define cs (apply-all-changes!
                  space (plasma-hit-ship! space ship p) (space-time space) "server"))
      (append! changes cs)))
  
  (for* ((u upgrades) (ship spaceships))
    (define cs (apply-all-changes!
                space (upgrade-hit-ship! space ship u) (space-time space) "server"))
    (append! changes cs))
  
  (let loop ((ships ships))
    (when (not (null? ships))
      (define ship (car ships))
      (for ((s (cdr ships)))
        (define cs (apply-all-changes!
                    space (ship-hit-ship! space ship s) (space-time space) "server"))
        (append! changes cs))
      (loop (cdr ships))))
  changes)


; return a list of commands
(define (run-ai! space)
  (define changes '())
  
  (define stacks (search space ai-pod? #t))

  ; if we haven't seen this pod before, set ai runtime to 0
  (for ((s stacks))
    (when (equal? #t (pod-npc? (get-pod s)))
      (set-pod-npc?! (get-pod s) 0)))

  ; sort stacks by last ai runtime
  (define sorted-stacks
    (sort stacks (lambda (s1 s2) (< (pod-npc? (get-pod s1))
                                    (pod-npc? (get-pod s2))))))

  (when (not (null? sorted-stacks))
    (define s (car sorted-stacks))  ; get first one
    (define p (get-pod s))
    ; don't run ai faster than AI_INTERVAL ms, could be slower
    (when ((- (space-time space) (pod-npc? p)) . > . AI_INTERVAL)
      (set-pod-npc?! p (space-time space))  ; set runtime

      ;(printf "running ai for pod ~a\n" (ob-id (get-pod s)))

      ; ai always fixes all the dmgs
      (for* ((t (in-list (pod-tools p)))
             (d (in-list (tool-dmgs t)))
            #:when (not (dmg-fixing? d)))
        (append! changes (list (command (ob-id d) #t))))
      
      ; run this pod's ai
      (when (and (findf fthrust? (pod-tools p)) (findf steer? (pod-tools p)))
        (append! changes (pilot-ai-strategy! space s))
        
        (when (ship-flying? (get-ship s))
          (append! changes (pilot-ai-fly! space s))))

      (when (findf pbolt? (pod-tools p))
        (append! changes (pbolt-ai! space s)))

      (when (findf shbolt? (pod-tools p))
        (append! changes (shbolt-ai! space s)))
      ))
  
  changes)


(define updates '())

(define (remove-client c msg)
  (printf "removing client ~v ~a\n" (client-player c) msg)
  (define s (find-stack ownspace (client-id c)))
  (append! updates
           (apply-all-changes! ownspace
                               (list (chrole (client-id c) #f) (chrm (client-id c)))
                               (space-time ownspace) "server"))
  
  (close-input-port (client-in c))
  (close-output-port (client-out c))
  (set! clients (remove c clients)))


(define (send-to-client c msg)
  (with-handlers ((exn:fail:network? (lambda (exn) (remove-client c "send-to-client"))))
    (define bstr (with-output-to-bytes (lambda () (write msg))))
    (define start-time (current-milliseconds))
    (let loop ((bytes-written 0))
      (cond
        (((- (current-milliseconds) start-time) . > . 500)
         (remove-client c "write-bytes-avail*"))
        ((not (= bytes-written (bytes-length bstr)))
         (define r (write-bytes-avail* bstr (client-out c) bytes-written))
         (loop (+ bytes-written r)))
        (else
         (flush-output (client-out c)))))))


(define (read-from-client c)
  (with-handlers ((exn:fail:network? (lambda (exn)
                                       (remove-client c "read-from-client")
                                       #f)))
    (read (client-in c))))


(define previous-physics-time #f)

(define (server-loop)
  (define current-time (current-milliseconds))
  (when (not previous-physics-time)
    (set! previous-physics-time current-time))
  
  ; process new clients
  (when (tcp-accept-ready? server-listener)
    (printf "server accept-ready\n")
    (define-values (in out) (tcp-accept server-listener))
    (set-tcp-nodelay! out #t)
    (define c (client (player (next-id) #f #f) in out))
    (append! clients (list c))
    (define p (car (read-from-client c)))
    (set-player-name! (client-player c) (player-name p))
    (send-to-client c (client-player c))  ; assign an id
    (send-to-client c ownspace)  ; send full state
    (append! updates
             (apply-all-changes! ownspace (list (chadd (client-player c) #f)) (space-time ownspace) "server"))
    (define m (message (next-id) (space-time ownspace) #f (format "New Player: ~a" (player-name p))))
    (append! updates (list m)))

  ; process commands
  (for ((c clients))
    (while (and (not (port-closed? (client-in c)))
                (byte-ready? (client-in c)))
      (define cmds (read-from-client c))
      (cond
        ((not cmds) #f)  ; if read-from-client fails, it returns #f
        ((eof-object? cmds)
         (remove-client c "eof"))
        (else
         (for ((m (in-list cmds)) #:when (anncmd? m))
           (scenario-on-message ownspace m change-scenario!))
         (let ((cmds (filter-not anncmd? cmds)))
           (define command-changes
             (apply-all-changes! ownspace cmds (space-time ownspace) "server"))
           (append! updates command-changes))))))
  
  ; simulation tick
  (when (TICK . < . (- current-time previous-physics-time))
    ; physics
    (set! previous-physics-time (+ previous-physics-time TICK))
    (set-space-time! ownspace (+ (space-time ownspace) TICK))
    (for ((o (space-objects ownspace)))
      (update-physics! ownspace o (/ TICK 1000.0))
      (when (ship? o) (update-energy! (/ TICK 1000.0) o 0.0)))

    ; collisions
    ; update-effects! returns already-applied changes
    (append! updates (update-effects! ownspace))

    ; ai
    (append! updates (apply-all-changes! ownspace (run-ai! ownspace)
                                         (space-time ownspace) "server"))
    )
  
  
  ; scenario hook
  (append! updates (apply-all-changes! ownspace (scenario-on-tick ownspace change-scenario!)
                                       (space-time ownspace) "server"))
  
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
  
  (when (and oldest (< 500 (- (space-time ownspace) (posvel-t (obj-posvel oldest)))))
;    (when (< 500 (- (space-time ownspace) (posvel-t (obj-posvel oldest))) 10000)
;      (printf "server oldest posvel is ~a\n" (- (space-time ownspace) (posvel-t (obj-posvel oldest)))))
    (set-posvel-t! (obj-posvel oldest) (space-time ownspace))
    (set! pvupdates (cons (pvupdate (ob-id oldest) (obj-posvel oldest)) pvupdates)))
  
  ; make total update message
  (define u (update (space-time ownspace) updates pvupdates))
  
  ; reset this before trying to send, so we can accumulate
  ; client-disconnect updates if there's an error
  (set! updates '())
  
  ;(printf "server sending ~v\n" u)
  (for ((c clients))
    (send-to-client c u))
  
  ; sleep so we don't hog the whole racket vm
  (define sleep-time (- (+ previous-physics-time TICK 1)
                        (current-milliseconds)))
  (if (sleep-time . > . 0)
      (sleep (/ sleep-time 1000.0))
      (printf "server skipping sleep ~a\n" sleep-time))
  
  (server-loop))



(define (change-scenario! (scenario sc-pick))
  (define-values (newspace on-tick on-message) (scenario ownspace scenario-on-tick scenario-on-message))
  ;(printf "start ownspace ~v\n" new-space)
  (set! ownspace newspace)
  (set! scenario-on-tick on-tick)
  (set! scenario-on-message on-message)
  (for ((c clients))
    (send-to-client c ownspace)))


(define (start-server port (scenario sc-pick))
  (change-scenario! scenario)
  (set! server-listener (tcp-listen port 4 #t))
  (printf "waiting for clients...\n")
  (server-loop))


(module+ main
  (start-server PORT))