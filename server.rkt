#lang racket/base

(require racket/tcp
         racket/math
         racket/port
         ffi/unsafe)

(require "defs.rkt"
         "utils.rkt"
         "change.rkt"
         "physics.rkt"
         "pilot.rkt"
         "weapons.rkt"
         "tactics.rkt"
         "plasma.rkt"
         "shield.rkt"
         "ships.rkt")

(provide start-server)

(struct client (id in out) #:prefab)

(define server-listener #f)
(define clients '())
(define ownspace #f)



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
(define (plasma-hit-ship! space ship p)
  (define changes '())
  (when (and (not ((ship-con ship) . <= . 0))
             (not (plasma-dead? space p)))
    (when (and (not (equal? (plasma-ownship-id p) (ob-id ship)))
               ((distance ship p) . < . (+ (ship-radius ship) (plasma-radius space p))))
      ;(printf "plasma hit ship ~a (~a ~a)\n" (ship-name ship) (plasma-ownship-id p) (obj-id ship))
      (define damage (plasma-energy space p))
      (define e (effect (next-id) (space-time space)
                        (struct-copy posvel (obj-posvel p)
                                     (dx (posvel-dx (obj-posvel ship)))
                                     (dy (posvel-dy (obj-posvel ship))))
                        (plasma-radius space p) 300))
      (set! changes (append changes (list (chdam (ob-id p) damage)
                                          (chdam (ob-id ship) damage)
                                          (chadd e))))))
  changes)


; return a list of changes
(define (plasma-hit-shield! space shield p)
  (define changes '())
  (when (and (not (shield-dead? space shield))
             (not (plasma-dead? space p)))
    (define r (posvel-r (obj-posvel shield)))
    (define-values (px py) (recenter shield p))
    (define x (+ (* px (cos r)) (* py (sin r))))
    (define y (+ (* -1 px (sin r)) (* py (cos r))))
    ; x,y are now the position of the plasma in the coord space of the shield
    ; shield is along the y axis
    (define rad (plasma-radius space p))
    (define l (shield-length shield))
    (when (and (< (- rad) x rad)
               (< (- (- (/ l 2)) rad) y (+ (/ l 2) rad)))
      (define damage (plasma-energy space p))
      (set! changes (append changes (list (chdam (ob-id p) damage)
                                          (chdam (ob-id shield) damage))))))
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
  (define hangar (get-hangar s2))
  (define pilot (copy (ship-pilot s1)))
  (set-pilot-dock! pilot #f)
  (list (chmov (ob-id s1) #f (ob-id hangar) #f) pilot))


(define (ship-hit-ship! space ship s)
  (define changes '())
  (when (and ((ship-con ship) . > . 0)  ; could have died
             ((ship-con s) . > . 0)  ; could have died
             (obj-posvel ship) (obj-posvel s)  ; could have docked
             ((distance ship s) . < . (hit-distance ship s)))
        ;(printf "ship ~a hit ship ~a\n" (ship-name ship) (ship-name s))
    (cond
      ((and (spacesuit? ship) (spacesuit? s))
       #f)
      ((spacesuit? ship)
       (when (equal? (ship-faction ship) (ship-faction s))
         (define role (car (multipod-roles (car (ship-pods ship)))))
         (define rc (role-change (role-player role) (ob-id role) (ob-id (ship-crew s)) (next-id)))
         (set! changes (append changes (list rc)))))
      ((spacesuit? s)
       (when (equal? (ship-faction ship) (ship-faction s))
         (define role (car (multipod-roles (car (ship-pods s)))))
         (define rc (role-change (role-player role) (ob-id role) (ob-id (ship-crew ship)) (next-id)))
         (set! changes (append changes (list rc)))))
      ((will-dock? ship s)
       (set! changes (append changes (dock! ship s))))
      ((will-dock? s ship)
       (set! changes (append changes (dock! s ship))))
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
  
  (for ((p plasmas))
    (for ((shield shields))
      (define cs (apply-all-changes!
                  space (plasma-hit-shield! space shield p) (space-time space) "server"))
      (set! changes (append changes cs)))
    (for ((ship spaceships))
      (define cs (apply-all-changes!
                  space (plasma-hit-ship! space ship p) (space-time space) "server"))
      (set! changes (append changes cs))))
  
  (let loop ((ships ships))
    (when (not (null? ships))
      (define ship (car ships))
      (for ((s (cdr ships)))
        (define cs (apply-all-changes!
                    space (ship-hit-ship! space ship s) (space-time space) "server"))
        (set! changes (append changes cs)))
      (loop (cdr ships))))
  changes)


; return a list of commands
(define (run-ai! space dt)
  (define commands '())
  (define (airole? o)
    (and (role? o)
         (role-npc? o)
         (not (role-player o))))
  
  (define airolestacks (search space airole? #t))
  (for ((s airolestacks))
    (define r (get-role s))
    ;(printf "role ~v\n" r)
    (cond
      ((weapons? r)
       (set! commands (append commands (weapons-ai! space dt s))))
      ((tactics? r)
       (set! commands (append commands (tactics-ai! space dt s))))))
  
  commands)


; return a list of pilot stacks to run
(define (get-pilot-ais space)
  (define (ai-pilot-role? o)
    (and (pilot? o)
         (role-npc? o)
         (not (role-player o))))
  
  (define pilot-stacks (search space ai-pilot-role? #t))
  
  (for ((s pilot-stacks))
    (when (equal? #t (role-npc? (get-role s)))
      (set-role-npc?! (get-role s) 0)))
  
  (define sorted-stacks
    (sort pilot-stacks
          (lambda (s1 s2) (< (role-npc? (get-role s1))
                             (role-npc? (get-role s2))))))
  
  (define ret '())
  
  (when (not (null? sorted-stacks))
    (for ((s (in-value (car sorted-stacks))))
      (set-role-npc?! (get-role s) (space-time space))
      (set! ret (append ret (list s)))))
  
  ret)


(define updates '())

(define (remove-client c msg)
  (define s (find-stack ownspace (client-id c)))
  (define p (if s (car s) #f))
  (define r (if s (get-role s) #f))
  (printf "removing client ~a ~a ~a\n" (client-id c) (if p (player-name p) "Unknown") msg)
  (when s
    (define changes
      (apply-all-changes! ownspace
                          (list (role-change p (ob-id r) #f -1))
                          (space-time ownspace) "server"))
    (set! updates (append updates changes)))
  (set! clients (remove c clients (lambda (x y) (= (client-id x) (client-id y))))))


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
                                       eof)))
    (read (client-in c))))


(define previous-physics-time #f)
(define previous-ai-time #f)
(define previous-pilot-ai-time #f)

(define (server-loop)
  (define current-time (current-milliseconds))
  (when (not previous-physics-time)
    (set! previous-physics-time current-time)
    (set! previous-ai-time current-time)
    (set! previous-pilot-ai-time current-time))
  
  ; process new clients
  (when (tcp-accept-ready? server-listener)
    (printf "server accept-ready\n")
    (define-values (in out) (tcp-accept server-listener))
    (set-tcp-nodelay! out #t)
    (define c (client (next-id) in out))
    (set! clients (append clients (list c)))
    (send-to-client c (player (client-id c) "New Player"))  ; assign an id
    (send-to-client c ownspace))  ; send full state
  
  ; physics
  (when (TICK . < . (- current-time previous-physics-time))
    (set! previous-physics-time (+ previous-physics-time TICK))
    (set-space-time! ownspace (+ (space-time ownspace) TICK))
    (for ((o (space-objects ownspace)))
      (update-physics! ownspace o (/ TICK 1000.0))
      (when (ship? o) (update-energy! (/ TICK 1000.0) o 0.0)))
    
    ; update-effects! returns already-applied changes
    (set! updates (append updates (update-effects! ownspace))))
  
  (when (AI_TICK . < . (- current-time previous-ai-time))
    (set! previous-ai-time (+ previous-ai-time AI_TICK))
    (define commands (run-ai! ownspace (/ AI_TICK 1000.0)))
    (define command-changes
      (apply-all-changes! ownspace commands (space-time ownspace) "server"))
    (set! updates (append updates command-changes))
    
    (for ((s (get-pilot-ais ownspace)))
      (define xs (pilot-ai-strategy! ownspace s))
      (define cs (apply-all-changes! ownspace xs (space-time ownspace) "server"))
      (set! updates (append updates cs))
      
      (when (ship-flying? (get-ship s))
        (define xs2 (pilot-ai! ownspace s))
        (define cs2 (apply-all-changes! ownspace xs2 (space-time ownspace) "server"))
        (set! updates (append updates cs2)))))
  
  
  ; process commands
  (for ((c clients))
    (while (and (not (port-closed? (client-in c)))
                (byte-ready? (client-in c)))
      (define cmds (read-from-client c))
      (cond
        ((eof-object? cmds)
         (close-input-port (client-in c))
         (remove-client c "eof"))
        (else
         (change-ids! cmds)
         (define command-changes
           (apply-all-changes! ownspace cmds (space-time ownspace) "server"))
         (set! updates (append updates command-changes))))))
  
  
  ; scenario hook
  (set! updates (append updates (apply-all-changes! ownspace
                                                    (scenario-hook ownspace)
                                                    (space-time ownspace) "server")))
  
  
  ; find least-recently sent posvels
  (define objs
    (sort (filter ship? (space-objects ownspace))
          (lambda (o1 o2) (> (- (space-time ownspace) (posvel-t (obj-posvel o1)))
                             (- (space-time ownspace) (posvel-t (obj-posvel o2)))))))
  (when (not (null? objs))
    (define oldest (- (space-time ownspace) (posvel-t (obj-posvel (car objs)))))
    (when (and (oldest . > . 500)
               (not (oldest . > . 10000)))
      (printf "server oldest posvel is ~a\n" oldest)
      )
    )
  (define pvupdates
    (for/list ((o objs) (i 10))
      (define pv (obj-posvel o))
      (set-posvel-t! pv (space-time ownspace))
      (pvupdate (ob-id o) pv)))
  
  ; make total update message
  (define u (update (space-time ownspace) updates pvupdates))
  
  ; reset this before trying to send, so we can accumulate
  ; client-disconnect updates if there's an error
  (set! updates '())
  
  (for ((c clients))
    ;(printf "server sending ~v\n" u)
    (send-to-client c u))
  
  ; sleep so we don't hog the whole racket vm
  (define sleep-time (- (+ previous-physics-time TICK 1)
                        (current-milliseconds)))
  (if (sleep-time . > . 0)
      (sleep (/ sleep-time 1000.0))
      (printf "server skipping sleep ~a\n" sleep-time))
  
  (server-loop))


(define (start-server port new-space)
  (set! ownspace new-space)
  (set! server-listener (tcp-listen port 4 #t))
  (server-loop))


; return a list of changes
(define (scenario-hook space)
  (define commands '())
  
;  (when (<= 1 (modulo (space-time space) 1800) TICK)
;    (set! commands (append commands (list (message (next-id) (space-time space) #f
;                                                   (format "Time ~a" (space-time space)))))))
  
  (define types (map ship-type (filter ship? (space-objects space))))
  ;(printf "types ~v\n" types)
  (define (count-type type)
    (length (filter (lambda (t) (equal? t type)) types)))
  
  (define theta (random-between 0 2pi))
  (define r (random-between 0 500))
  (define x (* r (cos theta)))
  (define y (* r (sin theta)))
  
  (cond
    (((count-type "blue-frigate") . < . 1)
     (define s (make-ship "blue-frigate" "Blue Frigate" "Rebel" #:npc? #t #:x x #:y y #:r (angle-add theta pi) #:start-ship? #t
                 #:in-hangar (list
                              (make-ship "blue-fighter" "Blue Fighter" "Rebel" #:npc? #t)
                              (make-ship "blue-fighter" "Blue Fighter" "Rebel" #:npc? #t))))
     (set! commands (append commands (list (chadd s)))))
    (((count-type "red-frigate") . < . 1)
     (define s (make-ship "red-frigate" "Red Frigate" "Empire" #:npc? #t #:x x #:y y #:r (angle-add theta pi) #:start-ship? #t
                 #:in-hangar (list
                              (make-ship "red-fighter" "Red Fighter" "Empire" #:npc? #t)
                              (make-ship "red-fighter" "Red Fighter" "Empire" #:npc? #t))))
     (set! commands (append commands (list (chadd s))))))
;    (((count-type "blue-fighter") . < . 2)
;     (define s (make-ship "blue-fighter" "Blue Fighter" "Rebel" #:start-ship? #t #:npc? #t #:x x #:y y #:r (angle-add theta pi)))
;     (set! commands (append commands (list (chadd s)))))
;    (((count-type "red-fighter") . < . 2)
;     (define s (make-ship "red-fighter" "Red Fighter" "Empire" #:npc? #t #:x x #:y y #:r (angle-add theta pi)))
;     (set! commands (append commands (list (chadd s))))))
  
  commands)


(module+ main
  
  (define ownspace
    (space
     0 2000 2000
     (list
      
;      (make-ship "red-frigate" "Empire1" "Empire" #:npc? #t #:x 500 #:y 0 #:r pi)
;      (make-ship "red-fighter" "Red 1" "Empire" #:npc? #t #:x 500 #:y 100 #:r pi)
;      (make-ship "red-fighter" "Red 2" "Empire" #:npc? #t #:x 500 #:y -100 #:r pi)
;      
;      (make-ship "blue-fighter" "Blue 5" "Rebel" #:start-ship? #t #:npc? #t #:x -400 #:y 100)
;      (make-ship "blue-fighter" "Blue 6" "Rebel" #:start-ship? #t #:npc? #t #:x -400 #:y -100)
;      (make-ship "blue-frigate" "Rebel1" "Rebel" #:npc? #t #:x -400 #:y 0 #:start-ship? #t
;                 #:in-hangar (list
;                              (make-ship "blue-fighter" "Blue 1" "Rebel" #:npc? #t #:posvel? #f)
;                              (make-ship "blue-fighter" "Blue 2" "Rebel" #:npc? #t #:posvel? #f)))
      
      
      ;    (make-ship "blue-fighter" "RF 1" "Rebel" #:npc? #t #:x -300 #:y 50 #:start-ship? #t)
      ;    (make-ship "blue-frigate" "Rebel1" "Rebel" #:npc? #t #:x -200 #:y 100)
      ;    (make-ship "blue-frigate" "Rebel1" "Rebel" #:npc? #t #:x -200 #:y 200)
      ;    (make-ship "red-frigate" "Empire1" "Empire" #:npc? #t #:x 200 #:y 100 #:r pi)
      ;    (make-ship "red-frigate" "Empire1" "Empire" #:npc? #t #:x 200 #:y 300 #:r pi)
      ;    (make-ship "blue-fighter" "RF 2" "Rebel" #:npc? #t #:x -300 #:y 150)
      ;    (make-ship "red-fighter" "EF 1" "Empire" #:npc? #t #:x 300 #:y 50 #:r pi)
      ;    (make-ship "red-fighter" "EF 2" "Empire" #:npc? #t #:x 200 #:y 150 #:r pi)
      ;    (make-ship "blue-fighter" "EF 3" "Empire" #:npc? #t #:x 200 #:y 75)
      ;(make-ship "blue-fighter" "Red 5" "Rebel" #:start-ship? #t #:npc? #t #:x 100 #:y 20 #:r pi #:dx -20)
      ;(big-ship "Empire1" "Empire" 400 0 pi/2 #f #t #f #t #t #t)
      ;(big-ship "Empire2" "Empire" 600 0 (- pi/2) #f #t #f #t #t #t)
      )))
  
  (start-server PORT ownspace))
