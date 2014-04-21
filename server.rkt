#lang racket/base

(require racket/tcp
         racket/math
         racket/port)

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


; return a list of changes
(define (plasma-hit-ship! space ship p)
  (define changes '())
  (when (and (not ((ship-containment ship) . <= . 0))
             (not (plasma-dead? space p)))
    (when (and (not (equal? (plasma-ownship-id p) (ob-id ship)))
               ((distance ship p) . < . (+ (stats-radius (ship-stats ship)) (plasma-radius space p))))
      ;(printf "plasma hit ship ~a (~a ~a)\n" (ship-name ship) (plasma-ownship-id p) (obj-id ship))
      (define damage (plasma-energy space p))
      (define e (effect (next-id) (space-time space)
                        (struct-copy posvel (obj-posvel p)
                                     (dx (posvel-dx (obj-posvel ship)))
                                     (dy (posvel-dy (obj-posvel ship))))
                        6 300))
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
  
  (define dt (/ (- (+ (stats-radius (ship-stats s1))
                      (stats-radius (ship-stats s2)))
                   (distance s1 s2))
                dv))
  
  (when (dt . > . (/ TICK 1000.0))
    ;(printf "dv ~a dt ~a\n" dv (- dt (/ TICK 1000.0)))
    (physics! (obj-posvel s1) (- dt (/ TICK 1000.0)))
    (physics! (obj-posvel s2) (- dt (/ TICK 1000.0))))
  
  ; make sure we send the new posvels right away
  (set-posvel-t! (obj-posvel s1) 0)
  (set-posvel-t! (obj-posvel s2) 0))


; return a list of changes
(define (dock! s1 s2)
  (define hangar (get-hangar s2))
  (define pilot (copy-role (ship-pilot s1)))
  (set-pilot-dock! pilot #f)
  (list (chmov (ob-id s1) #f (ob-id hangar) #f) pilot))


(define (ship-hit-ship! space ship s)
  (define changes '())
  (when (and (not ((ship-containment ship) . <= . 0))  ; could have died
             (not ((ship-containment s) . <= . 0))  ; could have died
             (obj-posvel ship) (obj-posvel s)  ; could have docked
             ((distance ship s) . < . (+ (stats-radius (ship-stats ship))
                                         (stats-radius (ship-stats s)))))
;    (printf "ship ~a (vx ~a) hit ship ~a (vx ~a)\n"
;            (ship-name ship) (posvel-dx (obj-posvel ship))
;            (ship-name s) (posvel-dx (obj-posvel s)))
    (cond
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
  
  (define plasmas (filter plasma? objects))
  (define shields (filter shield? objects))
  
  (for ((p plasmas))
    (for ((shield shields))
      (define cs (apply-all-changes!
                  space (plasma-hit-shield! space shield p) (space-time space) "server"))
      (set! changes (append changes cs)))
    (for ((ship ships))
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
  
  (set! commands (append commands (run-pilot-ai! space)))
  
  commands)


; return a list of commands
(define (run-pilot-ai! space)
  (define commands '())
  (define (ai-pilot-role? o)
    (and (pilot? o)
         (role-npc? o)
         (not (role-player o))))
  
  (define pilot-stacks (filter (lambda (s) (ship-flying? (get-ship s)))
                               (search space ai-pilot-role? #t)))
  
  (for ((s pilot-stacks))
    (when (equal? #t (role-npc? (get-role s)))
      (set-role-npc?! (get-role s) 0)))
  
  (define sorted-stacks
    (sort pilot-stacks
          (lambda (s1 s2) (< (role-npc? (get-role s1))
                             (role-npc? (get-role s2))))))
  
  (when (not (null? sorted-stacks))
    (for ((s (in-value (car sorted-stacks))))
      (set-role-npc?! (get-role s) (space-time space))
      (set! commands (append commands (pilot-ai-strategy! space s)))
      (set! commands (append commands (pilot-ai! space s)))))
  
  commands)


(define updates '())

(define (remove-client c msg)
  (define s (find-stack ownspace (client-id c)))
  (define p (if s (car s) #f))
  (define r (if s (get-role s) #f))
  (printf "removing client ~a ~a ~a\n" (client-id c) (if p (player-name p) "Unknown") msg)
  (when s
    (define changes
      (apply-all-changes! ownspace
                          (list (role-change p (ob-id r) #f))
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
         (loop (+ bytes-written r)))))))

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
    (set! updates (append updates command-changes)))
  
  
  ; process commands
  (for ((c clients))
    (while (and (not (port-closed? (client-in c)))
                (byte-ready? (client-in c)))
      (define x (read-from-client c))
      (cond
        ((eof-object? x)
         (close-input-port (client-in c))
         (remove-client c "eof"))
        (else
         (define commands (list x))
         (define command-changes
           (apply-all-changes! ownspace commands (space-time ownspace) "server"))
         (set! updates (append updates command-changes))))))
  
  
  ; find least-recently sent posvels
  (define objs
    (sort (space-objects ownspace)
          (lambda (o1 o2) (> (- (space-time ownspace) (posvel-t (obj-posvel o1)))
                             (- (space-time ownspace) (posvel-t (obj-posvel o2)))))))
  (when (not (null? objs))
    (define oldest (- (space-time ownspace) (posvel-t (obj-posvel (car objs)))))
    (when (and (oldest . > . 1000))
      (printf "server oldest posvel is ~a\n" oldest)))
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




;(module+ main
;  
;  (define ownspace
;    (space
;     0 4000 4000
;     (list
;      (make-ship "blue-frigate" "Rebel1" "Rebel" #:npc? #t #:start-ship? #t)
;      (make-ship "blue-frigate" "Empire1" "Empire" #:npc? #t #:start-ship? #t #:x 200 #:y 300 #:r pi)
;      (make-ship "blue-frigate" "Empire2" "Empire" #:npc? #t #:start-ship? #t #:x 300 #:y 200 #:r pi)
;      #;(make-ship "blue-frigate" "Rebel1" "Rebel" #:npc? #t #:start-ship? #t)
;      #;(make-ship "blue-fighter" "RF 1" "Rebel" #:npc? #t #:x 0 #:y 80)
;      #;(make-ship "blue-fighter" "RF 2" "Rebel" #:npc? #t #:x 0 #:y -30)
;      #;(make-ship "blue-fighter" "EF 1" "Empire" #:npc? #t #:x 100 #:y 0)
;      #;(make-ship "blue-fighter" "EF 2" "Empire" #:npc? #t #:x 100 #:y 50)
;      #;(make-ship "blue-fighter" "EF 3" "Empire" #:npc? #t #:x 100 #:y -50)
;      #;(make-ship "blue-fighter" "Red 5" "Rebel" #:start-ship? #t #:npc? #t
;                   #:x 100 #:y 20 #:r pi #:dx -20)
;      #;(big-ship "Empire1" "Empire" 400 0 pi/2 #f #t #f #t #t #t)
;      #;(big-ship "Empire2" "Empire" 600 0 (- pi/2) #f #t #f #t #t #t))))
;  
;  (start-server PORT ownspace))
