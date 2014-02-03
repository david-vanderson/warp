#lang racket/base

(require racket/tcp
         racket/math)

(require "defs.rkt"
         "utils.rkt"
         "change.rkt"
         "physics.rkt"
         "pilot.rkt"
         "weapons.rkt"
         "tactics.rkt"
         "plasma.rkt"
         "shield.rkt")

(provide start-server)

(define server-listener #f)
(define client-in-ports '())
(define client-out-ports '())
(define ownspace #f)


; return a list of changes
(define (plasma-hit-ship! space ship p)
  (define changes '())
  (when (and (not ((ship-containment ship) . <= . 0))
             (not (plasma-dead? space p)))
    (when (and (not (equal? (plasma-ownship-id p) (ob-id ship)))
               ((distance ship p) . < . (+ 10 (plasma-radius space p))))
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


; return a list of final changes
(define (effects! space)
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
  changes)


; return a list of commands
(define (run-ai! space)
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
      ((pilot? r)
       (set! commands (append commands (pilot-ai! space s))))
      ((weapons? r)
       (set! commands (append commands (weapons-ai! space s))))
      ((tactics? r)
       (set! commands (append commands (tactics-ai! space s))))))
  commands)


(define previous-physics-time #f)

(define (server-loop)
  (define current-time (current-milliseconds))
  (when (not previous-physics-time)
    (set! previous-physics-time current-time))
  
  ; process new clients
  (when (tcp-accept-ready? server-listener)
    (printf "server accept-ready\n")
    (define-values (in out) (tcp-accept server-listener))
    
    ; need to assign an id to the new player
    (write (player (next-id) "New Player") out)
    (flush-output out)
    
    (set! client-in-ports (cons in client-in-ports))
    (set! client-out-ports (cons out client-out-ports))
    
    ; send full state to new client
    (write ownspace out)
    (flush-output out))
  
  (define updates '())
  
  ; physics
  (when (TICK . < . (- current-time previous-physics-time))
    (set! previous-physics-time (+ previous-physics-time TICK))
    (set-space-time! ownspace (+ (space-time ownspace) TICK))
    (for ((o (space-objects ownspace))) (update-physics! ownspace o (/ TICK 1000.0)))
    
    ; update-effects! returns already-applied changes
    (set! updates (append updates (effects! ownspace)))
    
    (define commands (run-ai! ownspace))
    (define command-changes
      (apply-all-changes! ownspace commands (space-time ownspace) "server"))
    (set! updates (append updates command-changes)))
  
  
  ; process commands
  (for ((p client-in-ports))
    (while (byte-ready? p)
      (define x (read p))
      ;(printf "server applying command ~v\n" x)
      (define commands (list x))
      (define command-changes
        (apply-all-changes! ownspace commands (space-time ownspace) "server"))
      (set! updates (append updates command-changes))))
  
  
  ; find least-recently sent posvels
  (define objs
    (sort (space-objects ownspace)
          (lambda (o1 o2) (> (- (space-time ownspace) (posvel-t (obj-posvel o1)))
                             (- (space-time ownspace) (posvel-t (obj-posvel o2)))))))
  (when (not (null? objs))
    (define oldest (- (space-time ownspace) (posvel-t (obj-posvel (car objs)))))
    (when (and (oldest . > . 1000)
               (oldest . < . 10000))
      (printf "server oldest posvel is ~a\n" oldest)))
  (define pvupdates
    (for/list ((o objs) (i 10))
      (define pv (obj-posvel o))
      (set-posvel-t! pv (space-time ownspace))
      (pvupdate (ob-id o) pv)))
  
  
  ; send out updates
  (define u (update (space-time ownspace) updates pvupdates))
  (for ((p client-out-ports))
    ;(printf "server sending ~v\n" u)
    (write u p)
    (flush-output p))
  
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

(module+ main
  
  (define ownspace
    (space
     0 4000 4000
     (list
      (big-ship "Rebel1" "Rebel" 0 0 0 #f #t #f #t #t #f)
      (big-ship "Empire1" "Empire" 500 500 0 #f #t #f #t #t #f))))
  
  (start-server PORT ownspace))
