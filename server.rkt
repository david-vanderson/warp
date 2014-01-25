#lang racket/base

(require racket/tcp
         racket/math)

(require "defs.rkt"
         "utils.rkt"
         "change.rkt"
         "physics.rkt"
         "ai.rkt")

(provide start-server)

(define server-listener #f)
(define client-in-ports '())
(define client-out-ports '())
(define ownspace #f)


;; Server Utilities


(define previous-physics-time #f)

(define (server-loop)
  (define current-time (current-milliseconds))
  (when (not previous-physics-time)
    (set! previous-physics-time current-time))
  (define need-update #f)
  
  ; process new clients
  (when (tcp-accept-ready? server-listener)
    (printf "server accept-ready\n")
    (define-values (in out) (tcp-accept server-listener))
    
    ; need to assign an id to the new player
    (write (player (next-id) "New Player") out)
    
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
    (define effect-changes (update-effects! ownspace))
    ;(printf "server effect-changes ~v\n" effect-changes)
    (set! updates (append updates effect-changes))
    (run-ai! ownspace))
  
  ; XXX run the ai, return list of changes
  ; XXX process the ai changes
  
  ; process commands
  (for ((p client-in-ports))
    (while (byte-ready? p)
      (define x (read p))
      ;(printf "server applying command ~v\n" x)
      (define changes (apply-change! ownspace x #f))
      (if changes
          (set! updates (append updates changes))
          (printf "server made no change ~v\n" x))
      (set! need-update #t)))
  
  ; find least-recently sent posvels
  (define objs
    (sort (space-objects ownspace)
          (lambda (o1 o2) (> (- (space-time ownspace) (posvel-t (obj-posvel o1)))
                             (- (space-time ownspace) (posvel-t (obj-posvel o2)))))))
  (define oldest (- (space-time ownspace) (posvel-t (obj-posvel (car objs)))))
  (when (and (oldest . > . 1000)
             (oldest . < . 10000))
    (printf "server oldest posvel is ~a\n" oldest))
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
    (big-ship "Rebel1" #f "Rebel" 0 0 0 #t #t)
    (big-ship "Empire1" #f "Empire" 100 0 (/ pi 2) #f #t))))
  
  (start-server PORT ownspace))
