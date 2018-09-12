#lang racket/base

(require racket/tcp
         racket/async-channel)

(require "defs.rkt"
         "utils.rkt"
         "change.rkt"
         "physics.rkt"
         "quadtree.rkt"
         "ships.rkt"
         "pilot.rkt"
         "warp.rkt"
         "plasma.rkt"
         "explosion.rkt"
         "pbolt.rkt"
         "missile.rkt"
         "cannon.rkt"
         "shield.rkt"
         "scenario.rkt"
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


; return a list of already applied updates
(define (run-ai! space qt)
  (define updates '())
  
  (define stacks (search space space ai-ship? #t))
  (define delay 0)

  ; if we haven't seen this ship before, set ai runtime to 0
  (for ((s (in-list stacks)))
    (define changes '())
    (define ship (car s))
    
    (when (and (obj-alive? ship)
               ((- (space-time space) (ship-ai-time ship)) . > . (ship-ai-freq ship)))
      (set-ship-ai-time! ship (+ (space-time space) delay))  ; set runtime
      (set! delay (+ TICK delay))  ; push ais away from running in the same tick

      ;(printf "running ai for ship ~a\n" (ship-name ship))
      
      ; run this ship's ai
      (when (and (ship-tool ship 'engine)
                 (ship-tool ship 'steer))
        (when (not (missile? ship))
          (append! changes (pilot-ai-strategy! space qt s)))
        (when (and (ship-flying? ship)
                   (or (missile? ship)
                       (ship-strategy ship)))
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
      (add-to-qt! ownspace qt o))
    (append! updates (apply-all-changes! ownspace changes "server" #:addf addf)))

  ;(printf "ran ~a ai\n" (/ delay TICK))
  
  updates)


(define updates '())
     
(define (remove-client cid)
  (define c (findf (lambda (o) (= cid (client-id o))) clients))
  (when c
    (define name (player-name (client-player c)))
    (printf "server (~a) removing client ~a ~a\n" (length clients) (client-id c) name)
    (define m (make-message ownspace (format "Player Left: ~a" name)))
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
                      ; send version in the player name field
                      (player cid VERSION #f 0 '() #f #f)
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

    (define qt #f)

    ; physics
    (timeit time-tick
    (set! previous-physics-time (+ previous-physics-time TICK))
    (define-values (qt2 updates2)
      (tick-space! ownspace apply-all-changes!))
    (set! qt qt2)
    (append! updates updates2)
    )

    (define (addf o)
      (add-to-qt! ownspace qt o))

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
           (define p (car (update-changes u)))
           (define name (player-name p))
           (define c (findf (lambda (o) (= cid (client-id o))) clients))
           (cond
             ((not (equal? VERSION (ob-id p)))
              (printf "server version ~a dropping client id ~a version ~a : ~a\n"
                      VERSION cid (ob-id p) name)
              (remove-client cid))
             (else
              (set-player-name! (client-player c) name)
              (printf "server client ~a named ~a\n" cid name)
              (set-client-status! c CLIENT_STATUS_WAITING_FOR_SPACE)
              (append! updates
                       (apply-all-changes! ownspace (list (chadd (client-player c) #f))
                                           "server" #:addf addf))
              (define m (make-message ownspace (format "New Player: ~a" name)))
              (append! updates (apply-all-changes! ownspace (list m) "server" #:addf addf)))))
          
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
                   (append! updates (apply-all-changes! ownspace changes "server" #:addf addf)))
                  (else
                   (define pids '())
                   (define command-changes
                     (apply-all-changes! ownspace (list ch) "server"
                                         #:addf addf
                                         #:on-player-restart
                                         (lambda (pid) (append! pids pid))))
                   (append! updates command-changes)
                   (when scenario-on-player-restart
                     (for ((pid (in-list pids)))
                       (define changes (scenario-on-player-restart ownspace pid))
                       (append! updates (apply-all-changes! ownspace changes
                                                            "server" #:addf addf))))
                   ))))))
          (else
           (printf "server got unexpected data ~v\n" u)))
        (loop)))
    )

    (timeit time-effects
    (for ((o (space-objects ownspace))
          #:when (obj-alive? o))
      (define precs (upkeep! ownspace o))
      (define cs (apply-all-changes! ownspace precs "server" #:addf addf))
      (append! updates cs))

    ; find out if any player's rc objects went away
    (for ((p (space-players ownspace))
          #:when (and (player-rcid p)
                      (not (find-id ownspace ownspace (player-rcid p)))))
      (define cs (apply-all-changes! ownspace (list (endrc (ob-id p) #f)) "server" #:addf addf))
      (append! updates cs))
    )

    ; scenario hook
    (timeit time-hook
    (define ups (scenario-on-tick ownspace qt change-scenario!))
    (append! updates (apply-all-changes! ownspace ups "server" #:addf addf))
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
          #:when (and (obj? o)
                      (obj-posvel o)
                      (posvel-t (obj-posvel o))))
      (define pv (obj-posvel o))
      ;(printf " ~a" (posvel-t pv))
      (cond ((equal? #t (posvel-t pv))
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
  (load-ships!)
  (change-scenario! scenario)
  (set! spacebox spbox)
  (set! server-listener (tcp-listen port 100 #t))
  (printf "waiting for clients...\n")
  (server-loop))


(module+ main
  (start-server
   ;#:scenario testing-scenario
   ))
