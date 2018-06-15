#lang racket/base

(require racket/math
         racket/port
         racket/list
         racket/struct
         ffi/unsafe
         racket/draw)

(require "defs.rkt"
         "quadtree.rkt")

(provide (all-defined-out))

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


(define (make-in-thread id in-port)
  (define orig-thread (current-thread))
  (thread
   (lambda ()
     (let loop ()
       ; read from in-port
       (define v
         (with-handlers ((exn:fail:network? (lambda (exn) #f)))
           (read in-port)))
       (cond
         ((and v (not (eof-object? v)))
          ; send to client/server thread
          (thread-send orig-thread (cons id v))
          (loop))
         (else
          (thread-send orig-thread (cons id #f))
          (printf "in-thread ~a stopping ~v\n" id v)
          (sync never-evt)))))))

(define (make-out-thread id out-port)
  (define orig-thread (current-thread))
  (thread
   (lambda ()
     (let loop ()
       ; get next thing from the client/server thread
       (define v (thread-receive))
       ; send it out
       (define ret
         (with-handlers ((exn:fail:network? (lambda (exn) #f)))
           (write v out-port)
           (flush-output out-port)))
       (cond
         ((void? ret)
          (loop))
         (else
          (thread-send orig-thread (cons id #f))
          (printf "out-thread ~a stopping\n" id)
          (sync never-evt)))))))

(define-syntax-rule (timeit var e ...)
  (begin
    (define t (current-milliseconds))
    e ...
    (set! var (- (current-milliseconds) t))))

(define-syntax-rule (outputtime who time v ...)
  (begin
    (when (v . > . 1)
      (printf "~a : ~a ~a ~a\n" time who 'v v)) ...))

(define (standard-quit-scenario-button (tab? #f))
  (ann-button (next-id) 0 #t (posvel 0 60 100 0 120 50 0) tab? "Quit Scenario" "quit-scenario"))

(define-syntax-rule (append! lst e ...)
  (set! lst (append lst (flatten (list e ...)))))

(define-syntax-rule (prepend! lst e ...)
  (set! lst (append (flatten (list e ...)) lst)))

(define (remove-id id list)
  (filter (lambda (o) (not (equal? (ob-id o) id)))
          list))

(define (findfid id list)
  (findf (lambda (o) (equal? id (ob-id o))) list))

(define (copy s)
  (cond
    ((struct? s)
     (apply make-prefab-struct (prefab-struct-key s) (map copy (struct->list s))))
    ((cons? s)
     (cons (copy (car s)) (copy (cdr s))))
    ((list? s)
     (map copy s))
    ((or (number? s)
         (boolean? s)
         (string? s)
         (symbol? s))
     s)
    (else
     (error "copy unknown datatype:\n" s))))

(define (load-bitmap name)
  (read-bitmap (string-append "images/" name ".png")))

(define (obj-age space o)
  (- (space-time space) (obj-start-time o)))

; should only be used for things called every TICK
(define (time-for age repeat (offset 0))
  (<= (+ 1 offset) (modulo age repeat) (+ offset TICK)))

(define (time-toggle age repeat (div 2) (offset 0))
  (<= (modulo (+ age offset) repeat) (/ repeat div)))

(define (faction-check my-faction other-faction)
  (cond ((equal? my-faction other-faction) 1)
        ((or (equal? "_neutral" my-faction)
             (equal? "_neutral" other-faction)) 0)
        (else -1)))

(define (strategy-age space s)
  (- (space-time space) (strategy-t s)))

(define (current-strat-age space ship)
  (- (space-time space) (ship-ai-strat-time ship)))

; age, life, death are ms since object start time
(define (linear-fade age life death)
  (cond ((age . <= . life) 1.0)
        ((age . > . death) 0.0)
        (else (/ (- death age)
                 (- death life)))))

(define (clamp zmin zmax z)
  (max zmin (min zmax z)))

(define (remain a b)
  (define z (/ a b))
  (* b (- z (floor z))))

; returns triangle wave from 1-0-1 over cycletime
(define (cycletri age cycletime)
  (define a (/ (remain age cycletime) cycletime))  ; goes 0-1,0-1
  (abs (* 2.0 (- a 0.5))))  ; goes 1-0-1

; calculates partial sum of geometric series
; a is first term
; r is ratio
; n is number of terms in the sum
(define (geom-sum a r n)
  (* a (/ (- 1 (expt r n))
          (- 1 r))))

(define (sigmoid x div)
  (- (/ 2.0 (+ 1.0 (exp (- (/ x div))))) 1.0))

(define (debug fmt . args)
  (apply printf fmt args)
  (list-ref args (sub1 (length args))))

(define-syntax-rule (define/time (name arg ...) e ...)
  (define (name arg ...)
    (define start (current-milliseconds))
    (define ret (let () e ...))
    (printf "~a ~a\n" (object-name name) (- (current-milliseconds) start))
    ret))

(define-syntax-rule (with-time name e ...)
  (begin
    (define start (current-milliseconds))
    (define ret (let () e ...))
    (printf "~a ~a\n" name (- (current-milliseconds) start))
    ret))

(define-syntax-rule (while test e ...)
  (let loop ()
    (when test
      e ...
      (loop))))

; returns a list of stacks of all objects from ownspace
; to the object with the given id (found object first)
(define (search space o id (multiple? #f) (subships? #t))
  (define search-return '())  ; found stacks
  (let/ec esc
    (let search-internal ((o o) (stack '()))
      (when (and (not (space? o))
                 (or (and (number? id) (= id (ob-id o)))
                     (and (procedure? id) (id o))))
        (set! search-return (cons (cons o stack) search-return))
        (when (not multiple?) (esc)))
      (cond
        ((or (player? o)
             (plasma? o)
             (explosion? o)
             (missile? o)
             (shield? o)
             (effect? o)
             (message? o)
             (upgrade? o)
             (ann? o)
             (dmg? o))
         (void))
        ((space? o)
         (for ((x (in-list (space-objects o)))
               #:when (obj-alive? x))
           (search-internal x (cons o stack))))
        ((ship? o)
         (for ((x (in-list (ship-tools o))))
           (search-internal x (cons o stack)))
         (for ((x (in-list (ship-players space o))))
           (search-internal x (cons o stack)))
         (for ((x (in-list (ship-cargo o))))
           (search-internal x (cons o stack)))
         (when (and subships? (ship-hangar o))
           (for ((x (in-list (ship-hangar o))))
             (search-internal x (cons o stack)))))
        ((tool? o)
         (for ((x (in-list (tool-dmgs o))))
           (search-internal x (cons o stack))))
        (else
         (error 'search-internal "hit ELSE clause for ~v" o)))))
  search-return)


(define (find-all space o id)
  (map car (search space o id #t)))

(define (find-stack space o id (subships? #t))
  (define r (if id (search space o id #f subships?) null))
  (if (null? r) #f (car r)))

(define (find-id space o id (subships? #t))
  (define r (find-stack space o id subships?))
  (if r (car r) #f))

(define (find-top-id space id)
  (for/first ((o (in-list (space-objects space))) #:when (= (ob-id o) id)) o))

(define (find-top-containing-id space id)
  (for/first ((o (in-list (space-objects space)))
              #:when (find-id space o id))
    o))

(define (ship-flying? ship)
  (obj-posvel ship))

(define (get-ship stack)
  (findf ship? stack))

(define (get-ships stack)
  (filter ship? stack))

(define (get-topship stack)
  (get-ship (reverse stack)))

(define (ship-ships s)
  (define h (ship-hangar s))
  (if h h '()))

(define (get-center space stack)
  (define shipcenter (get-topship stack))
  (define p (car stack))
  (define rcobj (if (player-rcid p) (find-id space space (player-rcid p)) #f))
  (cond
    (rcobj rcobj)
    (else shipcenter)))

(define (get-space stack)
  (car (reverse stack)))

(define (ai-ship? o)
  (and (ship? o) (ship-ai? o) (null? (ship-playerids o))))

(define (can-launch? stack)
  (define ships (get-ships stack))
  (and (not (ship-flying? (car ships)))
       (not (null? (cdr ships)))
       (ship-flying? (cadr ships))))

(define (ship-tool ship sym)
  (findf (lambda (t) (equal? sym (tool-name t))) (ship-tools ship)))

(define (player-using-tool? p t)
  (findf (lambda (c)
           (equal? c (tool-name t)))
         (player-commands p)))

(define (tool-count space t ship)
  (cond
    ((not t) 0)
    ((empty? (ship-playerids ship))
     (if (tool-rc t) 1 0))
    (else
     (count (lambda (p) (player-using-tool? p t))
            (ship-players space ship)))))

(define (tool-online? t (dmgtype "offline"))
  (not (findf (lambda (d) (equal? dmgtype (dmg-type d))) (tool-dmgs t))))

(define (will-dock? s1 s2)
  (define d (ship-tool s1 'dock))
  (and d
       (tool-rc d)
       ((faction-check (ship-faction s2) (ship-faction s1)) . > . 0)
       (ship-hangar s2)))


(define (angle-norm r)
  (while (r . >= . 2pi) (set! r (- r 2pi)))
  (while (r . < . 0) (set! r (+ r 2pi)))
  r)

(define (angle-add r theta)
  (angle-norm (+ r theta)))

; gives angular distance and direction (-pi to pi)
(define (angle-frto from to)
  (define diff (- to from))
  (cond (((abs diff) . <= . pi) diff)
        ((diff . > . pi) (- diff 2pi))
        (else (+ 2pi diff))))


(define (random-between a b)
  (+ a (* (- b a) (random))))


(define (distance o1 o2)
  (define dx (- (posvel-x (obj-posvel o1)) (posvel-x (obj-posvel o2))))
  (define dy (- (posvel-y (obj-posvel o1)) (posvel-y (obj-posvel o2))))
  (sqrt (+ (* dx dx) (* dy dy))))

(define (dist-polar r1 t1 r2 t2)
  (sqrt (+ (* r1 r1) (* r2 r2)
           (- (* 2 r1 r2 (cos (- t2 t1)))))))

(define (atan0 y x)
  (if (= 0.0 x y) 0.0 (atan y x)))


(define (theta from to)
  (define dx (- (obj-x to) (obj-x from)))
  (define dy (- (obj-y to) (obj-y from)))
  ;(printf "dx ~a, dy ~a\n" dx dy)
  (atan0 dy dx))

(define (dtheta o)
  (define dx (posvel-dx (obj-posvel o)))
  (define dy (posvel-dy (obj-posvel o)))
  (atan0 dy dx))

(define (dmag o)
  (define pv (obj-posvel o))
  (sqrt (+ (* (posvel-dx pv) (posvel-dx pv))
           (* (posvel-dy pv) (posvel-dy pv)))))

(define (hit-distance ship1 ship2)
  (+ (ship-radius ship1)
     (ship-radius ship2)))


;; ai utils

(define (nearest-enemy qt ownship filterf?)
  (define ne #f)
  (define ne-dist #f)
  (for ((e (in-list (qt-retrieve qt (obj-x ownship) (obj-y ownship) (ship-radar ownship))))
        #:when (and (filterf? e)
                    ((faction-check (ship-faction ownship) (ship-faction e)) . < . 0)))
    (define d (distance ownship e))
    (when (and (d . < . (ship-radar ownship))
               (or (not ne) (d . < . ne-dist)))
      (set! ne e)
      (set! ne-dist d)))
  ne)


(define (ship-behind? qt ship)
  (define max-dist 50.0)
  (define max-ang pi/2)
  (define ns #f)
  
  (for ((o (in-list (qt-retrieve qt (obj-x ship) (obj-y ship) (+ (ship-radius ship) max-dist))))
        #:when (and (spaceship? o)
                    (not (= (ob-id ship) (ob-id o)))
                    ((distance ship o) . < . (+ (hit-distance ship o) max-dist))))
    
    (define a (angle-frto (angle-add pi (posvel-r (obj-posvel ship))) (theta ship o)))
    (when ((abs a) . < . max-ang)
      (set! ns o)))
  
  ns)


(define (target-angle source-pos source-vel target-pos target-vel shot-speed shot-life-secs)
  ; relative velocity
  (define vx (- (if target-vel (posvel-dx (obj-posvel target-vel)) 0)
                (if source-vel (posvel-dx (obj-posvel source-vel)) 0)))
  (define vy (- (if target-vel (posvel-dy (obj-posvel target-vel)) 0)
                (if source-vel (posvel-dy (obj-posvel source-vel)) 0)))
  (define st-r (theta source-pos target-pos))
  (define t #f)
  (cond
    ((= 0 vy vx)
     ; nobody's moving, so shoot directly at them
     (set! t st-r))
    (else
     ; use law of sines
     ; v-l and shot-speed are the lengths of the sides
     (define v-l (sqrt (+ (* vx vx) (* vy vy))))

     ; this is angle opposite shot-speed
     ; between side connecting ships and enemy velocity
     (define v-r (atan vy vx))
     
     ; should be sin(pi - x) but that's equal to sin(x)
     (define rel-angle (angle-frto v-r st-r))
     (define sin-aim (* (sin rel-angle)
                        (/ v-l shot-speed)))
     (when (<= -1 sin-aim 1)
       (define maybe-t (angle-add st-r (- (asin sin-aim))))

       ; answer could be going backwards in time
       (define secs #f)
       (define dx (- (* shot-speed (cos maybe-t)) vx))
       (cond
         ((not (= dx 0))
          (set! secs (/ (- (obj-x target-pos) (obj-x source-pos)) dx)))
         (else
          (define dy (- (* shot-speed (sin maybe-t)) vy))
          (when (not (= dy 0))
            (set! secs (/ (- (obj-y target-pos) (obj-y source-pos)) dy)))))

       (when (and secs (< 0 secs shot-life-secs))
         (set! t maybe-t)))))
  t)
