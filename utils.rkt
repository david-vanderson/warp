#lang racket/base

(require racket/math
         racket/port
         racket/list
         racket/struct
         racket/draw)

(require "defs.rkt")

(provide (all-defined-out))

(define (standard-quit-scenario-tab-button)
  (ann-button (next-id) 0 (posvel 0 (+ LEFT 90) (+ TOP 80) 0 180 50 0) #t "Quit Scenario" "quit-scenario"))

(define-syntax-rule (append! lst e ...)
  (set! lst (append lst (flatten (list e ...)))))

(define (remove-id id list)
  (filter (lambda (o) (not (equal? (ob-id o) id)))
          list))

(define (findfid id list)
  (findf (lambda (o) (= id (ob-id o))) list))

(define (copy-prefab s)
  (apply make-prefab-struct (prefab-struct-key s) (struct->list s)))

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

(define (faction-check-color my-faction other-faction)
  (define fc (faction-check my-faction other-faction))
  (cond ((fc . > . 0) "blue")
        ((fc . < . 0) "red")
        (else "gray")))

(define (strategy-age space s)
  (- (space-time space) (strategy-t s)))

; age, life, death are ms since object start time
(define (linear-fade age life death)
  (cond ((age . < . life) 1.0)
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
(define (search o id (multiple? #f) (subships? #t))
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
             (missile? o)
             (shield? o)
             (effect? o)
             (message? o)
             (upgrade? o)
             (ann? o)
             (dmg? o))
         (void))
        ((space? o)
         (for ((x (in-list (space-objects o))))
           (search-internal x (cons o stack))))
        ((ship? o)
         (for ((x (in-list (ship-tools o))))
           (search-internal x (cons o stack)))
         (for ((x (in-list (ship-players o))))
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


(define (find-all o id)
  (map car (search o id #t)))

(define (find-stack o id (subships? #t))
  (define r (if id (search o id #f subships?) null))
  (if (null? r) #f (car r)))

(define (find-id o id (subships? #t))
  (define r (find-stack o id subships?))
  (if r (car r) #f))

(define (find-top-id space id)
  (for/first ((o (in-list (space-objects space))) #:when (= (ob-id o) id)) o))

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
  (define rcobj (if (player-rcid p) (find-id space (player-rcid p)) #f))
  (cond
    (rcobj rcobj)
    (else shipcenter)))

(define (get-space stack)
  (car (reverse stack)))

(define (ai-ship? o)
  (and (ship? o) (ship-ai? o) (null? (ship-players o))))

(define (can-launch? stack)
  (define ships (get-ships stack))
  (and (not (ship-flying? (car ships)))
       (not (null? (cdr ships)))
       (ship-flying? (cadr ships))))

(define (ship-tool ship sym)
  (findf (lambda (t) (equal? sym (tool-name t))) (ship-tools ship)))

(define (tool-count t ship)
  (cond
    ((not t) 0)
    ((empty? (ship-players ship))
     (if (tool-rc t) 1 0))
    (else
     (count (lambda (p)
              (findf (lambda (c)
                       (equal? c (tool-name t)))
                     (player-commands p)))
            (ship-players ship)))))

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

(define (nearest-enemy space ownship)
  (define enemies (filter (lambda (o)
                            (and (spaceship? o)
                                 ((faction-check (ship-faction ownship) (ship-faction o)) . < . 0)))
                          (space-objects space)))
  (define ne #f)
  (define ne-dist #f)
  (for ((e (in-list enemies)))
    (define d (distance ownship e))
    (when (and (d . < . (ship-radar ownship))
               (or (not ne) (d . < . ne-dist)))
      (set! ne e)
      (set! ne-dist d)))
  ne)


(define (ship-behind? space ship)
  (define max-dist 75)
  (define max-ang pi/2)
  (define ships (filter (lambda (o)
                          (and (spaceship? o)
                               (not (= (ob-id ship) (ob-id o)))
                               ((distance ship o) . < . max-dist)))
                        (space-objects space)))
  (define ns #f)
  (for ((s (in-list ships)))
    (define a (angle-frto (angle-add pi (posvel-r (obj-posvel ship))) (theta ship s)))
    (when ((abs a) . < . max-ang)
      (set! ns s)))
  ns)


(define (target-angle source-pos source-vel target-pos target-vel shot-speed)
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
     (define v-r (atan vy vx))
     (define v-l (sqrt (+ (* vx vx) (* vy vy))))
     (define sin-aim (* (sin (angle-frto st-r v-r)) (/ v-l shot-speed)))
     (when (< -1 sin-aim 1)
       (set! t (angle-add st-r (asin sin-aim))))))
  t)
  

(define (nearest-incoming-plasma space ownship)
  (define agro-dist 200)  ; ignore plasmas farther away than this
  
  (define plasmas (filter plasma? (space-objects space)))
  (define np #f)
  (define np-dist #f)
  (for ((p (in-list plasmas)))
    (define d (distance ownship p))
    (when (d . < . agro-dist)
      (define t (target-angle p #f ownship ownship PLASMA_SPEED))
      (when (and t
                 ((abs (angle-frto t (dtheta p))) . < . (degrees->radians 3))  ; incoming
                 (or (not np) (d . < . np-dist)))
        (set! np p)
        (set! np-dist d))))
  np)
