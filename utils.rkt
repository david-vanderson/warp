#lang racket/base

(require racket/function
         racket/math
         racket/port)

(require "defs.rkt")

(provide (all-defined-out))

(define-syntax-rule (append! lst e ...)
  (set! lst (append lst e ...)))

(define (copy s)
  (read (open-input-string (with-output-to-string (lambda () (write s))))))

(define (obj-age space o)
  (- (space-time space) (obj-start-time o)))

(define (time-for age repeat (offset 0))
  (<= (+ 1 offset) (modulo age repeat) (+ offset TICK)))

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
      (cond
        ((and (not (space? o))
              (or (and (integer? id) (= id (ob-id o)))
                  (and (procedure? id) (id o))))
         (set! search-return (cons (cons o stack) search-return))
         (when (not multiple?) (esc)))
        ((or (player? o)
             (plasma? o)
             (shield? o)
             (effect? o)
             (message? o)
             (upgrade? o)
             (dmg? o))
         (void))
        ((space? o)
         (for ((x (in-list (space-objects o))))
           (search-internal x (cons o stack))))
        ((ship? o)
         (for ((x (in-list (ship-pods o))))
           (search-internal x (cons o stack)))
         (for ((x (in-list (ship-cargo o))))
           (search-internal x (cons o stack))))
        ((lounge? o)
         (for ((x (in-list (lounge-crew o))))
           (search-internal x (cons o stack))))
        ((hangar? o)
         (for ((x (in-list (hangar-crew o))))
           (search-internal x (cons o stack)))
         (when subships?
           (for ((x (in-list (hangar-ships o))))
             (search-internal x (cons o stack)))))
        ((pod? o)
         (when (pod-player o)
           (search-internal (pod-player o) (cons o stack)))
         (for ((x (in-list (pod-tools o))))
           (search-internal x (cons o stack))))
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

(define (ai-pod? o)
  (and (pod? o) (pod-npc? o) (not (pod-player o))))

(define (npc-ship? s)
  (find-id s (lambda (o) (and (pod? o) (pod-npc? o))) #f))

(define (get-pod stack)
  (findf pod? stack))

(define (get-ship stack)
  (findf ship? stack))

(define (get-ships stack)
  (filter ship? stack))

(define (get-topship stack)
  (get-ship (reverse stack)))

(define (ship-lounge s)
  (findf lounge? (ship-pods s)))

(define (ship-hangar s)
  (findf hangar? (ship-pods s)))

(define (ship-ships s)
  (define h (ship-hangar s))
  (if h (hangar-ships h) '()))

(define (get-center stack)
  (define shipcenter (get-topship stack))
  (define spv (obj-posvel shipcenter))
  
  (define ship (get-ship stack))
  (define pod (get-pod stack))
  (cond
    ((equal? (ob-id ship) (ob-id shipcenter))
     (obj #f #f (posvel
                 0
                 (+ (posvel-x spv) (* (pod-dist pod) (cos (+ (posvel-r spv) (pod-angle pod)))))
                 (+ (posvel-y spv) (* (pod-dist pod) (sin (+ (posvel-r spv) (pod-angle pod)))))
                 (posvel-r spv) 0 0 0)))
    (else shipcenter)))

(define (get-space stack)
  (car (reverse stack)))


(define (can-launch? stack)
  (define ships (get-ships stack))
  (and (not (ship-flying? (car ships)))
       (not (null? (cdr ships)))
       (ship-flying? (cadr ships))))


(define (will-dock? s1 s2)
  (define d (find-id s1 dock? #f))
  (and d
       (dock-on d)
       (equal? (ship-faction s1) (ship-faction s2))
       (ship-hangar s2)))


(define (angle-norm r)
  (while (r . >= . 2pi) (set! r (- r 2pi)))
  (while (r . < . 0) (set! r (+ r 2pi)))
  r)

(define (angle-add r theta)
  (angle-norm (+ r theta)))

(define (angle-sub r theta)
  (angle-norm (- r theta)))

; gives angular distance and direction (-pi to pi)
(define (angle-diff from to)
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


(define (atan0 y x)
  (if (= 0.0 x y) 0.0 (atan y x)))


(define (theta from to)
  (define dx (- (posvel-x (obj-posvel to)) (posvel-x (obj-posvel from))))
  (define dy (- (posvel-y (obj-posvel to)) (posvel-y (obj-posvel from))))
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

(define (pod-obj pod ship)
  (define ps (obj-posvel ship))
  (define pa (+ (posvel-r ps) (pod-angle pod)))
  (define pf (pod-facing pod))
  (obj #f #f (posvel #f
                     (+ (posvel-x ps) (* (pod-dist pod) (cos pa)))
                     (+ (posvel-y ps) (* (pod-dist pod) (sin pa)))
                     (+ (posvel-r ps) (if pf pf (pod-angle pod)))
                     ; add rotational velocity
                     (+ (posvel-dx ps) (* -1 (* (pod-dist pod) (posvel-dr ps)) (sin pa)))
                     (+ (posvel-dy ps) (* 1 (* (pod-dist pod) (posvel-dr ps)) (cos pa)))
                     (posvel-dr ps))))


;; ai utils

(define (nearest-enemy space ownship)
  (define agro-dist 500)  ; ignore ships farther away than this
  
  (define enemies (filter (lambda (o)
                            (and (spaceship? o)
                                 ((ship-con o) . > . 0)
                                 (not (equal? (ship-faction o) (ship-faction ownship)))))
                          (space-objects space)))
  (define ne #f)
  (define ne-dist #f)
  (for ((e (in-list enemies)))
    (define d (distance ownship e))
    (when (and (d . < . agro-dist)
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
                               ((ship-con o) . > . 0)
                               ((distance ship o) . < . max-dist)))
                        (space-objects space)))
  (define ns #f)
  (for ((s (in-list ships)))
    (define a (angle-diff (angle-add pi (posvel-r (obj-posvel ship))) (theta ship s)))
    (when ((abs a) . < . max-ang)
      (set! ns s)))
  ns)


(define (target-angle source-pos source-vel target-pos target-vel shot-speed)
  ; relative velocity
  (define vx (- (if target-vel (posvel-dx (obj-posvel target-vel)) 0)
                (if source-vel (posvel-dx (obj-posvel source-vel)) 0)))
  (define vy (- (if target-vel (posvel-dy (obj-posvel target-vel)) 0)
                (if source-vel (posvel-dy (obj-posvel source-vel)) 0)))
  (define t #f)
  (cond
    ((= 0 vy vx)
     ; nobody's moving, so shoot directly at them
     (set! t (theta source-pos target-pos)))
    (else
     (define v-r (atan vy vx))
     (define v-l (sqrt (+ (* vx vx) (* vy vy))))
     (define ep-r (theta target-pos source-pos))
     (define sin-aim (* (sin (angle-diff ep-r v-r)) (/ v-l shot-speed)))
     (when (< -1 sin-aim 1)
       (set! t (angle-sub (theta source-pos target-pos) (asin sin-aim))))))
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
                 ((abs (angle-diff t (dtheta p))) . < . (degrees->radians 3))  ; incoming
                 (or (not np) (d . < . np-dist)))
        (set! np p)
        (set! np-dist d))))
  np)
