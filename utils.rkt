#lang racket/base

(require racket/function
         racket/math)

(require "defs.rkt")

(provide (all-defined-out))


(define (obj-age space o)
  (- (space-time space) (obj-start-time o)))

; age, life, death are ms since object start time
(define (linear-fade age life death)
  (cond ((age . < . life) 1.0)
        ((age . > . death) 0.0)
        (else (/ (- death age)
                 (- death life)))))

(define (remain a b)
  (define z (/ a b))
  (* b (- z (floor z))))

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
(define (search o id (multiple? #f) (stack '()))
  (let/ec found
    (cond
      ((and (not (space? o)) (integer? id) (= id (ob-id o)))
       (list (cons o stack)))
      ((and (not (space? o)) (procedure? id) (id o))
       (list (cons o stack)))
      (else
       (define results
         (for/list ((c (get-children o)))
           (define r (search c id multiple? (cons o stack)))
           (if (and (not multiple?)
                    (not (null? r)))
               (found r)
               r)))
       (filter (negate null?) (apply append results))))))


(define (find-all o id)
  (map car (search o id #t)))

(define (find-stack o id)
  (define r (if id (search o id) null))
  (if (null? r) #f (car r)))

(define (find-id o id)
  (define r (find-stack o id))
  (if r (car r) #f))


(define (ship-helm s)
  (car (memf helm? (ship-pods s))))

(define (ship-pilot s)
  (pod-role (ship-helm s)))

(define (ship-flying? ship)
  (obj-posvel ship))

(define (recenter center o)
  (values (- (posvel-x (obj-posvel o)) (posvel-x (obj-posvel center)))
          (- (posvel-y (obj-posvel o)) (posvel-y (obj-posvel center)))))

(define (get-role stack)
  (car (memf role? stack)))

(define (get-pod stack)
  (car (memf pod? stack)))

(define (get-ship stack)
  (car (memf ship? stack)))

(define (get-ships stack)
  (filter ship? stack))

(define (get-hangar ship)
  (define a (filter hangarpod? (ship-pods ship)))
  (if (null? a)
      #f
      (car a)))

(define (get-center stack)
  (define center (get-ship (reverse stack)))
  (define spv (obj-posvel center))
  
  (define ship (get-ship stack))
  (cond
    ((and (equal? (ob-id ship) (ob-id center))
          (pod-facing (get-pod stack)))
     (define pod (get-pod stack))
     (obj #f #f (posvel
                 0
                 (+ (posvel-x spv) (* (pod-dist pod) (cos (+ (posvel-r spv) (pod-angle pod)))))
                 (+ (posvel-y spv) (* (pod-dist pod) (sin (+ (posvel-r spv) (pod-angle pod)))))
                 (posvel-r spv) 0 0 0)))
    (else center)))

(define (get-space stack)
  (car (reverse stack)))



(define (angle-norm r)
  (cond ((r . >= . 2pi) (- r 2pi))
        ((r . < . 0) (+ r 2pi))
        (else r)))

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

(define (theta from to)
  (define dx (- (posvel-x (obj-posvel to)) (posvel-x (obj-posvel from))))
  (define dy (- (posvel-y (obj-posvel to)) (posvel-y (obj-posvel from))))
  ;(printf "dx ~a, dy ~a\n" dx dy)
  (if (= 0 dx dy)
      0
      (atan dy dx)))

(define (dtheta o)
  (define dx (posvel-dx (obj-posvel o)))
  (define dy (posvel-dy (obj-posvel o)))
  (if (= 0 dx dy)
      0
      (atan dy dx)))

(define (dmag o)
  (define pv (obj-posvel o))
  (sqrt (+ (* (posvel-dx pv) (posvel-dx pv))
           (* (posvel-dy pv) (posvel-dy pv)))))


(define (pod-xyr pod ship)
  (define ps (obj-posvel ship))
  (define podangle (+ (posvel-r ps) (pod-angle pod)))
  (values (+ (posvel-x ps) (* (pod-dist pod) (cos podangle)))
          (+ (posvel-y ps) (* (pod-dist pod) (sin podangle)))
          podangle))

(define (pod-obj pod ship)
  (define-values (px py pr) (pod-xyr pod ship))
  (obj #f #f (posvel #f px py pr (posvel-dx (obj-posvel ship)) (posvel-dy (obj-posvel ship)) #f)))


;; ai utils

(define (nearest-enemy space ownship)
  (define agro-dist 600)  ; ignore ships farther away than this
  
  (define enemies (filter (lambda (o)
                            (and (ship? o)
                                 (not (equal? (ship-faction o) (ship-faction ownship)))))
                          (space-objects space)))
  (define ne #f)
  (define ne-dist #f)
  (for ((e enemies))
    (define d (distance ownship e))
    (when (and (d . < . agro-dist)
               (or (not ne) (d . < . ne-dist)))
      (set! ne e)
      (set! ne-dist d)))
  ne)


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
  (for ((p plasmas))
    (define d (distance ownship p))
    (when (d . < . agro-dist)
      (define t (target-angle p #f ownship ownship PLASMA_SPEED))
      (when (and t
                 ((abs (angle-diff t (dtheta p))) . < . (degrees->radians 3))  ; incoming
                 (or (not np) (d . < . np-dist)))
        (set! np p)
        (set! np-dist d))))
  np)
