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


(define (ship-pilot s)
  (pod-role (car (memf helm? (ship-pods s)))))

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
  (atan dy dx))
