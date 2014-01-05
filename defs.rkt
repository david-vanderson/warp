#lang racket/base

(require racket/math
         racket/function)

(provide (all-defined-out))

(define PORT 22381)
(define TICK 30)  ; ms time slice for physics, also determines max client frame rate
(define SERVER_SEND_DELAY 500)  ; don't send auto updates more often than X ms
(define WIDTH 1024)  ; how many meters wide is the screen view
(define HEIGHT 768)  ; how many meters tall is the screen view
(define LEFT (/ (- WIDTH) 2))  ; left edge of canonical view
(define BOTTOM (/ (- HEIGHT) 2))  ; bottom edge of canonical view
(define DRAG_COEF .7)  ; lose X% of your velocity / sec
(define RACC .5)  ; gain X radians / sec / sec
(define R_DRAG_COEF .7)  ; lose X% of your velocity / sec
(define 2pi (* 2 pi))
(define bgcolor "black")
(define fgcolor "white")
(define nocolor "whitesmoke")  ; used with a transparent pen/brush

(define next-id
  (let ((id 0))
    (lambda ()
      (set! id (add1 id))
      id)))

(define (debug fmt . args)
  (apply printf fmt args)
  (list-ref args (sub1 (length args))))

;; Game State

(struct posvel (x y r dx dy dr) #:mutable #:prefab)

(struct obj (id start-time posvel) #:mutable #:prefab)
; id is used to uniquely identify any game object
; start-time is seconds since scenario start that this object was created
;  - used for animations
; if posvel is #f, then this obj is inside something else
; if posvel is not #f, then this is a top-level object that is drawn

(struct plasma obj (energy ownship-id shields-hit) #:mutable #:prefab)
; ownship is unique id of the ship that fired it, or #f if it belongs to no ship
;  - plasma will not interact with the shields of ownship
; shields-hit is a list of shields that this plasma has already hit

(struct shield obj (energy length) #:mutable #:prefab)
; length is the size of the shield

(struct player obj (name) #:mutable #:prefab)
; name is what is shown in UIs
; npc is #t if this player is computer controlled, #f if it's a real person

(struct role obj (player) #:mutable #:prefab)
; player is #f if this role is unoccupied

(struct multirole obj (role start? roles) #:mutable #:prefab)
; role is the role that players joining this multirole get
; roles is a list of roles with players
; start? is #t if new players can start in this role

(struct observer role () #:mutable #:prefab)
; role where you don't do anything but you see the "captain's view"

(struct crewer role () #:mutable #:prefab)
; special role used to contain players while they are choosing their next role

(struct helm role (course fore) #:mutable #:prefab)
; course is angle helm wants to point at
; if fore is #t, main thrusters are firing

(struct pod obj (role angle dist facing spread) #:mutable #:prefab)
; angle/dist is where this pod is with respect to the ship
; facing is where the pod is directed towards (with respect to the ship)
; spread is angle within which we can shoot (centered on facing)

(struct weapon pod () #:mutable #:prefab)
; angle is with respect to the ship/pod we are one
; spread is the angle within which we can shoot (centered on angle)

(struct weapons role (fire) #:mutable #:prefab)
; fire is an angle if we want to shoot a plasma (at that angle)

(struct tactical pod () #:mutable #:prefab)
; angle is with respect to the ship/pod we are one

(struct tactics role (shield) #:mutable #:prefab)
; shield is an angle if we want to shoot a shield barrier (at that angle)

(struct ship obj (name npc? faction helm observers crew reactor containment pods) #:mutable #:prefab)
; npc? is #t if this ship is computer controlled
; faction is the name that this ship belongs to
; reactor is the energy produced by the reactor
; containment is the reactor health left
; shields are in radius order starting with the largest radius
; pods is a list of all the pods on the ship

(struct space (time sizex sizey objects) #:mutable #:prefab)
; time is msec since the scenario started
; sizex and sizey are how big space is


;; Messages

;; Most messages are just role? structs, but here are the exceptions

(struct role-change (player from to) #:mutable #:prefab)
; from and to are a role? id, multirole? id, or #f (no role)


;; UI

(struct button (x y width height left-inset top-inset name label) #:mutable #:prefab)
; x y width height are 0,0 bottom left corner 1,1 top right
; name is used internally
; label is what is written on the button




(define leave-button (button (/ (- WIDTH) 2) (/ (- HEIGHT) 2) 60 30 5 5 "leave" "Leave"))



;; Utilities

(define (recenter center o)
  (values (- (posvel-x (obj-posvel o)) (posvel-x (obj-posvel center)))
          (- (posvel-y (obj-posvel o)) (posvel-y (obj-posvel center)))))

(define (get-role stack)
  (if stack (cadr stack) #f))

(define (get-pod stack)
  (car (memf pod? stack)))

(define (get-ship stack)
  (car (memf ship? stack)))

(define (get-center stack)
  (define center (cadr (reverse stack)))
  (define spv (obj-posvel center))
  (define p (memf pod? stack))
  (cond (p
         (define pod (car p))
         (obj #f #f (posvel (+ (posvel-x spv)
                               (* (pod-dist pod) (cos (+ (posvel-r spv) (pod-angle pod)))))
                            (+ (posvel-y spv)
                               (* (pod-dist pod) (sin (+ (posvel-r spv) (pod-angle pod)))))
                            (posvel-r spv) 0 0 0)))
        (else center)))

(define (get-space stack)
  (car (reverse stack)))


(define (role-name role)
  (cond ((crewer? role) "Crewer")
        ((helm? role) "Helm")
        ((observer? role) "Observer")
        ((weapons? role) "Weapons")
        ((tactics? role) "Tactics")
        (else "Unknown")))


(define (get-children o)
  ;(printf "get-children: ~v\n" o)
  (cond
    ((space? o) (space-objects o))
    ((plasma? o) (list))
    ((ship? o) (filter values (append (list (ship-helm o) (ship-observers o) (ship-crew o))
                                      (ship-pods o))))
    ((multirole? o) (multirole-roles o))
    ((role? o) (filter values (list (role-player o))))
    ((shield? o) (list))
    ((player? o) (list))
    ((pod? o) (list (pod-role o)))
    (else
     (printf "get-children hit ELSE clause, o ~v\n" o)
     (error)
     (list))))

; returns a list of stacks of all objects from ownspace
; to the object with the given id (found object first)
(define (search o id (multiple? #f) (stack '()))
  (let/ec found
    (cond
      ((and (not (space? o)) (integer? id) (= id (obj-id o)))
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


(define (big-ship name npc? faction x y r fore?)
  (ship (next-id) #f (posvel x y r 0 0 0) name npc? faction
        (helm (next-id) #f #f #f r fore?)
        (if npc? #f (multirole (next-id) #f #f (observer (next-id) #f #f #f) #f '()))
        (if npc? #f (multirole (next-id) #f #f (crewer (next-id) #f #f #f) #t '()))
        110 100
        (list
         (weapon (next-id) #f #f
                 (weapons (next-id) #f #f #f #f)
                 (/ pi 4) (sqrt 200) (/ pi 4) (/ pi 2))
         (tactical (next-id) #f #f
                   (tactics (next-id) #f #f #f #f)
                   (* 3/4 pi) (sqrt 200) (* 3/4 pi) (/ pi 2))
         )
        ))
