#lang racket/base

(provide qt-new
         qt-add!
         qt-retrieve
         qt-collide!)

(define NUM_OBJS_BEFORE_SPLIT 10)
(define MIN_SIZE 50)

(struct qtobj (o x y r) #:prefab)
; o is object
; r is radius

(define (qt-hit? a b)
  (define d (+ (qtobj-r a) (qtobj-r b)))
  ((qt-distance2 a b) . < . (* d d)))

(define (qt-distance2 a b)
  (define dx (- (qtobj-x a) (qtobj-x b)))
  (define dy (- (qtobj-y a) (qtobj-y b)))
  (+ (* dx dx) (* dy dy)))

(struct quadtree (objs x y w h subtrees) #:mutable #:prefab)

(define (qt-new x y w h)
  ;(printf "qt-new w ~a h ~a\n" w h)
  (quadtree '()
            (exact->inexact x)
            (exact->inexact y)
            (exact->inexact w)
            (exact->inexact h)
            '()))

(define (qt-add! qt o x y r)
  (define qto (qtobj o (exact->inexact x) (exact->inexact y) (exact->inexact r)))
  (qt-add-internal! qt qto))

(define (qt-add-internal! qt qto)
  (cond
    ((null? (quadtree-subtrees qt))
     (set-quadtree-objs! qt (cons qto (quadtree-objs qt)))
     (when (and ((length (quadtree-objs qt)) . > . NUM_OBJS_BEFORE_SPLIT)
                ((max (quadtree-w qt) (quadtree-h qt)) . > . MIN_SIZE))
       (split! qt)))
    (else
     (define idx (get-index qt qto))
     (cond
       ((= idx -1)
        ; doesn't fit in a subtree
        (set-quadtree-objs! qt (cons qto (quadtree-objs qt))))
       (else
        (qt-add-internal! (list-ref (quadtree-subtrees qt) idx) qto))))))

(define (split! qt)
  (define x (quadtree-x qt))
  (define y (quadtree-y qt))
  (define w (quadtree-w qt))
  (define h (quadtree-h qt))
  (define w/2 (/ w 2.0))
  (define h/2 (/ h 2.0))
  (define w/4 (/ w 4.0))
  (define h/4 (/ h 4.0))
  (set-quadtree-subtrees! qt
                          (list (qt-new (- x w/4) (- y h/4) w/2 h/2)
                                (qt-new (- x w/4) (+ y h/4) w/2 h/2)
                                (qt-new (+ x w/4) (+ y h/4) w/2 h/2)
                                (qt-new (+ x w/4) (- y h/4) w/2 h/2)))
  (define objs (quadtree-objs qt))
  (set-quadtree-objs! qt '())
  (for ((o objs))
    (qt-add-internal! qt o)))

(define (get-index qt qto)
  (define top? ((- (qtobj-y qto) (qtobj-r qto)) . > . (quadtree-y qt)))
  (define bot? ((+ (qtobj-y qto) (qtobj-r qto)) . < . (quadtree-y qt)))
  (define left? ((+ (qtobj-x qto) (qtobj-r qto)) . < . (quadtree-x qt)))
  (define right? ((- (qtobj-x qto) (qtobj-r qto)) . > . (quadtree-x qt)))
  (cond
    ((and bot? left?) 0)
    ((and top? left?) 1)
    ((and top? right?) 2)
    ((and bot? right?) 3)
    (else -1)))

; return list of colliding objects
(define (qt-retrieve qt x y r)
  (define qto (qtobj 'unused (exact->inexact x) (exact->inexact y) (exact->inexact r)))
  (map qtobj-o (qt-retrieve-internal qt qto)))

(define (qt-retrieve-internal qt qto)
  (define idx (get-index qt qto))
  (define lst (filter (lambda (o)
                        (qt-hit? qto o))
                      (quadtree-objs qt)))
  (cond
    ((null? (quadtree-subtrees qt))
     lst)
    ((= idx -1)
     (apply append lst
            (for/list ((qtst (in-list (quadtree-subtrees qt))))
              (qt-retrieve-internal qtst qto))))
    (else
     (append lst
             (qt-retrieve-internal (list-ref (quadtree-subtrees qt) idx) qto)))))

; call coll! with every pair of objects that might collide
(define (qt-collide! qt coll! (parent-obj-list '()))
  (let loop ((objs (quadtree-objs qt)))
    (when (not (null? objs))
      (define a (car objs))
      ; test a against all other objects at this level, then all the parent objects
      (for* ((lst (cons (cdr objs) parent-obj-list))
             (b lst))
        (when (qt-hit? a b)
          (coll! (qtobj-o a) (qtobj-o b))))
      (loop (cdr objs))))

  (for ((st (quadtree-subtrees qt)))
    (qt-collide! st coll! (cons (quadtree-objs qt) parent-obj-list))))

(module+ test
  (random-seed 1)
  (define qt (qt-new 0 0 1000 1000))
  (for ((i 1000))
    (qt-add! qt i (* (- 0.5 (random)) 100) (* (- 0.5 (random)) 100) 10))
  ;(qt-retrieve qt 100 0 1.0)
  )