#lang racket/base

(require racket/math
         racket/vector
         racket/format)

(require "defs.rkt")

(provide (all-defined-out))


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

(define (opposite-sign? a b)
  (if (positive? a)
      (negative? b)
      (positive? b)))

(define (drag dv dt coef epsilon)
  (define newv (* dv (expt (1 . - . coef) dt)))
  (if ((abs newv) . < . epsilon) 0 newv))


(define (steer! ownship dt)
  (define course (helm-course (ship-helm ownship)))
  (define r (thing-r ownship))
  (define ddr
    (cond ((and
            ((abs (angle-diff r course)) . < . (/ 2pi 180))  ; within 2 degrees
            ((abs (thing-dr ownship)) . < . (/ 2pi 120)))  ; moving less than 3 degrees / sec
           ;(printf "STOPPING\n")
           (set-thing-r! ownship course)
           (set-thing-dr! ownship 0)
           0)  ; don't need to accelerate if we're already there
          (else
           (define future-r (angle-add r (thing-dr ownship)))  ; we'll be there in 1 sec
           (define future-diff (angle-diff future-r course))
           (define diff (angle-diff r course))
           (define fullRACC (if (positive? diff) RACC (- RACC)))
           
           (cond ((opposite-sign? diff (thing-dr ownship)) fullRACC)  ; going the wrong way, full acc
                 ((opposite-sign? diff future-diff)
                  ; getting close, accelerate future-diff less
                  ;(printf "getting close, ~a ~a\n" diff future-diff)
                  (- (* 0.5 fullRACC) (* -3 future-diff)))
                 (else fullRACC)))))  ; not close, keep at it
  
  ;(printf "r ~a, course ~a, acc ~a\n" r course acc)
  (set-thing-dr! ownship (+ (thing-dr ownship) (* ddr dt)))
  
  (define ddx 0)
  (define ddy 0)
  (when (helm-fore (ship-helm ownship))
    (set! ddx (* 20 (cos (thing-r ownship))))
    (set! ddy (* 20 (sin (thing-r ownship)))))
  
  (set-thing-dx! ownship (+ (thing-dx ownship) (* ddx dt)))
  (set-thing-dy! ownship (+ (thing-dy ownship) (* ddy dt)))
  
  (values ddx ddy ddr))


(define (physics! thing dt drag? ddx ddy ddr)
  (set-thing-x! thing (+ (thing-x thing) (* dt (thing-dx thing))))
  (set-thing-y! thing (+ (thing-y thing) (* dt (thing-dy thing))))
  (set-thing-r! thing (angle-add (thing-r thing) (* dt (thing-dr thing))))
  (when drag?
    (set-thing-dx! thing (drag (thing-dx thing) dt DRAG_COEF (if (zero? ddx) .1 0)))
    (set-thing-dy! thing (drag (thing-dy thing) dt DRAG_COEF (if (zero? ddy) .1 0)))
    (set-thing-dr! thing (drag (thing-dr thing) dt R_DRAG_COEF (if (zero? ddr) (/ 2pi 360) 0)))))


(define (spread-shields! ship dt-arg)
;  (printf "spread-shields!\n")
  (for ((s (ship-shields ship)))
;    (printf "shields ~a\n" (shield-color s))
    (let loop ((pipe? #t))
      (define dt dt-arg)
      (define sections (shield-sections s))
      (define n (vector-length sections))
      (define new-sections (vector-copy sections))
      (define arc-size (/ (* 2pi (shield-radius s)) n))
      (define pipe-size (if pipe? (* dt (/ 15.0 arc-size)) 0))
      
      (define tweak 30)
      (define dt* (min 1.0 dt (/ arc-size tweak)))
      (set! dt (- dt dt*))
      
;      (printf "sections (~a) " (~r (for/fold ((sum 0)) ((s sections)) (+ sum s))))
;      (for ((s sections)) (printf "~a, " (~r s)))
;      (printf "\ntweak ~a, pipe-size ~a, arc-size ~a, dt ~a, dt* ~a\n" tweak (~r pipe-size) (~r arc-size) (~r dt) (~r dt*))
      
      (for ((i n))
        (define prev (if (i . > . 0)        (sub1 i) (sub1 n)))
        (define next (if (i . < . (sub1 n)) (add1 i) 0))
        (define ie (vector-ref sections i))
        (define pe (vector-ref sections prev))
        (define ne (vector-ref sections next))
        
        (when (ie . > . (add1 pe))
          (define pipe-change (min (/ (- ie pe) 2) pipe-size))
          (define ratio-change (* tweak dt* (/ (- ie pe) 3.0 arc-size)))
          (define change (max pipe-change ratio-change))
          (vector-set! new-sections i (- (vector-ref new-sections i) change))
          (vector-set! new-sections prev (+ (vector-ref new-sections prev) change)))
        (when (ie . > . (add1 ne))
          (define pipe-change (min (/ (- ie ne) 2) pipe-size))
          (define ratio-change (* tweak dt* (/ (- ie ne) 3.0 arc-size)))
          (define change (max pipe-change ratio-change))
          (vector-set! new-sections i (- (vector-ref new-sections i) change))
          (vector-set! new-sections next (+ (vector-ref new-sections next) change))))
      
      (set-shield-sections! s new-sections)
      (when (dt . > . 0)
        (printf "respreading shields\n")
        (loop #f))
      )))


; This is used on both server and client (for prediction)
(define (update-physics! ownspace dt)
  (for ((o (space-objects ownspace)))
    (cond
      ((ship? o)
       (define-values (ddx ddy ddr) (steer! o dt))
       (physics! o dt #t ddx ddy ddr)
       (spread-shields! o dt))
      ((plasma? o)
       (physics! o dt #f 0 0 0)))))


(define (distance thing1 thing2)
  (define dx (- (thing-x thing1) (thing-x thing2)))
  (define dy (- (thing-y thing1) (thing-y thing2)))
  (sqrt (+ (* dx dx) (* dy dy))))


(define (all-within object others dist)
  (define close (filter (lambda (o) ((distance object o) . < . dist)) others))
  (remove object close))


(define (theta from to)
  (define dx (- (thing-x to) (thing-x from)))
  (define dy (- (thing-y to) (thing-y from)))
  ;(printf "dx ~a, dy ~a\n" dx dy)
  (atan dy dx))


(define (find-section shield theta)
  (define num (vector-length (shield-sections shield)))
  (define arc-size (/ 2pi num))
  (define theta* (angle-add theta (/ arc-size 2)))
  ;(printf "theta ~a, theta* ~a\n" theta theta*)
  (inexact->exact (floor (* (/ theta* 2pi) num))))


(define (plasma-hit-shield! ownspace ownship s p)
  (when (not (member (shield-color s) (plasma-shields-hit p)))
    ; find the shield section
    (define sections (shield-sections s))
    (define section (find-section s (angle-sub (theta ownship p) (thing-r ownship))))
    (define se (vector-ref sections section))
    (when (se . > . 0)
      (define pe (plasma-energy p))
      (define damage
        (cond ((se . >= . 100) pe)
              ((se . < . pe) se)
              (else (ceiling (* (/ se 100) pe)))))
      (when (equal? (shield-color s) (plasma-color p))
        (set! damage (ceiling (/ damage 2))))
      (vector-set! sections section (- se damage))
      (set-plasma-energy! p (- pe damage))
      (set-plasma-shields-hit! p (cons (shield-color s) (plasma-shields-hit p)))
      
      ;(printf "hit, new energy ~a\n" (plasma-energy p))
      
      (when ((plasma-energy p) . <= . 0)
        (set-space-objects! ownspace (remove p (space-objects ownspace)))))))


(define (update-ship-effects! ownspace ownship objects)
  (define plasmas (filter plasma? objects))
  (for* ((s (ship-shields ownship))
         (p plasmas))
    (define dist (- (shield-radius s) (distance ownship p)))
    (when ((abs dist) . < . (/ (plasma-energy p) 2))
      (plasma-hit-shield! ownspace ownship s p))))


(define (update-effects! ownspace)
  (define objects (space-objects ownspace))
  (for ((o objects))
    (cond ((ship? o) (update-ship-effects! ownspace o objects)))))
