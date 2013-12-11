#lang racket/base

(require racket/class
         racket/list
         racket/format
         racket/math)

(require "defs.rkt")

(provide (all-defined-out))

(define-syntax-rule (keep-transform dc e ...)
  (begin
    (define t (send dc get-transformation))
    (define a (let () e ...))
    (send dc set-transformation t)
    a))

(define (get-scale dc)
  (vector-ref (send dc get-initial-matrix) 0))

(define (dc->screen dc x y)
  (define m (send dc get-initial-matrix))
  (values
   (+ (* x (vector-ref m 0)) (* y (vector-ref m 1)) (vector-ref m 4))
   (+ (* x (vector-ref m 2)) (* y (vector-ref m 3)) (vector-ref m 5))))

(define (add-frame-time current-time frames)
  (cons current-time (take frames (min 10 (length frames)))))

(define (draw-framerate dc frames)
  (when (not (empty? frames))
    (keep-transform dc
      (send dc translate (- (/ WIDTH 2)) (/ HEIGHT 2))
      (send dc scale 1 -1)
      (define start (list-ref frames (- (length frames) 1)))
      (define end (first frames))
      (define span (/ (- end start) 1000))
      (send dc draw-text (format "~a" (truncate (/ (- (length frames) 1) span))) 0 0))))


(define (recenter o x y)
  (values (- x (posvel-x (obj-posvel o))) (- y (posvel-y (obj-posvel o)))))


(define (draw-background dc ownspace center)
  (define-values (x y) (recenter center 0 0))
  (send dc set-brush "red" 'transparent)
  (send dc draw-rectangle (- x 250) (- y 250) 500 500))


(define (draw-shield dc shield)
  (keep-transform dc
    (define num (vector-length (shield-sections shield)))
    (define radius (shield-radius shield))
    
    (for ((section (shield-sections shield))
          (i (in-naturals))
          #:when (section . >= . 1))
      (define linear-strength (/ section (shield-max shield)))
      (define log-strength (/ (log (add1 section)) (log (shield-max shield))))
      (send dc set-pen (shield-color shield) (* 3 (* 0.5 (+ linear-strength log-strength))) 'solid)
      (define r (- 2pi (/ (* 2pi i) num)))
      (define arc-size (* log-strength (/ 2pi num)))
      (send dc draw-arc
            (- radius) (- radius)
            (* 2 radius) (* 2 radius)
            (- r (* 0.5 arc-size)) (+ r (* 0.5 arc-size))))))


(define ship-external
  '((10 . 10)
    (0 . 20)
    (-10 . 10)
    (-10 . -10)
    (10 . -10)))

(define ship-internal
  '((10 . 10)
    (0 . 20)
    (-10 . 10)
    (-10 . -10)
    (10 . -10)))


(define (draw-ship dc s center)
  (keep-transform dc
    (define posvel (obj-posvel s))
    (define-values (x y) (recenter center (posvel-x posvel) (posvel-y posvel)))
    (send dc translate x y)
    
    (keep-transform dc
      (send dc rotate (- (posvel-r posvel)))
      (for ((shield (ship-shields s)))
        (draw-shield dc shield))
      
      (send dc rotate (/ pi 2))
      (send dc set-pen "black" 1 'solid)
      (send dc draw-polygon ship-external))
    
    (define scale 0.5)
    (send dc scale scale (- scale))
    (define text (~r (* 100 (ship-containment s)) #:precision 0))
    (define-values (w h b v) (send dc get-text-extent text))
    (send dc translate (* -0.5 w) (* -0.5 h))
    (send dc draw-text text 0 0)))


(define (draw-plasma dc p center space)
  (define-values (x y) (recenter center (posvel-x (obj-posvel p)) (posvel-y (obj-posvel p))))
  (send dc set-pen (plasma-color p) 1 'solid)
  (define rad (plasma-energy p))
  (define t (- (space-time space) (obj-start-time p)))
  (define rot (* pi (- t (truncate t))))
  (send dc draw-line
        (- x (* rad (cos rot))) (- y (* rad (sin rot)))
        (+ x (* rad (cos rot))) (+ y (* rad (sin rot))))
  (set! rot (+ rot (/ pi 2)))
  (send dc draw-line
        (- x (* rad (cos rot))) (- y (* rad (sin rot)))
        (+ x (* rad (cos rot))) (+ y (* rad (sin rot))))
  (send dc draw-ellipse (- x (/ rad 2)) (- y (/ rad 2)) rad rad))


(define (draw-no-role dc ownspace)
  (keep-transform dc
    (define max-x (space-sizex ownspace))
    (define max-y (space-sizey ownspace))
    (define scale (min (/ WIDTH max-x) (/ HEIGHT max-y)))
    (send dc scale scale scale)
    (define center (obj #f #f (posvel 0 0 0 0 0 0)))
    (draw-background dc ownspace center)
    (for ((o (space-objects ownspace)))
      (cond
        ((ship? o)
         (draw-ship dc o center))
        ((plasma? o)
         (draw-plasma dc o center ownspace)))))
  (list))


(define (draw-playing dc ownspace stack)
  (define role (get-role stack))
  (cond
    ((observer? role)
     (draw-observer dc ownspace stack))
    ((helm? role)
     (draw-observer dc ownspace stack))
    ((crewer? role)
     (draw-crewer dc ownspace stack))))


(define (draw-overlay dc ownspace stack)
  (define role (get-role stack))
  (keep-transform dc
    (send dc translate 0 (/ HEIGHT 2))
    (send dc scale 1 -1)
    (send dc draw-text (role-name role) 0 0))
  (list))


(define (draw-observer dc ownspace stack)
  (define center (get-center stack))
  (draw-background dc ownspace center)
  (for ((o (space-objects ownspace)))
    (cond
      ((ship? o)
       (draw-ship dc o center))
      ((plasma? o)
       (draw-plasma dc o center ownspace))))
  (list))


(define (draw-crewer dc ownspace stack)
  (define ship (get-ship stack))
  (keep-transform dc
    (send dc scale 18 18)
    (send dc set-pen "black" (/ 1.0 (get-scale dc)) 'solid)
    (send dc draw-polygon ship-internal))
  
  (define hangar-buttons
    (for/list ((o (ship-hangar ship))
               (i (in-naturals)))
      (keep-transform dc
        (define angle (* i (/ 2pi (length (ship-hangar ship)))))
        (send dc rotate angle)
        (send dc translate 0 100)
        (send dc rotate (- angle))
        
        (cond
          ((weapon-pod? o)
           (send dc set-pen "black" 1.0 'solid)
           (send dc draw-ellipse -20 -20 40 40)
           (send dc scale 1 -1)
           (send dc draw-text "W" -10 -10)
           
           (define-values (x y) (dc->screen dc -10 -10))
           ;(printf "x,y ~a,~a\n" x y)
           
           (button x y 20 20 (obj-id o) "W"))
          ))))
  
  (define ship-roles
    (find-all (get-ship stack)
              (lambda (o) (or (multirole? o)
                              (and (role? o) (not (role-player o)))))))
  (append
   (list leave-button)
   hangar-buttons
   (for/list ((r ship-roles)
              (i (in-naturals)))
     (cond
       ((role? r)
        (button (+ (/ (- WIDTH) 2) 100 (* i 100)) (+ (/ (- HEIGHT) 2) 60) 100 30 (obj-id r)
                (format "~a" (role-name r))))
       ((multirole? r)
        (define role (multirole-role r))
        (button (+ (/ (- WIDTH) 2) 100 (* i 100)) (+ (/ (- HEIGHT) 2) 60) 100 30 (obj-id r)
                (format "~a" (role-name role))))))))


(define (draw-buttons dc buttons)
  (for ((b buttons))
    (keep-transform dc
      (define-values (x y w h) (values (button-x b) (button-y b)
                                       (button-width b) (button-height b)))
      (send dc set-brush "lightgray" 'solid)
      (send dc set-pen "black" 1 'solid)
      (send dc draw-rectangle x y w h)
      (send dc translate x (+ y h))
      (send dc scale 1 -1)
      (send dc draw-text (button-label b) 5 5))))


