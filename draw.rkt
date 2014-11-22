#lang racket/base

(require racket/class
         racket/list
         racket/draw)

(require "defs.rkt"
         "utils.rkt"
         "draw-utils.rkt"
         "plasma.rkt"
         "shield.rkt"
         "effect.rkt"
         "ships.rkt"
         "upgrade.rkt")

(provide (all-defined-out))


(define (add-frame-time current-time frames)
  (cons current-time (take frames (min 10 (length frames)))))

(define (draw-framerate dc frames)
  (when ((length frames) . > . 1)
    (send dc set-text-foreground "white")
    (keep-transform dc
      (send dc translate (- (/ WIDTH 2)) (/ (- HEIGHT) 2))
      (define start (list-ref frames (- (length frames) 1)))
      (define end (first frames))
      (define span (/ (- end start) 1000))
      (draw-text dc (format "FPS: ~a" (truncate (/ (- (length frames) 1) span))) 0 0))))


(define stars '(
    (-232 . 26) (119 . -256) (-160 . 406) (104 . 15) (-494 . 107)
    (105 . -158) (403 . 127) (233 . 365) (137 . 492) (-465 . -351)
    (422 . 52) (-129 . 30) (-96 . -187) (-149 . -423) (-98 . 414)
    (-205 . -1) (-405 . 265) (119 . 65) (-343 . 330) (-427 . 198)
    (-433 . -93) (1 . -216) (5 . -228) (152 . 196) (125 . 166)
    (367 . 138) (-100 . -406) (53 . 385) (434 . 198) (-20 . -398)
    (220 . -268) (320 . 413) (-82 . 44) (-182 . 243) (85 . -178)
    (101 . 276) (-253 . -146) (-315 . 279) (-371 . -159) (-277 . 379)
    (-409 . -410) (-202 . -61) (-121 . 122) (-338 . -67) (-334 . -106)
    (315 . -12) (-120 . 192) (351 . -179) (431 . -360) (206 . 484)
    (-455 . 119) (-246 . -474) (-195 . 384) (318 . 50) (-88 . -466)
    (402 . 214) (67 . 166) (-309 . -397) (465 . 460) (-126 . 397)
    (-181 . 418) (98 . 369) (-423 . -287) (-476 . -108) (-446 . 232)
    (-226 . -225) (-127 . 74) (-347 . -268) (27 . 153) (385 . -152)
    (-75 . -99) (353 . -245) (450 . 209) (-138 . 305) (82 . 220)
    (-336 . -40) (484 . 159) (-206 . -148) (259 . -298) (129 . 147)
    (424 . 15) (394 . 101) (148 . 376) (250 . -446) (89 . -257) (-231 . 426)
    (135 . 195) (-432 . -373) (-96 . 0) (81 . -453) (-211 . -347) (366 . -106)
    (314 . -242) (324 . 15) (-403 . -427) (219 . 95) (159 . 308) (-265 . 234)
    (357 . -57) (387 . 272)))

(define (draw-background-stars dc space center parallax width height)
  (define repeatx 1000)
  (define repeaty 1000)
  (define x (remain (* parallax (posvel-x (obj-posvel center))) repeatx))
  (define y (remain (* parallax (- (posvel-y (obj-posvel center)))) repeaty))
  
  ;(set! width (/ width 2))
  ;(set! height (/ height 2))
  (define w/2 (/ width 2))
  (define h/2 (/ height 2))
  
  (define istart (- (ceiling (- (/ (- w/2 x) repeatx) 0.5))))
  (define iend (add1 (ceiling (- (/ (+ w/2 x) repeatx) 0.5))))
  (define kstart (- (ceiling (- (/ (- h/2 y) repeaty) 0.5))))
  (define kend (add1 (ceiling (- (/ (+ h/2 y) repeaty) 0.5))))
  
  (send dc set-pen "white" 1.5 'solid)
  
  (for* ((i (in-range istart iend))
         (k (in-range kstart kend)))
    (define xx (- x (* i repeatx)))
    (define yy (- y (* k repeaty)))
    
    (for ((s (in-list stars)))
      (define sx (- (car s) xx))
      (define sy (- (cdr s) yy))
      (when (and (< (- w/2) sx w/2)
                 (< (- h/2) sy h/2)
                 (< (- (/ (space-width space) 2)) (+ (posvel-x (obj-posvel center)) sx) (/ (space-width space) 2))
                 (< (- (/ (space-height space) 2)) (- (posvel-y (obj-posvel center)) sy) (/ (space-height space) 2)))
        (send dc draw-point sx sy))))
  
;  (send dc set-pen "white" 1 'solid)
;  (send dc set-brush nocolor 'transparent)
;  (send dc draw-rectangle (- (/ width 2)) (- (/ height 2)) width height)
  
  )


(define (draw-object dc o center space (map #f))
  (cond
    ((ship? o)
     (if map
         (draw-ship dc o center)
         (draw-ship dc o center)))
    ((plasma? o)
     (if map
         (let ()
           (define-values (x y) (recenter center o))
           (send dc set-pen "red" (/ 2 (dc-point-size dc)) 'solid)
           (send dc draw-point x (- y)))
         (draw-plasma dc o center space)))
    ((shield? o)
     (draw-shield dc space center o))
    ((effect? o)
     (if map
         (void)
         (draw-effect dc space center o)))
    ((upgrade? o)
     (if map
         (draw-upgrade dc space center o)
         (draw-upgrade dc space center o)))))


(define (draw-server-objects dc center space)
  (send dc set-pen "hotpink" 1 'solid)
  (send dc set-brush nocolor 'transparent)
  (for ((o (in-list (space-objects space))))
    (keep-transform dc
      (define-values (x y) (recenter center o))
      (send dc translate x y)
      (send dc draw-ellipse -2 -2 4 4)
      (when (ship? o)
        (define r (ship-radius o))
        (send dc draw-ellipse (- r) (- r) (* 2 r) (* 2 r))))))


(define (draw-view dc center space)
  ;(draw-background dc space center background-bitmap 4 1 WIDTH HEIGHT)
  (draw-background-stars dc space center 1 WIDTH HEIGHT)
  ;(draw-background dc space center stars1-bitmap 8 2 WIDTH HEIGHT)
  (define objects (space-objects space))
  (define ships (filter ship? objects))
  (define effects (filter effect? objects))
  (define other (remove* (append ships effects) objects))
  
  (define backeffects (filter backeffect? effects))
  (set! effects (remove* backeffects effects))
  
  (for ((o (in-list (append backeffects ships other effects))))
    (draw-object dc o center space)))


(define (draw-ship dc s center)
  (keep-transform dc
    (define-values (x y) (recenter center s))
    (send dc translate x (- y))
    (send dc rotate (posvel-r (obj-posvel s)))
    (send dc set-pen fgcolor 1 'solid)
    (send dc set-brush nocolor 'transparent)
    (define ship-bitmap (get-ship-bitmap s))
    (send dc draw-bitmap
          ship-bitmap
          (- (/ (send ship-bitmap get-width) 2))
          (- (/ (send ship-bitmap get-height) 2)))))


; assuming dc is already centered on middle of ship and rotated for the ship
;(define (draw-pod dc pod)
;  (keep-transform dc
;    (send dc set-pen fgcolor 1 'solid)
;    (send dc rotate (- (pod-angle pod)))
;    (send dc translate (pod-dist pod) 0)
;    (send dc draw-ellipse -5 -5 10 10)))


(define (draw-sector dc space stack)
  (keep-transform dc
    (define max-x (/ (space-width space) 2))
    (define max-y (/ (space-height space) 2))
    (define scale (min (/ WIDTH (space-width space)) (/ HEIGHT (space-height space))))
    (send dc scale scale scale)
    ;(printf "dc-point-size: ~a\n" (dc-point-size dc))
    (define center (obj #f #f (posvel 0 0 0 0 0 0 0)))
    (define cc (linear-color "blue" "blue" 1.0 0.25))
    (send dc set-pen cc (/ 2 (dc-point-size dc)) 'solid)
    (define sw 500)
    
    (send dc draw-line 0 (- max-y) 0 max-y)
    (for ((i (in-range 1 (+ 1 (inexact->exact (floor (/ max-x sw)))))))
      (send dc draw-line (* sw i) (- max-y) (* sw i) max-y)
      (send dc draw-line (* sw (- i)) (- max-y) (* sw (- i)) max-y))
    
    (send dc draw-line (- max-x) 0 max-x 0)
    (for ((i (in-range 1 (+ 1 (inexact->exact (floor (/ max-y sw)))))))
      (send dc draw-line (- max-x) (* sw i) max-x (* sw i))
      (send dc draw-line (- max-x) (* sw (- i)) max-x (* sw (- i))))
    
    (for ((o (in-list (space-objects space))))
      (draw-object dc o center space #t))
    )
  
  (define buttons (list leave-button))
  
  (cond 
    ((not stack)
     (define start-stacks
       (search space (lambda (o) (and (multipod? o)
                                      (multipod-start? o))) #t))
     
     (set! start-stacks (filter (lambda (s) (ship-flying? (get-ship s))) start-stacks))
     
     (for ((s (in-list start-stacks))
           (i (in-naturals)))
       (define mp (car s))
       (define b (button (+ LEFT 100 (* i 250)) (+ BOTTOM 60) 200 30 5 5 (ob-id mp)
                         (format "~a on ~a" (role-name (pod-role mp))
                                 (ship-name (get-ship s)))))
       (set! buttons (append buttons (list b)))))
    (else
     (set! buttons (append buttons (list (sector-button))))))
  
  buttons)


(define (draw-observer dc space stack serverspace)
  (cond
    ((unbox viewing-sector?)
     (draw-sector dc space stack))
    (else
     (draw-view dc (get-center stack) space)
     (when serverspace (draw-server-objects dc (get-center stack) serverspace))
     (draw-hud dc (get-ship stack) #f)
     (for ((p (in-list (ship-pods (get-ship stack))))
           (i (in-naturals)))
       (define pod-color
         (cond (((pod-energy p) . < . (* (pod-maxe p) (/ 1.0 3.0))) "red")
               (((pod-energy p) . < . (* (pod-maxe p) (/ 2.0 3.0))) "yellow")
               (else "green")))
       (send dc draw-text (role-name (pod-role p)) (/ (- WIDTH) 2) (+ (/ (- HEIGHT) 2) (* (+ i 5) 20)))
       (draw-fraction-box dc (+ (/ (- WIDTH) 2) 70) (+ (/ (- HEIGHT) 2) (* (+ i 5) 20))
                          (pod-maxe p) 20 (pod-energy p) pod-color))))
    
  (list leave-button (sector-button)))


(define (draw-overlay dc space stack)
  (send dc set-text-foreground "white")
  (when stack
    (define role (get-role stack))
    (define str (format "~a" (role-name role)))
    (for ((s (in-list (get-ships stack))))
      (set! str (format "~a on ~a" str (ship-name s))))
    (keep-transform dc
      (send dc translate 0 (/ (- HEIGHT) 2))
      (draw-text dc str 0 0)))
  
  (when space
    (define max 6)
    (define num 0)
    (let loop ((l (space-objects space)))
      (when (and (not (null? l)) (num . < . max))
        (when (message? (car l))
          (define m (car l))
          (set! num (+ num 1))
          (keep-transform dc
            (send dc translate (- (/ WIDTH 2)) (+ (/ HEIGHT 2) (* (+ max 7) -20) (* num 20)))
            (define z (linear-fade (obj-age space m) (/ MSG_FADE_TIME 2) MSG_FADE_TIME))
            (define cc (linear-color "white" "white" z z))
            (send dc set-text-foreground cc)
            (draw-text dc (message-msg m) 0 0)))
        (loop (cdr l))))))

(define (draw-fraction-box dc x y width height fill-width fill-color)
  (send dc set-pen nocolor 1 'transparent)
  (send dc set-brush fill-color 'solid)
  (send dc draw-rectangle x (+ y 1) (max 0.0 fill-width) (- height 2))
  (send dc set-pen fgcolor 1.5 'solid)
  (send dc set-brush fill-color 'transparent)
  (send dc draw-rectangle x (+ y 1) width (- height 2)))

(define (draw-hud dc ship pod)
  (define boxx 40)
  (define ydrop 20)
  (send dc set-text-foreground "white")
  (define con-color
    (cond (((ship-con ship) . < . (* (ship-maxcon ship) (/ 1.0 3.0))) "red")
          (((ship-con ship) . < . (* (ship-maxcon ship) (/ 2.0 3.0))) "yellow")
          (else "green")))
  (define startx (- (/ WIDTH 2)))
  (define starty (+ (/ (- HEIGHT) 2) ydrop))
  (send dc draw-text "Hull" startx starty)
  (draw-fraction-box dc (+ startx boxx) starty (ship-maxcon ship) 20 (ship-con ship) con-color)
  
  (define bat-color
    (cond (((ship-bat ship) . < . (* (ship-maxbat ship) (/ 1.0 3.0))) "red")
          (((ship-bat ship) . < . (* (ship-maxbat ship) (/ 2.0 3.0))) "yellow")
          (else "green")))
  (send dc draw-text "Res" startx (+ starty ydrop))
  (draw-fraction-box dc (+ startx boxx) (+ starty ydrop) (ship-maxbat ship) 20 (ship-bat ship) bat-color)
  
  (when pod
    (define pod-color
      (cond (((pod-energy pod) . < . (* (pod-maxe pod) (/ 1.0 3.0))) "red")
            (((pod-energy pod) . < . (* (pod-maxe pod) (/ 2.0 3.0))) "yellow")
            (else "green")))
    (send dc draw-text (role-name (pod-role pod)) startx (+ starty (* 3 ydrop)))
    (draw-fraction-box dc (+ startx boxx) (+ starty (* 3 ydrop)) (pod-maxe pod) 20 (pod-energy pod) pod-color)))


(define (draw-buttons dc buttons)
  (send dc set-brush "gray" 'solid)
  (send dc set-pen fgcolor 1 'solid)
  (send dc set-text-foreground "black")
  (for ((b (in-list buttons)))
    (keep-transform dc
      (define-values (x y w h) (values (button-x b) (button-y b)
                                       (button-width b) (button-height b)))
      (set! y (- 0.0 y h))
      (send dc draw-rectangle x y w h)
      (send dc translate x y)
      (draw-text dc (button-label b) (button-left-inset b) (button-top-inset b)))))


(define (draw-dmgfx dc stack)
  (define space (get-space stack))
  (define ship (get-ship (reverse stack)))
  (define keep
    (for/list ((d (in-list (ship-dmgfx ship))))
      (case (dmgfx-type d)
        (("translation")
         (define t (* (dmgfx-size d) (linear-fade (obj-age space d) 0 1000)))
         (send dc translate (random-between (- t) t) (random-between (- t) t))
         (if (t . > . 0) d #f))
        (("shear")
         (define t (* (dmgfx-size d) 0.02 (linear-fade (obj-age space d) 0 500)))
         (if ((random) . < . 0.5)
             (send dc transform (vector 1 (random-between (- t) t) 0 1 0 0))
             (send dc transform (vector 1 0 (random-between (- t) t) 1 0 0)))
         (if (t . > . 0) d #f))
        (("rotation")
         (define t (* (dmgfx-size d) 0.02 (linear-fade (obj-age space d) 0 500)))
         (send dc rotate (random-between (- t) t))
         (if (t . > . 0) d #f))
        (("fade")
         (define t (linear-fade (obj-age space d) 0 (* 30 (dmgfx-size d))))
         (send dc set-alpha (- 1.0 t))
         (if (t . > . 0) d #f))
        (("flicker")
         (define t (linear-fade (obj-age space d) 0 (* 100 (dmgfx-size d))))
         (send dc set-alpha (if ((random) . < . 0.3) 0 1))
         (if (t . > . 0) d #f)))))
  
  (set-ship-dmgfx! ship (filter values keep)))

