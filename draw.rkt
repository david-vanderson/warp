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
         "ships.rkt")

(provide (all-defined-out))

(define stars1-bitmap (make-bitmap 1 1))
(send stars1-bitmap load-file "images/stars.png" 'png/alpha)

(define background-bitmap (make-bitmap 1 1))
(send background-bitmap load-file "images/background.jpg")


(define (add-frame-time current-time frames)
  (cons current-time (take frames (min 10 (length frames)))))

(define (draw-framerate dc frames)
  (when ((length frames) . > . 1)
    (keep-transform dc
      (send dc translate (- (/ WIDTH 2)) (/ HEIGHT 2))
      (send dc scale 1 -1)
      (define start (list-ref frames (- (length frames) 1)))
      (define end (first frames))
      (define span (/ (- end start) 1000))
      (send dc draw-text (format "~a" (truncate (/ (- (length frames) 1) span))) 0 0))))


(define (draw-background dc space center bitmap scale parallax width height)
  (define repeatx (* scale (send bitmap get-width)))
  (define repeaty (* scale (send bitmap get-height)))
  (define x (remain (* parallax (posvel-x (obj-posvel center))) repeatx))
  (define y (remain (* parallax (posvel-y (obj-posvel center))) repeaty))
  
  ;(set! width (- width 20))
  ;(set! height (- height 20))
  (define w/2 (/ width 2))
  (define h/2 (/ height 2))
  
  (define istart (- (ceiling (- (/ (- w/2 x) repeatx) 0.5))))
  (define iend (add1 (ceiling (- (/ (+ w/2 x) repeatx) 0.5))))
  (define kstart (- (ceiling (- (/ (- h/2 y) repeaty) 0.5))))
  (define kend (add1 (ceiling (- (/ (+ h/2 y) repeaty) 0.5))))
  ;(printf "i from ~a to ~a\n" istart iend)
  ;(printf "k from ~a to ~a\n" kstart kend)
  
  (for* ((i (in-range istart iend))
         (k (in-range kstart kend)))
    (define xx (- x (* i repeatx)))
    (define yy (- y (* k repeaty)))
    (keep-transform dc
      
;      (send dc translate (- xx) (- yy))
;      (send dc scale scale scale)
;      (send dc translate (- (/ (send bitmap get-width) 2)) (/ (send bitmap get-height) 2))
;      (send dc scale 1 -1)
;      (send dc draw-bitmap bitmap 0 0)
      
      (define xorig (- 0 xx (/ repeatx 2)))
      (define yorig (- 0 (- yy) (/ repeaty 2)))
      (define xscreen (if (xorig . < . (- w/2))
                          (+ (- w/2) (- scale) (remain (- xorig (- w/2)) scale))
                          xorig))
      (define yscreen (if (yorig . < . (- h/2))
                          (+ (- h/2) (- scale) (remain (- yorig (- h/2)) scale))
                          yorig))
      (define xbitmap (max 0 (- xscreen xorig)))
      (define ybitmap (max 0 (- yscreen yorig)))
      (define xbitmapsize (- w/2 xscreen))
      (define ybitmapsize (- h/2 yscreen))
      ;(printf "xorig ~a yorig ~a xscreen ~a yscreen ~a xbitmap ~a ybitmap ~a\n" xorig yorig xscreen yscreen xbitmap ybitmap)
      
      (send dc scale scale scale)
      (send dc scale 1 -1)
      (send dc draw-bitmap-section bitmap (/ xscreen scale) (/ yscreen scale)
            (/ xbitmap scale) (/ ybitmap scale) (/ xbitmapsize scale) (/ ybitmapsize scale))
      
      ))
  
;  (send dc set-pen "white" 1 'solid)
;  (send dc set-brush nocolor 'transparent)
;  (send dc draw-rectangle (- (/ width 2)) (- (/ height 2)) width height)
  
  )


(define (draw-background-stars dc space center parallax width height)
  (define repeatx 1000)
  (define repeaty 1000)
  (define x (remain (* parallax (posvel-x (obj-posvel center))) repeatx))
  (define y (remain (* parallax (posvel-y (obj-posvel center))) repeaty))
  
  ;(set! width (/ width 2))
  ;(set! height (/ height 2))
  (define w/2 (/ width 2))
  (define h/2 (/ height 2))
  
  (define istart (- (ceiling (- (/ (- w/2 x) repeatx) 0.5))))
  (define iend (add1 (ceiling (- (/ (+ w/2 x) repeatx) 0.5))))
  (define kstart (- (ceiling (- (/ (- h/2 y) repeaty) 0.5))))
  (define kend (add1 (ceiling (- (/ (+ h/2 y) repeaty) 0.5))))
  
  (send dc set-pen "white" 1 'solid)
  
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
  
  (for* ((i (in-range istart iend))
         (k (in-range kstart kend)))
    (define xx (- x (* i repeatx)))
    (define yy (- y (* k repeaty)))
    
    (for ((s stars))
      (define sx (- (car s) xx))
      (define sy (- (cdr s) yy))
      (when (and (< (- w/2) sx w/2)
                 (< (- h/2) sy h/2))
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
           (send dc draw-point x y))
         (draw-plasma dc o center space)))
    ((shield? o)
     (draw-shield dc space center o))
    ((effect? o)
     (if map
         (void)
         (draw-effect dc space center o)))))


(define (draw-server-objects dc center space)
  (send dc set-pen "hotpink" 1 'solid)
  (send dc set-brush nocolor 'transparent)
  (for ((o (space-objects space)))
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
  
  (for ((o (append backeffects ships other effects)))
    (draw-object dc o center space)))


(define (draw-ship dc s center)
  (keep-transform dc
    (define-values (x y) (recenter center s))
    (send dc translate x y)
    
    (keep-transform dc
      (send dc rotate (- (posvel-r (obj-posvel s))))
      (send dc set-pen fgcolor 1 'solid)
      (send dc set-brush nocolor 'transparent)
      (define ship-bitmap (get-ship-bitmap s))
      (send dc scale 1 -1)
      (send dc draw-bitmap
              ship-bitmap
              (- (/ (send ship-bitmap get-width) 2))
              (- (/ (send ship-bitmap get-height) 2)))
      
;      (for ((pod (ship-pods s)))
;        (draw-pod dc pod)))
    
;    (define scale 0.5)
;    (send dc scale scale (- scale))
;    (define text (~r (ship-con s) #:precision 0))
;    (define-values (w h b v) (send dc get-text-extent text #f #t))
;    (send dc translate (* -0.5 w) (* -0.5 h))
;    (send dc set-pen nocolor 1 'transparent)
;    (send dc set-brush fgcolor 'solid)
;    (let ((p (new dc-path%)))
;      (send p text-outline (send dc get-font) text 0 0)
;      (send dc draw-path p 0 0))
    )))


; assuming dc is already centered on middle of ship and rotated for the ship
;(define (draw-pod dc pod)
;  (keep-transform dc
;    (send dc set-pen fgcolor 1 'solid)
;    (send dc rotate (- (pod-angle pod)))
;    (send dc translate (pod-dist pod) 0)
;    (send dc draw-ellipse -5 -5 10 10)))


(define (draw-no-role dc space)
  (keep-transform dc
    (define max-x (space-width space))
    (define max-y (space-height space))
    (define scale (min (/ WIDTH max-x) (/ HEIGHT max-y)))
    (send dc scale scale scale)
    ;(printf "dc-point-size: ~a\n" (dc-point-size dc))
    (define center (obj #f #f (posvel 0 0 0 0 0 0 0)))
    ;(draw-background dc space center background-bitmap 4 1 max-x max-y)
    ;(draw-background dc space center stars1-bitmap 8 2 max-x max-y)
    (define cc (linear-color "blue" "blue" 1.0 0.25))
    (send dc set-pen cc (/ 2 (dc-point-size dc)) 'solid)
    (define sw 500)
    (for ((i (inexact->exact (round (/ max-x sw)))))
      (send dc draw-line (* sw i) (- (/ max-y 2)) (* sw i) (/ max-y 2)))
    (for ((i (inexact->exact (round (/ (- max-x sw) sw)))))
      (send dc draw-line (- (* sw i)) (- (/ max-y 2)) (- (* sw i)) (/ max-y 2)))
    
    (for ((i (inexact->exact (round (/ max-y sw)))))
      (send dc draw-line (- (/ max-x 2)) (* sw i) (/ max-x 2) (* sw i)))
    (for ((i (inexact->exact (round (/ (- max-y sw) sw)))))
      (send dc draw-line (- (/ max-x 2)) (- (* sw i)) (/ max-x 2) (- (* sw i))))
    
    (for ((o (space-objects space)))
      (draw-object dc o center space #t)))
  
  (define buttons (list leave-button))
  
  (define start-stacks
    (search space (lambda (o) (and (multipod? o)
                                   (multipod-start? o))) #t))
  
  (set! start-stacks (filter (lambda (s) (obj-posvel (get-ship s))) start-stacks))
  
  (for ((s start-stacks)
        (i (in-naturals)))
    (define mp (car s))
    (define b (button (+ LEFT 100 (* i 250)) (+ BOTTOM 60) 200 30 5 5 (ob-id mp)
                      (format "~a on ~a" (role-name (pod-role mp))
                              (ship-name (get-ship s)))))
    (set! buttons (append buttons (list b))))
  buttons)


(define (draw-observer dc space stack serverspace)
  (draw-view dc (get-center stack) space)
  (when serverspace (draw-server-objects dc (get-center stack) serverspace))
  (draw-hud dc (get-ship stack) #f)
  (for ((p (ship-pods (get-ship stack)))
        (i (in-naturals)))
    (define e (inexact->exact (round (pod-energy p))))
    (draw-hud-status-text dc (+ 10 i) (format "~a ~a" (role-name (pod-role p)) e)))
  (list leave-button))


(define (draw-overlay dc space stack)
  (when stack
    (define role (get-role stack))
    (define str (format "~a" (role-name role)))
    (for ((s (get-ships stack)))
      (set! str (format "~a on ~a" str (ship-name s))))
    (keep-transform dc
      (send dc translate 0 (/ HEIGHT 2))
      (send dc scale 1 -1)
      (send dc draw-text str 0 0)))
  
  (when space
    (define max 8)
    (define msgs (filter message? (space-objects space)))
    (set! msgs (take (reverse msgs) (min 8 (length msgs))))
    (for ((m msgs) (i max))
      (keep-transform dc
        (send dc translate (- (/ WIDTH 2)) (- (/ HEIGHT 2)))
        (send dc translate 0 (* (+ max 6) 20))
        (send dc translate 0 (* i -20))
        (send dc scale 1 -1)
        (define z (linear-fade (obj-age space m) (/ MSG_FADE_TIME 2) MSG_FADE_TIME))
        (define cc (linear-color "white" "white" z z))
        (send dc set-text-foreground cc)
        (send dc draw-text (message-msg m) 0 0)))
    (send dc set-text-foreground "white")))


(define (draw-hud dc ship pod)
  (draw-hud-status-text dc 1 (format "Ship Hull ~a" (inexact->exact (round (ship-con ship)))))
  (draw-hud-status-text dc 2 (format "Reactor   ~a" (inexact->exact (round (ship-power ship)))))
  (draw-hud-status-text dc 3 (format "Reserve   ~a" (inexact->exact (round (ship-bat ship)))))
  (when pod
    (define e (inexact->exact (round (pod-energy pod))))
    (draw-hud-status-text dc 5 (format "Pod Bat ~a" e))))


(define (draw-buttons dc buttons)
  (for ((b buttons))
    (keep-transform dc
      (define-values (x y w h) (values (button-x b) (button-y b)
                                       (button-width b) (button-height b)))
      (send dc set-brush "darkgray" 'solid)
      (send dc set-pen fgcolor 1 'solid)
      (send dc draw-rectangle x y w h)
      (send dc translate x (+ y h))
      (send dc scale 1 -1)
      (send dc draw-text (button-label b) (button-left-inset b) (button-top-inset b)))))


(define (draw-dmgfx dc stack)
  (define space (get-space stack))
  (define ship (get-ship (reverse stack)))
  (define keep
    (for/list ((d (ship-dmgfx ship)))
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

