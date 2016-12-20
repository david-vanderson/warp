#lang racket/base

(require racket/class
         racket/list
         racket/math
         mode-lambda
         racket/draw)

(require "defs.rkt"
         "utils.rkt"
         "draw-utils.rkt"
         "plasma.rkt"
         "missile.rkt"
         "shield.rkt"
         "effect.rkt"
         "ships.rkt"
         "pbolt.rkt"
         "warp.rkt"
         "upgrade.rkt")

(provide (all-defined-out))


(define (add-frame-time current-time frames)
  (cons current-time (take frames (min 10 (length frames)))))


(define (random-stars n)
  (define W 700.0)
  (for*/list ((i n) (k n))
    (define w (/ W n))
    (define w/2 (/ w 2.0))
    (define mx (+ (* i w) w/2))
    (define my (+ (* k w) w/2))
    (define rx (- (* w (random)) w/2))
    (define ry (- (* w (random)) w/2))
    (cons (inexact->exact (truncate (- (+ mx rx) (/ W 2.0))))
          (inexact->exact (truncate (- (+ my ry) (/ W 2.0)))))))

(define stars1 '(
  (-294 . -295) (-240 . -227) (-296 . -92) (-303 . 44) (-328 . 205) (-348 . 332)
  (-186 . -270) (-174 . -185) (-231 . -59) (-158 . 90) (-125 . 128) (-190 . 282)
  (-8 . -260) (-49 . -161) (-30 . -1) (-64 . 19) (-41 . 210) (-30 . 343)
  (86 . -248) (55 . -130) (108 . -57) (9 . 49) (54 . 174) (59 . 325)
  (137 . -242) (177 . -178) (144 . -66) (171 . 95) (130 . 144) (169 . 300)
  (263 . -311) (329 . -163) (346 . -41) (292 . 4) (349 . 148) (317 . 256)))
(define stars2 '(
  (-269 . -254) (-318 . -148) (-226 . 28) (-260 . 188) (-267 . 227)
  (-183 . -272) (-187 . -103) (-101 . -16) (-100 . 83) (-171 . 229)
  (-69 . -214) (-56 . -73) (-15 . -21) (-7 . 79) (-52 . 316)
  (207 . -313) (166 . -80) (98 . 67) (77 . 184) (147 . 246)
  (227 . -214) (229 . -188) (316 . 62) (272 . 206) (333 . 250)))
(define stars3 '(
  (-295 . -343) (-176 . -7) (-316 . 115) (-290 . 309)
  (-146 . -263) (-110 . -106) (-16 . 91) (-169 . 176)
  (37 . -302) (153 . -6) (29 . 14) (134 . 231)
  (329 . -216) (216 . -77) (345 . 107) (277 . 328)))
(define stars4 '(
  (-245 . -336) (-274 . -96) (-313 . 300)
  (84 . -196) (17 . 69) (-68 . 270)
  (139 . -329) (218 . -65) (267 . 117)))

(define (new-stars)
  (set! stars1 (random-stars 6))
  (set! stars2 (random-stars 5))
  (set! stars3 (random-stars 4))
  (set! stars4 (random-stars 3))
  )


(define (draw-background-stars csd center scale fowlist)
  (define spr '())
  (define width 700)
  (define height 700)
  (define offx (remain (obj-x center) width))
  (define offy (remain (obj-y center) height))
  (define origx (+ (obj-x center) (- offx) (/ width 2)))
  (define origy (+ (obj-y center) (- offy) (/ height 2)))
  (define w/2 (/ WIDTH 2 scale))
  (define h/2 (/ HEIGHT 2 scale))
  
  (define istart (- 0 (ceiling (/ (max 0 (- w/2 offx)) width))))
  (define iend (+ 0 (ceiling (/ (max 0 (- w/2 (- width offx))) width)) 1))
  (define kstart (- 0 (ceiling (/ (max 0 (- h/2 offy)) height)) 1))
  (define kend (+ 0 (ceiling (/ (max 0 (- h/2 (- height offy))) height)) 1))

  (for ((x (in-list '(0.7 1.0 1.5 2.5)))
        (stars (in-list (list stars1 stars2 stars3 stars4))))
    (define pw (* x scale))
    (when (pw . > . 1.0)
      ;(printf "stars ~a ~v\n" x stars)
      (define m
        (cond ((pw . > . 3.0) 2.0)
              (else 1.0)))
      (for* ((i (in-range istart iend))
             (k (in-range kstart kend))
             (s (in-list stars)))
        (define x (+ origx (* i width) (car s)))
        (define y (+ origy (* k height) (cdr s)))
        (when ((get-alpha x y fowlist) . = . 1.0)
          (define-values (sx sy) (xy->screen x y center scale))
          (append! spr (sprite sx sy (sprite-idx csd 'star)
                               #:layer LAYER_MAP #:m m)))
        )))
  spr)

(define (draw-object csd textr center scale o space pid showplayers? fowa layer_effects)
  (define spr '())
  (cond       
    #;((ptsize . < . 0.25)  ; "sector" view - ships are triangles
     (cond ((ship? o)
            (define col
              (if (find-id o pid)
                  (linear-color "blue" "blue" 1.0
                                (+ 0.5 (* 0.5 (cycletri (space-time space) 1500))))
                  "blue"))
            (define outline
              (cond ((spacesuit? o)
                     ; diamond, but small enough so it looks like a dot
                     '((-1 . 0) (0 . 1) (1 . 0) (0 . -1) (-1 . 0)))
                    ((find-id o fthrust? #f)
                     '((-1 . -2) (-1 . 2) (5 . 0) (-1 . -2)))  ; engines -> triangle
                    (else
                     '((-5 . -5) (-5 . 5) (5 . 5) (5 . -5) (-5 . -5)))))  ; no engines -> square
            (define zoutline
              (for/list ((x (in-list outline)))
                (cons (* (car x) (ship-radius o) (/ 0.15 (sqrt ptsize)))
                      (* (cdr x) (ship-radius o) (/ 0.15 (sqrt ptsize))))))
            (keep-transform dc
              (center-on dc o #f)
              (when showplayers? (draw-playerlist dc o))
              (send dc rotate (- (obj-r o)))
              (send dc set-pen col (/ 1.5 ptsize) 'solid)
              (send dc draw-lines zoutline)))
           ((upgrade? o)
            (send dc set-pen (upgrade-color o) (/ 1.5 ptsize) 'transparent)
            (send dc draw-point (obj-x o) (obj-y o)))
           ((missile? o)
            (send dc set-pen "red" (/ 2.5 (dc-point-size dc)) 'solid)
            (send dc draw-point (obj-x o) (obj-y o)))
           ((plasma? o)
            (send dc set-pen "orange" (/ 2.0 (dc-point-size dc)) 'solid)
            (send dc draw-point (obj-x o) (obj-y o)))))
    (else
     (cond ((ship? o)
            (define-values (x y) (obj->screen o center scale))
            (append! spr (sprite x y (sprite-idx csd (string->symbol (ship-type o)))
                                 #:layer LAYER_SHIPS #:m scale #:theta (- (obj-r o))
                                 #:a fowa #:r (get-red space o)))
            (append! spr (draw-ship-info csd center scale o (obj-x o) (obj-y o) space fowa layer_effects))
            (when showplayers?
              (append! spr (draw-playerlist csd textr center scale o))))
           ((plasma? o)
            (append! spr (draw-plasma csd center scale o space fowa)))
           ((missile? o)
            (append! spr (draw-missile csd center scale o space fowa)))
           ((shield? o)
            (append! spr (draw-shield csd center scale space o fowa)))
           ((effect? o)
            (append! spr (draw-effect csd center scale space o fowa layer_effects)))
           ((upgrade? o)
            (append! spr (draw-upgrade csd center scale space o fowa))))))
  spr)


(define (draw-playerlist csd textr center scale ship)
  (define spr '())
  (define players (find-all ship player?))
  (append! players (player -1 "player1" "fac1") (player -1 "player2" "fac2"))
  (when (not (null? players))
    (define-values (sx sy) (obj->screen ship center scale))
    (append! spr (sprite (+ sx 25.0) (+ sy 25.0) (sprite-idx csd 'square) #:layer LAYER_MAP
                         #:mx (/ 71.0 (sprite-width csd (sprite-idx csd 'square)))
                         #:my (/ 2.0 (sprite-height csd (sprite-idx csd 'square)))
                         #:b 255 #:theta (/ pi 4)))
    (append! spr (sprite (+ sx 85.0) (+ sy 50.0) (sprite-idx csd 'square) #:layer LAYER_MAP
                         #:mx (/ 70.0 (sprite-width csd (sprite-idx csd 'square)))
                         #:my (/ 2.0 (sprite-height csd (sprite-idx csd 'square)))
                         #:b 255))
    (for ((p players) (i (in-naturals)))
      (append! spr (text-sprite textr (player-name p)
                                (+ sx 55.0) (+ sy 55.0 (* i 20))
                                LAYER_MAP 1.0 "blue"))))
  spr)
    
   

#;(define (draw-server-objects dc center space)
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


(define (draw-sector-lines csd center scale space)
  (define spr '())
  (define max-x (/ (space-width space) 2))
  (define max-y (/ (space-height space) 2))
  
  (define sw 1000)
  
  (define-values (x y) (xy->screen 0.0 0.0 center scale))
  (append! spr (sprite x y (sprite-idx csd 'blue) #:layer LAYER_MAP #:my (* max-y scale) #:a 0.8))
  (for ((i (in-range 1 (+ 1 (inexact->exact (floor (/ max-x sw)))))))
    (define-values (x y) (xy->screen (exact->inexact (* sw i)) 0.0 center scale))
    (append! spr (sprite x y (sprite-idx csd 'blue) #:layer LAYER_MAP #:my (* max-y scale) #:a 0.8))
    (define-values (x2 y2) (xy->screen (exact->inexact (* sw (- i))) 0.0 center scale))
    (append! spr (sprite x2 y2 (sprite-idx csd 'blue) #:layer LAYER_MAP #:my (* max-y scale) #:a 0.8)))
  
  (append! spr (sprite x y (sprite-idx csd 'blue) #:layer LAYER_MAP #:mx (* max-x scale) #:a 0.8))
  (for ((i (in-range 1 (+ 1 (inexact->exact (floor (/ max-y sw)))))))
    (define-values (x y) (xy->screen 0.0 (exact->inexact (* sw i)) center scale))
    (append! spr (sprite x y (sprite-idx csd 'blue) #:layer LAYER_MAP #:mx (* max-x scale) #:a 0.8))
    (define-values (x2 y2) (xy->screen 0.0 (exact->inexact (* sw (- i))) center scale))
    (append! spr (sprite x2 y2 (sprite-idx csd 'blue) #:layer LAYER_MAP #:mx (* max-x scale) #:a 0.8)))
  spr)



(define (draw-overlay textr space stack)
  (define spr '())
  ; string saying where you are
  (when stack
    (define p (get-pod stack))
    (define str (format "~a" (if p (pod-name p) "Crew")))
    (for ((s (in-list (get-ships stack))))
      (set! str (format "~a on ~a" str (ship-name s))))
    (append! spr (textr str 0.0 (- 5.0 (/ HEIGHT 2.0)) #:layer LAYER_UI_TEXT
                        #:r 255 #:g 255 #:b 255)))

  ; messages
  (when space
    (define max 6)
    (define num 0)
    (let loop ((l (space-objects space)))
      (when (and (not (null? l)) (num . < . max))
        (when (message? (car l))
          (define m (car l))
          (set! num (+ num 1))
          (define z (linear-fade (obj-age space m) (/ MSG_FADE_TIME 2) MSG_FADE_TIME))
          (append! spr (text-sprite textr (message-msg m)
                                    (- 5.0 (/ WIDTH 2.0)) (+ -200.0 (* num 20))
                                    LAYER_UI_TEXT z)))
        (loop (cdr l)))))
  spr)


(define (stoplight-color v max)
  (cond ((v . < . (* max (/ 1.0 3.0))) "red")
        ((v . < . (* max (/ 2.0 3.0))) "yellow")
        (else "green")))


(define (button-sprites csd textr buttons time)
  (define spr '())
  (for ((b (in-list buttons))
        #:when (not (equal? (button-draw b) 'hidden)))
    (define-values (x y w h) (values (exact->inexact (button-x b))
                                     (exact->inexact (button-y b))
                                     (exact->inexact (button-width b))
                                     (button-height b)))

    (when h (set! h (exact->inexact h)))

    (define sprname (if (dmgbutton? b) 'dmgbutton-normal 'button-normal))
    (define txtcol 255)
    (case (button-draw b)
      ((disabled dmg)
       (set! sprname 'button-disabled)
       (set! txtcol 150))
      ((outline)
       (set! sprname 'button-outline)))

    (when (not h)
      (set! sprname (string->symbol (string-append (symbol->string sprname) "-circle"))))

    (define br 0)
    (when (dmgbutton? b)
      (set! br
            (if (dmgbutton-fixing? b) 255
                (if (time-toggle time 1000) 255 100)))
      (append! spr (sprite x y (sprite-idx csd 'dmgbutton-fill) #:layer LAYER_UI
                         #:mx (/ w (sprite-width csd (sprite-idx csd 'dmgbutton-fill)) 1.0)
                         #:my (/ (exact->inexact (* h (dmgbutton-frac b)))
                                 (sprite-height csd (sprite-idx csd 'dmgbutton-fill)) 1.0)
                         #:r br))
      )

    (append! spr (sprite x y (sprite-idx csd sprname) #:layer LAYER_UI
                         #:mx (/ w (sprite-width csd (sprite-idx csd sprname)) 1.0)
                         #:my (/ (if h h w) (sprite-height csd (sprite-idx csd sprname)) 1.0)
                         #:r br))
    (append! spr (textr (button-label b) x (- y (/ TEXTH 2.0)) #:layer LAYER_UI_TEXT
                        #:r txtcol #:g txtcol #:b txtcol)))
  spr)


(define (draw-ship-info csd center scale ship wx wy space a layer)
  (define spr '())
  (define shipr (if (ship-flying? ship) (obj-r ship) pi/2))
  (for ((p (in-list (ship-pods ship))))
    (define r (angle-add shipr (pod-angle p)))
    (define x (+ wx (* (pod-dist p) (cos r))))
    (define y (+ wy (* (pod-dist p) (sin r))))
    (define rr (angle-add shipr (if (pod-facing p) (pod-facing p) 0)))
    (define-values (sx sy) (xy->screen x y center scale))
    (define m (* scale (/ (pod-e p) (pod-maxe p) 75.0)))
    (define col (send the-color-database find-color (stoplight-color (pod-e p) (pod-maxe p))))
    (append! spr (sprite sx sy (sprite-idx csd 'podarc)
                         #:layer layer #:m m #:theta (- rr) #:a a
                         #:r (send col red) #:g (send col green) #:b (send col blue)))
    (define ndmgs (length (search p dmg? #t #f)))
    (when (ndmgs . > . 0)
      (define da (degrees->radians 40.0))
      (define sa (- (* (- ndmgs 1) (/ da 2.0))))
      (for ((i ndmgs))
        (define dr (angle-add rr (angle-add sa (* i da))))
        (define dx (+ x (* 8.0 (cos dr))))
        (define dy (+ y (* 8.0 (sin dr))))
        (define-values (sx sy) (xy->screen dx dy center scale))
        (append! spr (sprite sx sy (sprite-idx csd 'circle) #:r 255
                             #:layer layer #:m (/ scale 500.0) #:a a)))))
  spr)



(define (draw-pods csd center scale ship where stack send-commands canvas meid)
  (define buttons '())
  (define shipr (if (ship-flying? ship) (obj-r ship) pi/2))
  (for ((p (in-list (ship-pods ship)))
        #:when (not (lounge? p)))
    (define r (angle-add shipr (pod-angle p)))
    (define x (+ (obj-x where) (* (pod-dist p) (cos r))))
    (define y (+ (obj-y where) (* (pod-dist p) (sin r))))
    (define-values (sx sy) (xy->screen x y center scale))
    (define size (min (* scale 10.0) (* scale scale 1.0)))
    (when (size . > . 12.0)
      (cond
        ((hangar? p)
         (define b (button 'normal #f sx sy size size (pod-name p)
                           (lambda (x y)
                             (send-commands (chrole meid (ob-id p))))))
         (append! buttons (list b)))
        (else
         (define b (button 'normal #f sx sy size #f (pod-name p)
                           (lambda (x y)
                             (send-commands (chrole meid (ob-id p))))))
         (when (pod-player p)
           (set-button-draw! b 'disabled)
           (set-button-label! b (player-name (pod-player p))))
         (append! buttons b)))))
  buttons)


(define (draw-docking csd center scale space stack)
  (define spr '())
  (define ship (get-ship stack))
  (define col (send the-color-database find-color "hotpink"))
  (for ((s (in-list (space-objects space))))
    (when (and (spaceship? s)
               (not (= (ob-id ship) (ob-id s)))
               (will-dock? ship s))
      (define-values (x y) (obj->screen s center scale))
      (append! spr (sprite x y (sprite-idx csd 'circle-outline)
                           #:layer LAYER_OVERLAY #:m (/ scale 20.0)
                           #:r (send col red) #:g (send col green) #:b (send col blue)))))
  spr)


(define (draw-tool-overlay csd center scale t stack)
  (cond
    ((dock? t)
     (when (dock-on t)
       (draw-docking csd center scale (get-space stack) stack)))
    ((pbolt? t)
     (define spr '())
     (define s (get-ship stack))
     (when (ship-flying? s)
       (define p (get-pod stack))
       (when (pod-facing p)
         (define r (angle-add (obj-r s) (pod-angle p)))
         (define x (+ (obj-x s) (* (pod-dist p) (cos r))))
         (define y (+ (obj-y s) (* (pod-dist p) (sin r))))
         (define rr (angle-add (obj-r s) (pod-facing p)))
         (define len 50.0)
         (for ((a (list (angle-add rr (/ (pod-spread p) 2))
                        (angle-add rr (- (/ (pod-spread p) 2))))))
           (define-values (lx ly) (xy->screen (+ x (* len (cos a))) (+ y (* len (sin a)))
                                              center scale))
           (append! spr (sprite lx ly (sprite-idx csd 'square) #:layer LAYER_OVERLAY
                         #:mx (/ (* len scale) 0.5 (sprite-width csd (sprite-idx csd 'square)))
                         #:my (/ 2.0 (sprite-height csd (sprite-idx csd 'square)))
                         #:r 255 #:theta (- a))))))
     spr)))


; drawn in canon transform
(define (draw-pod-ui csd textr center scale stack)
  (define spr '())
  
  ; pod energy
  (define p (get-pod stack))
  (when ((pod-maxe p) . > . 0)
    (append! spr (sprite (+ LEFT 20) (- BOTTOM 10 (/ (pod-maxe p) 2)) (sprite-idx csd 'square-outline)
                         #:layer LAYER_UI
                         #:mx (/ 22.0 (sprite-width csd (sprite-idx csd 'square-outline)))
                         #:my (/ (+ 2.0 (pod-maxe p)) (sprite-height csd (sprite-idx csd 'square-outline)) 1.0)
                         #:r 255 #:g 255 #:b 255))
    (define col (send the-color-database find-color (stoplight-color (pod-e p) (pod-maxe p))))
    (append! spr (sprite (+ LEFT 20) (- BOTTOM 10 (/ (pod-e p) 2)) (sprite-idx csd 'square)
                         #:layer LAYER_UI #:mx (/ 20.0 (sprite-width csd (sprite-idx csd 'square)))
                         #:my (/ (pod-e p) (sprite-height csd (sprite-idx csd 'square)) 1.0)
                         #:r (send col red) #:g (send col green) #:b (send col blue))))

  ; ship hp
  (define s (get-ship stack))
  (define mc (ship-maxcon s))
  (append! spr (sprite (- RIGHT 45 (/ (ship-maxcon s) 2)) (+ TOP 20) (sprite-idx csd 'square-outline)
                         #:layer LAYER_UI
                         #:mx (/ (+ 2.0 (ship-maxcon s)) (sprite-width csd (sprite-idx csd 'square-outline)) 1.0)
                         #:my (/ 22.0 (sprite-height csd (sprite-idx csd 'square-outline)))
                         #:r 255 #:g 255 #:b 255))
  (define col (send the-color-database find-color (stoplight-color (ship-con s) mc)))
  (append! spr (sprite (- RIGHT 45 (/ (ship-con s) 2)) (+ TOP 20) (sprite-idx csd 'square)
                         #:layer LAYER_UI #:my (/ 20.0 (sprite-height csd (sprite-idx csd 'square)))
                         #:mx (/ (ship-con s) (sprite-width csd (sprite-idx csd 'square)) 1.0)
                         #:r (send col red) #:g (send col green) #:b (send col blue)))
  (append! spr (textr "Hull" (- RIGHT 20) (+ TOP 10) #:layer LAYER_UI
                      #:r 255 #:g 255 #:b 255))

  ; ship reserves
  (define mb (ship-maxbat s))
  (when (mb . > . 0)
    (append! spr (sprite (- RIGHT 45 (/ (ship-maxbat s) 2)) (+ TOP 50) (sprite-idx csd 'square-outline)
                         #:layer LAYER_UI
                         #:mx (/ (+ 2.0 (ship-maxbat s)) (sprite-width csd (sprite-idx csd 'square-outline)) 1.0)
                         #:my (/ 22.0 (sprite-height csd (sprite-idx csd 'square-outline)))
                         #:r 255 #:g 255 #:b 255))
    (define col (send the-color-database find-color (stoplight-color (ship-bat s) mb)))
    (append! spr (sprite (- RIGHT 45 (/ (ship-bat s) 2)) (+ TOP 50) (sprite-idx csd 'square)
                         #:layer LAYER_UI #:my (/ 20.0 (sprite-height csd (sprite-idx csd 'square)))
                         #:mx (/ (ship-bat s) (sprite-width csd (sprite-idx csd 'square)) 1.0)
                         #:r (send col red) #:g (send col green) #:b (send col blue)))
    (append! spr (textr "Res" (- RIGHT 25) (+ TOP 40) #:layer LAYER_UI
                        #:r 255 #:g 255 #:b 255)))
  spr)


; drawn in canon transform (buttons, dmgs, warnings)
(define (draw-tool-ui csd center scale space t stack send-commands)
  (define buttons '())
  (define spr '())
  (cond
    ((pbolt? t) (append! buttons (draw-pbolt-ui! t stack send-commands)))
    ((warp? t)
     (define-values (bs ss) (draw-warp-ui! csd center scale t stack send-commands))
     (append! buttons bs)
     (append! spr ss))
    ((mtube? t)
     (define-values (bs ss) (draw-mtube-ui! csd center scale space t stack send-commands))
     (append! buttons bs)
     (append! spr ss))
    ((steer? t)
     (define offline (findf (lambda (d) (equal? "offline" (dmg-type d))) (tool-dmgs t)))
     (when offline
       (define ob (dmgbutton 'normal #f
                             0.0 (- BOTTOM 105) 200 30
                             "Steer Offline"
                             (lambda (x y) (send-commands (command (ob-id offline)
                                                                   (not (dmg-fixing? offline)))))
                             (/ (dmg-energy offline) (dmg-size offline)) (dmg-fixing? offline)))
       (append! buttons (list ob))))
    ((fthrust? t)
     (define b (button 'normal #\w 0.0 (- BOTTOM 35) 80 30 (if (fthrust-on t) "Stop [W]" "Go [W]")
                       (lambda (x y) (send-commands (command (ob-id t) (not (fthrust-on t)))))))
     (when (not (ship-flying? (get-ship stack)))
       (set-button-draw! b 'disabled))
     (append! buttons (list b))
     (define ob (add-offline-button! t b send-commands))
     (when ob (append! buttons (list ob))))
     
    ((dock? t)
     (define b (button 'normal #\c -130.0 (- BOTTOM 35) 120.0 30.0 (if (dock-on t) "Docking... [C]" "Dock [C]")
                       (lambda (x y) (send-commands (command (ob-id t) (not (dock-on t)))))))
     (when (not (ship-flying? (get-ship stack)))
       (set-button-draw! b 'disabled))
     (append! buttons (list b))
     (define ob (add-offline-button! t b send-commands))
     (when ob (append! buttons (list ob)))
     (define lb (button (if (can-launch? stack) 'normal 'disabled) #\l -250.0 (- BOTTOM 35) 80.0 30.0 "Launch [L]"
                       (lambda (x y) (send-commands (command (ob-id t) "launch")))))
     (append! buttons (list lb))
     (define lob (add-offline-button! t lb send-commands "nolaunch"))
     (when lob (append! buttons (list lob))))
    
    ((shbolt? t)
     (define ship (get-ship stack))
     (define pod (get-pod stack))
     (define b (button 'normal #\space (+ LEFT 65) (- BOTTOM 35) 50.0 50.0 "Shield [_]" #f))
     (cond
       ((and (ship-flying? ship) ((pod-energy pod) . > . (shbolt-shield-size t)))
        (define a (+ (obj-r ship) (pod-facing (get-pod stack))))
        (set-button-f! b (lambda (x y) (send-commands (command (ob-id t) a)))))
       (else
        (set-button-draw! b 'disabled)))
     (append! buttons (list b))
     (define ob (add-offline-button! t b send-commands))
     (when ob (append! buttons (list ob)))))
  (values buttons spr))


(define (get-dmgfx stack)
  (define x 0.0)
  (define y 0.0)
  (define space (get-space stack))
  (define ship (get-ship (reverse stack)))
  (define keep
    (for/list ((d (in-list (ship-dmgfx ship))))
      (case (dmgfx-type d)
        (("translation")
         (define t (* (dmgfx-size d) (linear-fade (obj-age space d) 0 1000)))
         (set! x (random-between (- t) t))
         (set! y (random-between (- t) t))
         (if (t . > . 0) d #f))
;        (("shear")
;         (define t (* (dmgfx-size d) 0.02 (linear-fade (obj-age space d) 0 500)))
;         (if ((random) . < . 0.5)
;             (send dc transform (vector 1 (random-between (- t) t) 0 1 0 0))
;             (send dc transform (vector 1 0 (random-between (- t) t) 1 0 0)))
;         (if (t . > . 0) d #f))
;        (("rotation")
;         (define t (* (dmgfx-size d) 0.02 (linear-fade (obj-age space d) 0 500)))
;         (send dc rotate (random-between (- t) t))
;         (if (t . > . 0) d #f))
;        (("fade")
;         (define t (linear-fade (obj-age space d) 0 (* 30 (dmgfx-size d))))
;         (send dc set-alpha (- 1.0 t))
;         (if (t . > . 0) d #f))
;        (("flicker")
;         (define t (linear-fade (obj-age space d) 0 (* 100 (dmgfx-size d))))
;         (send dc set-alpha (if ((random) . < . 0.3) 0 1))
;         (if (t . > . 0) d #f))
        )))
  
  (set-ship-dmgfx! ship (filter values keep))
  (values x y))

