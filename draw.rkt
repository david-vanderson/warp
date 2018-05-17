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
         "explosion.rkt"
         "missile.rkt"
         "cannon.rkt"
         "probe.rkt"
         "shield.rkt"
         "effect.rkt"
         "ships.rkt"
         "pbolt.rkt"
         "warp.rkt"
         "upgrade.rkt")

(provide (all-defined-out))


(define (add-frame-time current-time frames)
  (cons current-time (take frames (min 30 (length frames)))))


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
  (define w/2 (/ canon-width 2 scale))
  (define h/2 (/ canon-height 2 scale))
  
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
          (prepend! spr (sprite sx sy (sprite-idx csd 'star)
                                #:layer LAYER_MAP #:m m)))
        )))
  spr)

(define (draw-object csd textr textsr center scale o space pid showplayers? fowa layer_effects faction)
  (cond
    #;((ptsize . < . 0.25)  ; "sector" view - ships are triangles
     (cond ((ship? o)
            (define col
              (if (find-id space o pid)
                  (linear-color "blue" "blue" 1.0
                                (+ 0.5 (* 0.5 (cycletri (space-time space) 1500))))
                  "blue"))
            (define outline
              (cond ((spacesuit? o)
                     ; diamond, but small enough so it looks like a dot
                     '((-1 . 0) (0 . 1) (1 . 0) (0 . -1) (-1 . 0)))
                    ((find-id space o fthrust? #f)
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
     (cond ((plasma? o)
            (draw-plasma csd center scale o space fowa))
           ((explosion? o)
            (draw-explosion csd center scale o space fowa))
           ((shield? o)
            (draw-shield csd center scale space o fowa))
           ((effect? o)
            (draw-effect csd center scale space o fowa layer_effects))
           ((upgrade? o)
            (define spr '())
            (prepend! spr (draw-upgrade csd center scale space o fowa))
            (when showplayers?
              (prepend! spr (draw-cargolist csd textr textsr center scale o "gray" fowa
                                            (list (upgrade-type o)) '())))
            spr)
           ((ship? o)
            (define spr '())
            (prepend! spr (obj-sprite o csd center scale LAYER_SHIPS
                                      (string->symbol (ship-type o))
                                      (ship-info-size (hash-ref ship-list (ship-type o)))
                                      fowa (obj-r o) (make-color (get-red space o) 0 0 1.0)))
            
            ;(prepend! spr (draw-ship-info csd center scale o (obj-x o) (obj-y o) space fowa layer_effects))
            (when showplayers?
              (define colstr (faction-check-color faction (ship-faction o)))
              (define players (find-all space o player?))
              ;(append! players (player -1 "player1" "fac1") (player -1 "player2" "fac2"))
              (prepend! spr (draw-cargolist csd textr textsr center scale o colstr fowa
                                            (cons (ship-name o) (map upgrade-type (ship-cargo o)))
                                            (map player-name players))))
            spr)))))


(define (draw-cargolist csd textr textsr center scale obj colstr fowa text-above text-below)
  (define spr '())
  (define col (send the-color-database find-color colstr))
  (define-values (sx sy) (obj->screen obj center scale))
  (prepend! spr (sprite (+ sx 25.0) (+ sy 25.0) (sprite-idx csd 'square) #:layer LAYER_MAP
                        #:mx (/ 71.0 (sprite-width csd (sprite-idx csd 'square)))
                        #:my (/ 2.0 (sprite-height csd (sprite-idx csd 'square)))
                        #:r (send col red) #:g (send col green) #:b (send col blue)
                        #:theta (/ pi 4) #:a fowa))
  (prepend! spr (sprite (+ sx 85.0) (+ sy 50.0) (sprite-idx csd 'square) #:layer LAYER_MAP
                        #:mx (/ 70.0 (sprite-width csd (sprite-idx csd 'square)))
                        #:my (/ 2.0 (sprite-height csd (sprite-idx csd 'square)))
                        #:r (send col red) #:g (send col green) #:b (send col blue) #:a fowa))
  (for ((t text-above) (i (in-naturals)))
    (prepend! spr (text-sprite textr textsr t
                               (+ sx 55.0) (+ sy 50.0 (- (* (+ i 1) 14)))
                               LAYER_MAP fowa colstr)))
  (for ((t text-below) (i (in-naturals)))
    (prepend! spr (text-sprite textr textsr t
                               (+ sx 55.0) (+ sy 55.0 (* i 14))
                               LAYER_MAP fowa colstr)))
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
  (define mx (* max-x scale 2.0 (/ 1.0 100.0)))
  (define max-y (/ (space-height space) 2))
  (define my (* max-y scale 2.0 (/ 1.0 100.0)))
  
  (define sw 1000)
  (define idx (sprite-idx csd '100x1))
  (define w 2.0)
  
  (define-values (x y) (xy->screen 0.0 0.0 center scale))
  (prepend! spr (sprite x y idx #:layer LAYER_MAP #:mx my #:my w #:theta pi/2 #:b (send mapcol blue)))
  (for ((i (in-range 1 (+ 1 (inexact->exact (floor (/ max-x sw)))))))
    (define-values (x y) (xy->screen (exact->inexact (* sw i)) 0.0 center scale))
    (prepend! spr (sprite x y idx #:layer LAYER_MAP #:mx my #:my w #:theta pi/2 #:b (send mapcol blue)))
    (define-values (x2 y2) (xy->screen (exact->inexact (* sw (- i))) 0.0 center scale))
    (prepend! spr (sprite x2 y2 idx #:layer LAYER_MAP #:mx my #:my w #:theta pi/2 #:b (send mapcol blue))))
  
  (prepend! spr (sprite x y idx #:layer LAYER_MAP #:my w #:mx mx #:b (send mapcol blue)))
  (for ((i (in-range 1 (+ 1 (inexact->exact (floor (/ max-y sw)))))))
    (define-values (x y) (xy->screen 0.0 (exact->inexact (* sw i)) center scale))
    (prepend! spr (sprite x y idx #:layer LAYER_MAP #:my w #:mx mx #:b (send mapcol blue)))
    (define-values (x2 y2) (xy->screen 0.0 (exact->inexact (* sw (- i))) center scale))
    (prepend! spr (sprite x2 y2 idx #:layer LAYER_MAP #:my w #:mx mx #:b (send mapcol blue))))
  spr)


(define (draw-green-corners o csd center scale)
  (define sprites '())
  (define idx (sprite-idx csd '5x1))
  (define w 2.0)
  (define h 2.5)
  (define-values (x y) (obj->screen o center scale))
  (prepend! sprites (sprite (+ x 25 (- h)) (+ y 24)
                            idx #:my w #:layer LAYER_UI #:g 200))
  (prepend! sprites (sprite (+ x 24) (+ y 25 (- h))
                            idx #:my w #:theta pi/2 #:layer LAYER_UI #:g 200))

  (prepend! sprites (sprite (- x 25 (- h)) (+ y 24)
                            idx #:my w #:layer LAYER_UI #:g 200))
  (prepend! sprites (sprite (- x 24) (+ y 25 (- h))
                            idx #:my w #:theta pi/2 #:layer LAYER_UI #:g 200))

  (prepend! sprites (sprite (+ x 25 (- h)) (- y 24)
                            idx #:my w #:layer LAYER_UI #:g 200))
  (prepend! sprites (sprite (+ x 24) (- y 25 (- h))
                            idx #:my w #:theta pi/2 #:layer LAYER_UI #:g 200))

  (prepend! sprites (sprite (- x 25 (- h)) (- y 24)
                            idx #:my w #:layer LAYER_UI #:g 200))
  (prepend! sprites (sprite (- x 24) (- y 25 (- h))
                            idx #:my w #:theta pi/2 #:layer LAYER_UI #:g 200))

  sprites)


(define (stoplight-color v max)
  (cond ((v . < . (* max (/ 1.0 3.0))) "red")
        ((v . < . (* max (/ 2.0 3.0))) "yellow")
        (else "green")))


(define (button-sprites csd textr buttons time holding pressed)
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
    (cond
      ((member (button-draw b) '(outline))
       (set! sprname 'button-outline))
      ((or (member (button-draw b) '(disabled dmg))
           (if (holdbutton? b)
               ; if we are holding the button, draw it as pressed
               (ormap (lambda (h)
                        (equal? (hold-key h) (button-key b)))
                      holding)
               ; if we recently pressed the button, draw it as pressed
               (ormap (lambda (p)
                        (equal? (press-key p) (button-key b)))
                      pressed)))
       (set! sprname 'button-disabled)
       (set! txtcol 150)))

    (when (not h)
      (set! sprname (string->symbol (string-append (symbol->string sprname) "-circle"))))

    (define br 0)
    (when (dmgbutton? b)
      (set! br
            (if (dmgbutton-fixing? b) 255
                (if (time-toggle time 1000) 255 100)))
      (prepend! spr (sprite x y (sprite-idx csd 'dmgbutton-fill) #:layer LAYER_UI
                         #:mx (/ w (sprite-width csd (sprite-idx csd 'dmgbutton-fill)) 1.0)
                         #:my (/ (exact->inexact (* h (dmgbutton-frac b)))
                                 (sprite-height csd (sprite-idx csd 'dmgbutton-fill)) 1.0)
                         #:r br))
      )

    (prepend! spr (sprite x y (sprite-idx csd sprname) #:layer LAYER_UI
                         #:mx (/ w (sprite-width csd (sprite-idx csd sprname)) 1.0)
                         #:my (/ (if h h w) (sprite-height csd (sprite-idx csd sprname)) 1.0)
                         #:r br))
    (prepend! spr (textr (button-label b) x y #:layer LAYER_UI_TEXT
                        #:r txtcol #:g txtcol #:b txtcol)))
  spr)



(define (draw-docking csd center scale space stack)
  (define spr '())
  (define ship (get-ship stack))
  (define col (send the-color-database find-color "hotpink"))
  (for ((s (in-list (space-objects space))))
    (when (and (spaceship? s)
               (not (= (ob-id ship) (ob-id s)))
               (will-dock? ship s))
      (define-values (x y) (obj->screen s center scale))
      (prepend! spr (sprite x y (sprite-idx csd 'circle-outline)
                           #:layer LAYER_OVERLAY #:m (/ scale 20.0)
                           #:r (send col red) #:g (send col green) #:b (send col blue)))))
  spr)


#;(define (draw-tool-overlay csd center scale t stack)
  (cond
    ((dock? t)
     (when (dock-on t)
       (draw-docking csd center scale (get-space stack) stack)))
    ))


; drawn in canon transform
(define (draw-ship-hp csd textr center scale stack)
  (define spr '())

  ; ship hp
  (define s (get-ship stack))
  (prepend! spr (sprite (- (right) 45 (/ (ship-maxcon s) 2)) (+ (top) 30) (sprite-idx csd 'square-outline)
                         #:layer LAYER_UI
                         #:mx (/ (+ 2.0 (ship-maxcon s)) (sprite-width csd (sprite-idx csd 'square-outline)) 1.0)
                         #:my (/ 22.0 (sprite-height csd (sprite-idx csd 'square-outline)))
                         #:r 255 #:g 255 #:b 255))
  (define col (send the-color-database find-color (stoplight-color (ship-con s) (ship-maxcon s))))
  (prepend! spr (sprite (- (right) 45 (/ (ship-con s) 2)) (+ (top) 30) (sprite-idx csd 'square)
                         #:layer LAYER_UI #:my (/ 20.0 (sprite-height csd (sprite-idx csd 'square)))
                         #:mx (/ (ship-con s) (sprite-width csd (sprite-idx csd 'square)) 1.0)
                         #:r (send col red) #:g (send col green) #:b (send col blue)))
  (prepend! spr (textr "Hull" (- (right) 20) (+ (top) 30) #:layer LAYER_UI
                      #:r 255 #:g 255 #:b 255))
  spr)


; drawn in canon transform (buttons, dmgs, warnings)
(define (draw-tool-ui csd center scale space pid ship t stack
                      send-commands active-mouse-tool last-pbolt-time)
  (define buttons '())
  (define spr '())
  (define cmdlevel (player-cmdlevel (car stack)))
  (when (tool-visible? t)
    (case (tool-name t)
      ((engine turnleft turnright)
       (define-values (str key x)
         (case (tool-name t)
           ((engine) (values "Go [w]" #\w 0))
           ((turnleft) (values "Left [a]" #\a -100))
           ((turnright) (values "Right [d]" #\d 100))))
       (define b (holdbutton 'normal key #f x (- (bottom) 35) 80 30 str
                             (lambda (x y) (send-commands (command pid cmdlevel (tool-name t) #t)))
                             (lambda () (send-commands (command pid cmdlevel (tool-name t) #f)))))
       (when (or (not (ship-flying? ship))
                 (and (warping? ship) (not (tool-while-warping? t))))
         (set-button-draw! b 'disabled))
       (prepend! buttons (list b))
       (define ob (add-offline-button! t b send-commands))
       (when ob (prepend! buttons (list ob))))
      ((pbolt)
       (define b (button 'disabled -1 #f (+ (left) 65) (- (bottom) 35) 100 50 "Plasma" #f))
       (when (and (not (equal? 'pbolt (unbox active-mouse-tool)))
                  (ship-flying? ship)
                  (or (tool-while-warping? t) (not (warping? ship))))
         (set-button-draw! b 'normal)
         (set-button-f! b (lambda (x y) (set-box! active-mouse-tool 'pbolt))))
       (prepend! buttons b)
       (define f (pbolt-frac last-pbolt-time (space-time space)))
       (prepend! spr (sprite (+ (left) 65) (- (bottom) 48) (sprite-idx csd '5x1)
                             #:layer LAYER_UI_TEXT
                             #:mx (* f 10.0)
                             #:my 4.0
                             #:r 255))
       (define ob (add-offline-button! t b send-commands))
       (when ob (prepend! buttons (list ob))))
      ((warp)
       (define-values (bs ss) (draw-warp-ui! csd center scale space ship t stack send-commands))
       (prepend! buttons bs)
       (prepend! spr ss))
      ((missile)
       (let ()
         (define b (button 'normal #\q #f (+ (left) 80) (- (bottom) 350) 100 50 "Missile [q]"
                           (lambda (x y)
                             (send-commands (command pid cmdlevel (tool-name t) 'left)))))
         (when (or (not (ship-flying? ship))
                   (and (warping? ship) (not (tool-while-warping? t))))
           (set-button-draw! b 'disabled))
         (prepend! buttons b)
         (define ob (add-offline-button! t b send-commands))
         (when ob (prepend! buttons ob)))

       (let ()
         (define b (button 'normal #\e #f (- (right) 80) (- (bottom) 350) 100 50 "Missile [e]"
                           (lambda (x y)
                             (send-commands (command pid cmdlevel (tool-name t) 'right)))))
         (when (or (not (ship-flying? ship))
                   (and (warping? ship) (not (tool-while-warping? t))))
           (set-button-draw! b 'disabled))
         (prepend! buttons b)
         (define ob (add-offline-button! t b send-commands))
         (when ob (prepend! buttons ob))))
      ((probe)
       (define b (button 'normal #\x #f (- (right) 80) (- (bottom) 250) 100 50 "Probe [x]"
                         (lambda (x y) (send-commands (command pid cmdlevel (tool-name t) #t)))))
       (when (or (not (ship-flying? ship))
                 (and (warping? ship) (not (tool-while-warping? t))))
         (set-button-draw! b 'disabled))
       (prepend! buttons b)
       (define ob (add-offline-button! t b send-commands))
       (when ob (prepend! buttons ob)))
      ((cannon)
       (define b (holdbutton 'normal #\c #f (- (right) 80) (- (bottom) 400) 100 50 "Cannon [c]"
                             (lambda (x y) (send-commands (command pid cmdlevel (tool-name t) (obj-r (get-ship stack)))))
                             (lambda () (send-commands (endcb pid #t)))))
       (when (or (not (ship-flying? ship))
                 (and (warping? ship) (not (tool-while-warping? t))))
         (set-button-draw! b 'disabled))
       (prepend! buttons b)
       (define ob (add-offline-button! t b send-commands))
       (when ob (prepend! buttons ob)))
      ((endrc)
       (define life (max 0 ((tool-rc t) . - . (/ (obj-age space ship) 1000.0))))
       (prepend! spr (sprite 0.0 (- (bottom) 4) (sprite-idx csd 'square) #:layer LAYER_UI
                             #:mx (/ life .1 (sprite-width csd (sprite-idx csd 'square)) 1.0)
                             #:my (/ 6.0 (sprite-height csd (sprite-idx csd 'square)) 1.0)
                             #:r 255))
     
       (define b (button 'normal #\s #f 0 (- (bottom) 35) 70 30 "Stop [s]"
                         (lambda (x y)
                           (send-commands (endrc pid #t)))))
       (prepend! buttons b))
      #;((steer? t)
         (define offline (findf (lambda (d) (equal? "offline" (dmg-type d))) (tool-dmgs t)))
         (when offline
           (define ob (dmgbutton 'normal #f #f
                                 0.0 (- (bottom) 105) 200 30
                                 "Steer Offline"
                                 (lambda (x y) (send-commands (command (ob-id offline)
                                                                       (not (dmg-fixing? offline)))))
                                 (/ (dmg-energy offline) (dmg-size offline)) (dmg-fixing? offline)))
           (prepend! buttons (list ob))))     
      ((dock)
       (when (can-launch? stack)
         (define lb (button 'normal #\w #f (- (right) 80) (- (bottom) 150) 120.0 30.0 "Launch [w]"
                            (lambda (x y) (send-commands (command pid cmdlevel (tool-name t) 'launch)))))
         (prepend! buttons (list lb))
         (define lob (add-offline-button! t lb send-commands "nolaunch"))
         (when lob (prepend! buttons (list lob)))))
    
      #;((shbolt? t)
         (define ship (get-ship stack))
         (define pod (get-pod stack))
         (define b (button 'normal #\space #f (+ (left) 65) (- (bottom) 35) 50.0 50.0 "Shield [_]" #f))
         (cond
           ((and (ship-flying? ship) ((pod-energy pod) . > . (shbolt-shield-size t)))
            (define a (+ (obj-r ship) (pod-facing (get-pod stack))))
            (set-button-f! b (lambda (x y) (send-commands (command (ob-id t) a)))))
           (else
            (set-button-draw! b 'disabled)))
         (prepend! buttons (list b))
         (define ob (add-offline-button! t b send-commands))
         (when ob (prepend! buttons (list ob))))))
  (values buttons spr))

