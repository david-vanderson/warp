#lang racket/base

(require racket/class
         racket/list
         racket/string
         racket/math
         mode-lambda
         racket/draw)

(require "defs.rkt"
         "utils.rkt"
         "draw-utils.rkt"
         "physics.rkt"
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


(define (get-alpha o r fowlist)
  (define a 0.0)
  (let/ec done
    (for ((f (in-list fowlist)))
      (define dx (- (obj-x o) (fow-x f)))
      (define dy (- (obj-y o) (fow-y f)))
      (define d (+ (* dx dx) (* dy dy)))
      (define vd (+ (fow-visible f) r))
      (define va (linear-fade d (* vd vd 0.9) (* vd vd)))
      (define rd (+ (fow-radar f) r))
      (define ra (linear-fade d (* rd rd 0.9) (* rd rd)))
      ; radar is attenuated by the nebula
      (set! a (max a va (* ra (obj-neb o))))
      (when (a . = . 1.0)
        (done))))
  a)


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


(define (draw-nebula csd center scale space o fowa)
  (define red (+ 100 (inexact->exact (round (* 50 (cycletri (obj-age space o) 17000))))))
  (define blue (+ 100 (inexact->exact (round (* 50 (cycletri (obj-age space o) 19000))))))
  (define green (+ 100 (inexact->exact (round (* 50 (cycletri (obj-age space o) 23000))))))
  (obj-sprite o csd center scale LAYER_MAP
              'nebula
              (/ (+ 1.0 (nebula-radius o)) 25.0)
              fowa (obj-r o)
              (make-color red blue green 1.0))
  )


(define (draw-object csd textr textsr center scale o space myshipid
                     showplayers? fowa faction layer-ships layer-effects)
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
            (draw-plasma csd center scale o space fowa layer-ships))
           ((explosion? o)
            (draw-explosion csd center scale o space fowa layer-effects))
           ((shield? o)
            (draw-shield csd center scale space o fowa layer-ships))
           ((effect? o)
            (draw-effect csd center scale space o fowa layer-effects))
           ((upgrade? o)
            (draw-upgrade csd center scale space o fowa layer-ships))
           ((nebula? o)
            (draw-nebula csd center scale space o fowa))
           ((ship? o)
            (define spr '())
            (define si (hash-ref ship-list (ship-type o)))
            (define sym (bm-sym (ship-info-bm si)))

            (define ship-scale (/ (exact->inexact (ship-sprite-size o))
                                  (sprite-size csd sym)))

            (cond
              ((and (mine? o) (time-toggle (space-time space) 2000))
               (set! sym 'mine2))
              ((and (cannonball? o)
                    (time-toggle (obj-age space o) 200))
               (set! sym
                     (cond
                       ((equal? (ship-type o) "red-cannonball") 'red-cannonball2)
                       ((equal? (ship-type o) "blue-cannonball") 'blue-cannonball2)
                       ((equal? (ship-type o) "purple-cannonball") 'purple-cannonball2)
                       (else sym)))))

            (prepend! spr (obj-sprite o csd center scale layer-ships
                                      sym ship-scale
                                      fowa (obj-r o) (make-color (get-red space o) 0 0 1.0)))
            
            (define eng (ship-tool o 'engine))
            (cond
              ((warping? o)
               ; something
               (void))
              (eng
               (define c (+ (tool-count space eng o) (remainder (debug-num) 5)))
               (when (c . > . 0)
                 (define sym (engine-frame-sym (ship-engine-name o) c
                                               (quotient (obj-age space o) 100)))
                 (when sym
                   (define idx (sprite-idx csd sym))
                   (for ((e (in-list (ship-info-engines si))))
                     (define w (* (engine-w e) ship-scale
                                  (/ (exact->inexact (sprite-height csd idx)))))
                     (define d (* w 0.5 (sprite-width csd idx)))
                     (define r (angle-add (obj-r o) (engine-r e)))
                     (define x (+ (obj-x o)
                                  (* (engine-x e) ship-scale (cos (obj-r o)))
                                  (- (* (engine-y e) ship-scale (sin (obj-r o))))
                                  (- (* d (cos r)))))
                     (define y (+ (obj-y o)
                                  (* (engine-x e) ship-scale (sin (obj-r o)))
                                  (* (engine-y e) ship-scale (cos (obj-r o)))
                                  (- (* d (sin r)))))
                     (define-values (ex ey) (xy->screen x y center scale))
                     (prepend! spr (xy-sprite ex ey csd scale layer-effects
                                              sym w fowa r (make-color 0 0 0 1.0))))))))

            (define w (ship-w o scale))
            
            (define ovpair (assoc faction (ship-overlays o)))
            (when ovpair
              (define ov (cdr ovpair))
              (case (overlay-sym ov)
                ((overlay-qm overlay-cargo)
                 (prepend! spr (obj-sprite o csd center scale layer-effects
                                           (overlay-sym ov) (/ 1.0 scale)
                                           (if (overlay-fow? ov) fowa 1.0) 0.0
                                           (make-color 0 255 0 1.0))))))
            
            (when (and (not (equal? (ob-id o) myshipid))
                       (not (cannonball? o))
                       (not (mine? o))
                       (not (missile? o)))
              (define fc (faction-check faction (ship-faction o)))
              (define col
                (cond ((fc . > . 0) (make-color 0 0 200 (min 0.8 fowa)))
                      ((fc . < . 0) (make-color 200 0 0 (min 0.8 fowa)))
                      (else #f)))
              (when col
                (prepend! spr (draw-corners o w csd center scale col layer-effects))))

            (when (equal? myshipid (ob-id o))
              ; green corners for our ship
              (prepend! spr (draw-corners o w csd center scale
                                          (make-color 0 200 0 1.0) layer-effects)))

            
            (define-values (x y) (obj->screen o center scale))
            (prepend! spr (draw-hp-bar o x y w csd layer-effects fowa))
            
            ;(prepend! spr (draw-ship-info csd center scale o (obj-x o) (obj-y o) space fowa layer_effects))
            (when showplayers?  
              (define players (find-all space o player?))
              (for ((i (modulo (debug-num) 10)))
                (append! players (player -1 (string-append "player" (number->string i))
                                         "team" -1 '() #f #f)))
              (for ((p players)
                    (i (in-naturals)))
                (prepend! spr (text-sprite textr textsr (player-name p)
                                           (+ x (/ w 2.0)) (+ y w 5.0 (* i 20.0))
                                           LAYER_MAP fowa))))
            spr)))))


(define (draw-cargolist csd textr textsr center scale obj colstr fowa text-above text-below)
  (define spr '())
  (define col (send the-color-database find-color colstr))
  (define-values (sx sy) (obj->screen obj center scale))
  (for ((t text-above) (i (in-naturals)))
    (prepend! spr (text-sprite textr textsr t
                               (+ sx 55.0) (+ sy 50.0 (- (* (+ i 1) 14)))
                               LAYER_MAP fowa colstr)))
  (for ((t text-below) (i (in-naturals)))
    (prepend! spr (text-sprite textr textsr t
                               (+ sx 55.0) (+ sy 55.0 (* i 14))
                               LAYER_MAP fowa colstr)))
  spr)


(define (draw-tool-icons csd textr textsr ship x y w layer)
  (define spr '())
  (define size 12.0)
  (define syms
    (filter values (map (lambda (t)
                          (case (tool-name t)
                            ((missile) 'missile)
                            ((probe) 'probe)
                            ((cannon)
                             (if (string-contains? (ship-type ship) "red")
                                 'red-cannonball
                                 'blue-cannonball))
                            ((mine) 'mine)
                            (else #f)))
                        (ship-tools ship))))
  (when (not (null? (ship-playerids ship)))
    (set! syms (append syms (list 'spacesuit))))
  (for ((sym syms)
        (i (in-naturals)))
    (define idx (sprite-idx csd sym))
    (define m (/ size (max (sprite-width csd idx) (sprite-height csd idx))))
    (prepend! spr (sprite (+ x (/ w 2.0) (- size))
                          (+ y (- (/ w 2.0)) (* i (+ size 2.0)) size)
                          idx #:layer layer #:m m #:theta (- pi/2))))
  (when (ship-ai ship)
    (prepend! spr (textr "COM" x (- y (/ w 2.0) -16.0)
                         #:layer layer
                         #:r 100 #:g 100 #:b 255)))
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
  (define mx (* max-x scale 2.0 (/ 1.0 1000.0)))
  (define max-y (/ (space-height space) 2))
  (define my (* max-y scale 2.0 (/ 1.0 1000.0)))
  
  (define sw 2000)
  (define idx (sprite-idx csd '1000x2))
  (define w 1.1)
  
  (define-values (x y) (xy->screen 0.0 0.0 center scale))
  (prepend! spr (sprite x y idx #:layer LAYER_MAP #:mx my #:my w #:theta pi/2
                        #:b (send mapcol blue) #:a (send mapcol alpha)))
  (for* ((i (in-range 1 (+ 1 (inexact->exact (floor (/ max-x sw))))))
         (side (in-list '(-1 1))))
    (define-values (x y) (xy->screen (exact->inexact (* sw side i)) 0.0 center scale))
    (prepend! spr (sprite x y idx #:layer LAYER_MAP #:mx my #:my w #:theta pi/2
                          #:b (send mapcol blue) #:a (send mapcol alpha))))
  
  (prepend! spr (sprite x y idx #:layer LAYER_MAP #:my w #:mx mx
                        #:b (send mapcol blue) #:a (send mapcol alpha)))
  (for* ((i (in-range 1 (+ 1 (inexact->exact (floor (/ max-y sw))))))
         (side (in-list '(-1 1))))
    (define-values (x y) (xy->screen 0.0 (exact->inexact (* sw side i)) center scale))
    (prepend! spr (sprite x y idx #:layer LAYER_MAP #:my w #:mx mx
                          #:b (send mapcol blue) #:a (send mapcol alpha))))
                          
  spr)


(define (draw-corners o w csd center scale color layer)
  (define spr '())
  (define-values (x y) (obj->screen o center scale))
  (define idx (sprite-idx csd 'corner))
  (prepend! spr (sprite (- x w) (- y w) idx
                        #:layer layer #:m 3.0
                        #:a (send color alpha)
                        #:r (send color red) #:g (send color green) #:b (send color blue)))
  (prepend! spr (sprite (+ x w) (- y w) idx
                        #:layer layer #:m 3.0 #:theta pi/2
                        #:a (send color alpha)
                        #:r (send color red) #:g (send color green) #:b (send color blue)))
  (prepend! spr (sprite (+ x w) (+ y w) idx
                        #:layer layer #:m 3.0 #:theta pi
                        #:a (send color alpha)
                        #:r (send color red) #:g (send color green) #:b (send color blue)))
  (prepend! spr (sprite (- x w) (+ y w) idx
                        #:layer layer #:m 3.0 #:theta (+ pi pi/2)
                        #:a (send color alpha)
                        #:r (send color red) #:g (send color green) #:b (send color blue)))
  spr)


(define (draw-hp-bar o x y w csd layer fowa)
  (define hpmax (ship-maxcon o))
  (define hp (ship-con o))
  (cond
    ((and (hp . < . hpmax)
          (not (missile? o))
          (not (cannonball? o))
          (not (spacesuit? o)))
     (define frac (clamp 0.0 1.0 (/ hp hpmax)))
     (define color (stoplight-color hp hpmax))
     (sprite x (- y w 4.5) (sprite-idx csd '5x1) #:layer layer
             #:mx (max 1.0 (* frac 2.0 (min w 48.0) 0.2))
             #:my 3.0
             #:a fowa
             #:r (send color red) #:g (send color green) #:b (send color blue)))
    (else
     '())))


(define (stoplight-color v max)
  (cond ((v . < . (* max (/ 1.0 3.0))) (make-color 255 0 0 1.0))
        ((v . < . (* max (/ 2.0 3.0))) (make-color 255 255 0 1.0))
        (else (make-color 0 255 0 1.0))))


(define (button-sprites csd textr buttons time holding pressed)
  (define spr '())
  (for ((b (in-list buttons))
        #:when (not (equal? (button-draw b) 'hidden)))
    (define-values (x y w h) (values (exact->inexact (button-x b))
                                     (exact->inexact (button-y b))
                                     (exact->inexact (button-width b))
                                     (exact->inexact (button-height b))))

    (define strs (string-split (button-label b) "\n"))

    (define col button-normal)
    (define fill button-normal-fill)
    (define txtcol button-txt)
    (cond
      ((member (button-draw b) '(outline))
       (set! col button-outline)
       (set! fill #f))
      ((member (button-draw b) '(dmg))
       (set! fill button-disable-fill)
       (set! txtcol button-dmg-txt)
       (define br (if (time-toggle time 1000) 255 100))
       (set! col (make-color br 0 0 1.0))
       (set! strs (cons "" strs))
       (prepend! spr (textr "Offline" x (+ y (- (* 10.0 (- (length strs) 1))) 2.0)
                           #:layer LAYER_UI_TEXT
                           #:r br)))
      ((or (member (button-draw b) '(disabled))
           (if (holdbutton? b)
               ; if we are holding the button, draw it as pressed
               (ormap (lambda (h)
                        (equal? (hold-key h) (button-key b)))
                      holding)
               ; if we recently pressed the button, draw it as pressed
               (ormap (lambda (p)
                        (equal? (press-key p) (button-key b)))
                      pressed)))
       (set! col button-disable)
       (set! fill button-disable-fill)
       (set! txtcol button-disable-txt)))

    (prepend! spr (rect-outline csd x y w h 2.0 LAYER_UI
                                #:r (send col red)
                                #:g (send col green)
                                #:b (send col blue)))
    (when fill
      (prepend! spr (rect-filled csd x y w h LAYER_UI
                                 #:r (send fill red)
                                 #:g (send fill green)
                                 #:b (send fill blue))))
    
    (for ((str strs)
          (i (in-naturals)))
      (prepend! spr (textr str x (+ y (- (* 10.0 (- (length strs) 1))) (* 20.0 i))
                           #:layer LAYER_UI_TEXT
                           #:r (send txtcol red)
                           #:g (send txtcol green)
                           #:b (send txtcol blue)))))
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
                           #:layer LAYER_EFFECTS #:m (/ scale 20.0)
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
  (define w (exact->inexact (ship-maxcon s)))
  (define h 20.0)
  (define x (- (right) 8.0 (/ (ship-maxcon s) 2)))
  (define y (+ (top) 8.0 (/ h 2)))
  
  (prepend! spr (rect-outline csd x y w h 2.0 LAYER_UI))
  
  (define col (stoplight-color (ship-con s) (ship-maxcon s)))
  (prepend! spr (rect-filled csd (+ x (/ (- (ship-maxcon s) (ship-con s)) 2))
                             y (ship-con s) h LAYER_UI
                             #:r (send col red) #:g (send col green) #:b (send col blue)))
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
       (define-values (str key x y)
         (case (tool-name t)
           ((engine) (values "Go [w]" #\w (+ (left) 92) (- (bottom) 76)))
           ((turnleft) (values "Left [a]" #\a (+ (left) 48) (- (bottom) 28)))
           ((turnright) (values "Right [d]" #\d (+ (left) 136) (- (bottom) 28)))))
       (define b (holdbutton 'normal key #f x y 80 40 str
                             (lambda (x y) (send-commands (command pid cmdlevel (tool-name t) #t)))
                             (lambda () (send-commands (command pid cmdlevel (tool-name t) #f)))))
       (when (or (not (ship-flying? ship))
                 (and (warping? ship) (not (tool-while-warping? t))))
         (set-button-draw! b 'disabled))
       (prepend! buttons (list b))
       (button-set-dmg! t b))
      ((pbolt)
       (define b (button 'disabled -1 #f (- (right) 58) (- (bottom) 28) 100 40 "Plasma" #f))
       (when (and (not (equal? 'pbolt (unbox active-mouse-tool)))
                  (ship-flying? ship)
                  (or (tool-while-warping? t) (not (warping? ship))))
         (set-button-draw! b 'normal)
         (set-button-f! b (lambda (x y) (set-box! active-mouse-tool 'pbolt))))
       (prepend! buttons b)
       (define f (pbolt-frac last-pbolt-time (space-time space)))
       (prepend! spr (sprite (- (right) 58) (- (bottom) 40) (sprite-idx csd '5x1)
                             #:layer LAYER_UI_TEXT
                             #:mx (* f 10.0)
                             #:my 4.0
                             #:r 255))
       (button-set-dmg! t b))
      ((warp)
       (define-values (bs ss) (draw-warp-ui! csd center scale space ship t stack send-commands))
       (prepend! buttons bs)
       (prepend! spr ss))
      ((missile)
       (let ()
         (define b (holdbutton 'normal #\q #f (- (right) 166) (- (bottom) 124) 100 40 "Missile [q]"
                               (lambda (x y)
                                 (send-commands (command pid cmdlevel (tool-name t) 'left)))
                               void))
         (when (or (not (ship-flying? ship))
                   (and (warping? ship) (not (tool-while-warping? t))))
           (set-button-draw! b 'disabled))
         (prepend! buttons b)
         (button-set-dmg! t b))

       (let ()
         (define b (holdbutton 'normal #\e #f (- (right) 58) (- (bottom) 124) 100 40 "Missile [e]"
                               (lambda (x y)
                                 (send-commands (command pid cmdlevel (tool-name t) 'right)))
                               void))
         (when (or (not (ship-flying? ship))
                   (and (warping? ship) (not (tool-while-warping? t))))
           (set-button-draw! b 'disabled))
         (prepend! buttons b)
         (button-set-dmg! t b)))
      ((probe)
       (define b (holdbutton 'normal #\x #f (- (right) 58) (- (bottom) 76) 100 40 "Probe [x]"
                             (lambda (x y) (send-commands (command pid cmdlevel (tool-name t) #t)))
                             void))
       (when (or (not (ship-flying? ship))
                 (and (warping? ship) (not (tool-while-warping? t))))
         (set-button-draw! b 'disabled))
       (prepend! buttons b)
       (button-set-dmg! t b))
      ((mine)
       (define b (holdbutton 'normal #\m #f (- (right) 166) (- (bottom) 76) 100 40 "Mine [m]"
                             (lambda (x y) (send-commands (command pid cmdlevel (tool-name t) #t)))
                             void))
       (when (or (not (ship-flying? ship))
                 (and (warping? ship) (not (tool-while-warping? t))))
         (set-button-draw! b 'disabled))
       (prepend! buttons b)
       (button-set-dmg! t b))
      ((cannon)
       (define b (holdbutton 'normal #\c #f (- (right) 58) (- (bottom) 172) 100 40 "Cannon [c]"
                             (lambda (x y) (send-commands (command pid cmdlevel (tool-name t) (obj-r (get-ship stack)))))
                             (lambda () (send-commands (endcb pid #t)))))
       (when (or (not (ship-flying? ship))
                 (and (warping? ship) (not (tool-while-warping? t))))
         (set-button-draw! b 'disabled))
       (prepend! buttons b)
       (button-set-dmg! t b))
      ((endrc)
       (define life (/ (max 0 ((tool-rc t) . - . (/ (obj-age space ship) 1000.0)))
                       (tool-rc t)))
       (define w (ship-w ship scale))
       (define-values (x y) (obj->screen ship center scale))
       (prepend! spr (sprite x (+ y w 4.5) (sprite-idx csd '5x1) #:layer LAYER_EFFECTS
                             #:mx (* life 2.0 (min w 48.0) 0.2)
                             #:my 4.0
                             #:r 200))
     
       (define b (button 'normal #\s #f 0 (- (bottom) 101) 100 40 "Stop [s]"
                         (lambda (x y)
                           (send-commands (endrc pid #t)))))
       (prepend! buttons b))  
      ((dock)
       (when (can-launch? stack)
         (define lb (button 'normal #\w #f 0 (- (bottom) 124) 120 40 "Launch [w]"
                            (lambda (x y) (send-commands (command pid cmdlevel (tool-name t) 'launch)))))
         (prepend! buttons (list lb))
         (button-set-dmg! t lb "nolaunch")))))
  (values buttons spr))

