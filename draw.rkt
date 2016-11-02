#lang racket/base

(require racket/class
         racket/list
         racket/math
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

(define (draw-framerate dc frames)
  (when ((length frames) . > . 1)
    (send dc set-text-foreground "white")
    (keep-transform dc
      (send dc translate (+ LEFT 60) TOP)
      (define start (list-ref frames (- (length frames) 1)))
      (define end (first frames))
      (define span (/ (- end start) 1000))
      (draw-text dc (format "FPS: ~a" (truncate (/ (- (length frames) 1) span))) 0 0))))

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


(define (draw-background-stars dc center scale)
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

  (send dc set-brush nocolor 'transparent)
  (define ptsize (dc-point-size dc))

  (for ((x (in-list '(#;0.5 1.0 #;1.2 #;2.0)))
        (stars (in-list (list #;stars1 #;stars2 stars3 #;stars4))))
    (define pw (* x ptsize))
    (when (pw . > . 0.6)
      ;(printf "stars ~a ~v\n" x stars)
      (send dc set-pen "white" (/ (* 2.0 (sigmoid pw 1.0)) ptsize) 'solid)
      (for* ((i (in-range istart iend))
             (k (in-range kstart kend))
             (s (in-list stars)))
        (define x (+ origx (* i width) (car s)))
        (define y (+ origy (* k height) (cdr s)))
        (send dc draw-point x y)))))

(define (draw-object dc o space pid)
  (define ptsize (dc-point-size dc))
  (cond       
    ((ptsize . < . 0.25)  ; "sector" view - ships are triangles
     (cond ((ship? o)
            (define col
              (if (find-id o pid)
                  (linear-color "blue" "blue" 1.0
                                (+ 0.5 (* 0.5 (cycletri (space-time space) 1500))))
                  "blue"))
            (send dc set-pen col (/ 1.5 ptsize) 'solid)
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
              (center-on dc o)
              (send dc draw-lines zoutline)))
           ((upgrade? o)
            (send dc set-pen (upgrade-color o) (/ 1.5 ptsize) 'transparent)
            (send dc draw-point (obj-x o) (obj-y o)))
           ((plasma? o)
            (send dc set-pen "red" (/ 2.0 (dc-point-size dc)) 'solid)
            (send dc draw-point (obj-x o) (obj-y o)))))
    (else
     (cond ((ship? o)
            (draw-ship dc o)
            (when (spaceship? o)
              (draw-ship-info dc o space)))
           ((plasma? o)
            (draw-plasma dc o space))
           ((missile? o)
            (draw-missile dc o space))
           ((shield? o)
            (draw-shield dc space o))
           ((effect? o)
            (draw-effect dc space o))
           ((upgrade? o)
            (draw-upgrade dc space o))))))

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


(define (draw-objects dc space pid)
  (define objects (space-objects space))
  (define ships (filter ship? objects))
  (define effects (filter effect? objects))
  (define other (remove* (append ships effects) objects))
  
  (define backeffects (filter backeffect? effects))
  (set! effects (remove* backeffects effects))
  
  (for ((o (in-list (append backeffects ships other effects))))
    (draw-object dc o space pid))
  )

(define (draw-ship-raw dc s)
  (define ship-bitmap (get-ship-bitmap s))
  (send dc draw-bitmap
        ship-bitmap
        (- (/ (send ship-bitmap get-width) 2))
        (- (/ (send ship-bitmap get-height) 2))))

(define (draw-ship-up dc s)
  (keep-transform dc
    (send dc rotate (- pi/2))
    (draw-ship-raw dc s)))

(define (draw-ship dc s)
  (keep-transform dc
    (center-on dc s)
    (draw-ship-raw dc s)))


(define (draw-sector-lines dc space)
  (define max-x (/ (space-width space) 2))
  (define max-y (/ (space-height space) 2))
  
  (define cc (linear-color "blue" "blue" 1.0 0.25))
  (send dc set-pen cc (/ 2.0 (dc-point-size dc)) 'solid)
  (define sw 1000)
  
  (send dc draw-line 0 (- max-y) 0 max-y)
  (for ((i (in-range 1 (+ 1 (inexact->exact (floor (/ max-x sw)))))))
    (send dc draw-line (* sw i) (- max-y) (* sw i) max-y)
    (send dc draw-line (* sw (- i)) (- max-y) (* sw (- i)) max-y))
  
  (send dc draw-line (- max-x) 0 max-x 0)
  (for ((i (in-range 1 (+ 1 (inexact->exact (floor (/ max-y sw)))))))
    (send dc draw-line (- max-x) (* sw i) max-x (* sw i))
    (send dc draw-line (- max-x) (* sw (- i)) max-x (* sw (- i)))))



(define (draw-overlay dc space stack)
  (send dc set-text-foreground "white")

  ; string saying where you are
  (when stack
    (define p (get-pod stack))
    (define str (format "~a" (if p (pod-name p) "Crew")))
    (for ((s (in-list (get-ships stack))))
      (set! str (format "~a on ~a" str (ship-name s))))
    (keep-transform dc
      (send dc translate 0 (/ HEIGHT 2))
      (draw-text dc str 0 0)))

  ; messages
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


(define (stoplight-color v max)
  (cond ((v . < . (* max (/ 1.0 3.0))) "red")
        ((v . < . (* max (/ 2.0 3.0))) "yellow")
        (else "green")))


(define (draw-buttons dc buttons time)
  (for ((b (in-list buttons))
        #:when (not (equal? (button-draw b) 'hidden)))
    (keep-transform dc
      (define-values (x y w h) (values (button-x b) (button-y b)
                                       (button-width b) (button-height b)))
      (case (button-draw b)
        ((normal)
         (send dc set-brush "gray" 'solid)
         (send dc set-pen fgcolor 1 'solid)
         (send dc set-text-foreground "black"))
        ((disabled dmg)
         (send dc set-brush "black" 'solid)
         (send dc set-pen fgcolor 1 'solid)
         (send dc set-text-foreground "gray"))
        ((hidden-text)
         (send dc set-brush nocolor 'transparent)
         (send dc set-pen fgcolor 1 'solid)
         (send dc set-text-foreground "black")))

      (define r (send dc get-clipping-region))

      (define-values (tw th td ta) (send dc get-text-extent (button-label b)))
      
      (cond
        ((dmgbutton? b)
         (define col (if (dmgbutton-fixing? b) "red"
                         (if (time-toggle time 1000) "red" "darkred")))
         (define transcol (linear-color col "black" 0.2 1.0))
         (send dc set-brush transcol 'solid)
         (send dc set-pen col 1 'solid)
         (send dc draw-rectangle x y w h)

         (send dc set-brush col 'solid)
         (send dc draw-rectangle x y w (* h (dmgbutton-frac b)))
         
         (send dc set-text-foreground "black")
         (send dc set-clipping-rect x y w h)
         (draw-text dc (button-label b)
                    (+ x (/ w 2.0) (- (/ tw 2.0)))
                    (+ y (/ h 2.0) (/ th 2.0))))
        (h
         (send dc draw-rectangle x y w h)
         (send dc set-clipping-rect x y w h)
         (draw-text dc (button-label b)
                    (+ x (/ w 2.0) (- (/ tw 2.0)))
                    (+ y (/ h 2.0) (/ th 2.0)))
         (when (equal? 'dmg (button-draw b))
           (send dc set-pen "darkred" 2 'solid)
           (send dc set-brush nocolor 'transparent)
           (send dc draw-rectangle (+ x 2) (+ y 2) (- w 4) (- h 4))
           (send dc draw-line (+ x 2) (+ y 2) (+ x w -2) (+ y h -2))
           (send dc draw-line (+ x w -2) (+ y 2) (+ x 2) (+ y h -2))))
        (else
         (send dc draw-ellipse (- x w) (- y w) (* 2 w) (* 2 w))
         (send dc set-clipping-rect (- x w) (- y w) (* 2 w) (* 2 w))
         (draw-text dc (button-label b)
                    (+ x (- (/ tw 2.0)))
                    (+ y (/ th 2.0)))))
      
      (send dc set-clipping-region r))))


(define (draw-ship-info dc ship space)
  (keep-transform dc
    (send dc translate (obj-x ship) (obj-y ship))

    (send dc set-pen nocolor 1 'transparent)
    (define hpfrac (max 0.0 (/ (ship-con ship) (ship-maxcon ship))))
    (when (hpfrac . < . 0.5)
      (define cycletime (+ 200 (* 5000 hpfrac)))
      (define z (cycletri (obj-age space ship) cycletime))
      (define alpha (+ 0.0 (* 1.0 z (max 0.1 (- 0.8 hpfrac)))))
      (define hpcolor (linear-color "red" "black" (max 0.2 (- hpfrac 0.2)) alpha))
      (send dc set-brush hpcolor 'solid)
      (define hpr (ship-radius ship))
      (send dc draw-arc (- hpr) (- hpr) (* 2 hpr) (* 2 hpr) 0 2pi))
    
    (send dc rotate (- (obj-r ship)))
    (for ((p (in-list (ship-pods ship))))
      (keep-transform dc
        (send dc translate (* (pod-dist p) (cos (pod-angle p))) (* (pod-dist p) (sin (pod-angle p))))
        (when (pod-facing p)
          (send dc rotate (- (pod-facing p))))
        (when ((pod-maxe p) . > . 0)
          (send dc set-brush nocolor 'transparent)
          (define height (+ 0.5 (* 1.0 (/ (pod-e p) (pod-maxe p)))))
          (send dc set-pen (stoplight-color (pod-e p) (pod-maxe p)) height 'solid)
          (define r 4.0)
          (send dc draw-arc (- r) (- r) (* 2 r) (* 2 r) (- (/ pi 4)) (/ pi 4)))
        (define ndmgs (length (search p dmg? #t #f)))
        (when (ndmgs . > . 0)
          (send dc set-pen "red" 3 'solid)
          (define r 5.0)
          (define da (degrees->radians 40.0))
          (define a (- (* (- ndmgs 1) (/ da 2.0))))
          (send dc rotate a)
          (for ((i ndmgs))
            (send dc draw-point r 0)
            (send dc rotate da)))
        ))))
          


(define (draw-pods dc ship rot stack send-commands canvas meid)
  (define buttons '())
  (for ((p (in-list (ship-pods ship)))
        #:when (not (lounge? p)))
    (keep-transform dc
      (send dc translate (* (pod-dist p) (cos (pod-angle p))) (* (pod-dist p) (sin (pod-angle p))))
      (send dc rotate rot)  ; back out the ship rotation
      
      (define mypod (equal? (ob-id p) (ob-id (get-pod stack))))  ; the pod I'm in
      (when mypod
        (draw-my-pod dc stack rot))

      (define ptsize (dc-point-size dc))
      (define size (* 3.0 (min 1.0 (max 0.0 (/ (- ptsize 1.0) 4.0)))))
      (when (size . > . 0.5)
        
        (cond
          ((pod-player p)
           ;(send dc draw-rectangle (- 5) (- 5) 10 10)
           ;(send dc draw-text (pod-name p) (- 5) (- 5))
           )
          ((hangar? p)
           (define-values (x y) (dc->canon canvas dc (- size) (- size)))
           (define-values (x2 y2) (dc->canon canvas dc size size))
           (define b (button 'normal #f x y (abs (- x2 x)) (abs (- y2 y)) (pod-name p)
                             (lambda (x y)
                               (send-commands (chrole meid (ob-id p))))))
           (append! buttons (list b)))
          (else
           (define-values (cx cy) (dc->canon canvas dc 0 0))
           (define-values (rx ry) (dc->canon canvas dc size 0))
           ;(printf "drawing button at ~a ~a ~a\n" cx cy (abs (- rx cx)))
           (define b (button 'normal #f cx cy (abs (- rx cx)) #f (pod-name p)
                             (lambda (x y)
                               (send-commands (chrole meid (ob-id p))))))
           (append! buttons (list b)))))))
buttons)


(define (draw-my-pod dc stack rot)
  (define p (get-pod stack))
  (when (pod-facing p)
    (keep-transform dc
      ;(send dc translate (* (pod-dist p) (cos (pod-angle p))) (* (pod-dist p) (sin (pod-angle p))))
      (define line-size 50)
      (send dc set-pen "darkred" (/ 1.0 (dc-point-size dc)) 'dot)
      (for ((a (in-list (list (+ rot (pod-facing p) (/ (pod-spread p) 2))
                              (+ rot (pod-facing p) (- (/ (pod-spread p) 2)))))))
        (send dc draw-line 0 0 (* line-size (cos a)) (* line-size (sin a)))))))


(define (draw-docking dc space stack)
  (define ship (get-ship stack))
  (for ((s (in-list (space-objects space))))
    (when (and (spaceship? s)
               (not (= (ob-id ship) (ob-id s)))
               (will-dock? ship s))
      (send dc set-brush nocolor 'transparent)
      (send dc set-pen "hotpink" (/ 1.5 (dc-point-size dc)) 'solid)
      (send dc draw-ellipse (- (obj-x s) 10) (- (obj-y s) 10) 20 20))))


; stuff drawn in scaled space transform (docking, targeting)
(define (draw-tool-overlay dc t stack)
  (cond
    ((dock? t)
     (when (dock-on t)
       (draw-docking dc (get-space stack) stack)))))


(define (draw-fraction-box dc x y width height frac fill-color orient)
  (send dc set-pen nocolor 1 'transparent)
  (send dc set-brush fill-color 'solid)
  (case orient
    ((horz)
     (define f (max 0 (* frac width)))
     (send dc draw-rectangle (+ x (- width f)) y f height))
    ((vert)
     (send dc draw-rectangle x y width (max 0 (* frac height)))))
  
  (send dc set-pen fgcolor 1.5 'solid)
  (send dc set-brush fill-color 'transparent)
  (send dc draw-rectangle x y width height))


; drawn in canon transform
(define (draw-pod-ui dc stack)
  (send dc set-text-foreground "white")
  
  ; pod energy
  (define p (get-pod stack))
  (when ((pod-maxe p) . > . 0)
    (draw-fraction-box dc (+ LEFT 10) (+ BOTTOM 10) 20 (pod-maxe p) (/ (pod-e p) (pod-maxe p))
                       (stoplight-color (pod-e p) (pod-maxe p)) 'vert))

  ; ship hp
  (define s (get-ship stack))
  (define mc (ship-maxcon s))
  (draw-fraction-box dc (- RIGHT 40 mc) (- TOP 10 20) mc 20 (/ (ship-con s) mc)
                     (stoplight-color (ship-con s) mc) 'horz)
  (draw-text dc "Hull" (- RIGHT 30) (- TOP 10))

  ; ship reserves
  (define mb (ship-maxbat s))
  (when (mb . > . 0)
    (draw-fraction-box dc (- RIGHT 40 mb) (- TOP 40 20) mb 20 (/ (ship-bat s) mb)
                       (stoplight-color (ship-bat s) mb) 'horz)
    (draw-text dc "Res" (- RIGHT 30) (- TOP 40))))


; drawn in canon transform (buttons, dmgs, warnings)
(define (draw-tool-ui dc t stack send-commands)
  (define buttons '())
  (cond
    ((pbolt? t) (append! buttons (draw-pbolt-ui! dc t stack send-commands)))
    ((warp? t) (append! buttons (draw-warp-ui! dc t stack send-commands)))
    ((mtube? t) (append! buttons (draw-mtube-ui! dc t stack send-commands)))
    ((steer? t)
     (define offline (findf (lambda (d) (equal? "offline" (dmg-type d))) (tool-dmgs t)))
     (when offline
       (define ob (dmgbutton 'normal #f
                             -100 -200 200 30
                             "Steer Offline"
                             (lambda (x y) (send-commands (command (ob-id offline)
                                                                   (not (dmg-fixing? offline)))))
                             (/ (dmg-energy offline) (dmg-size offline)) (dmg-fixing? offline)))
       (append! buttons (list ob))))
    ((fthrust? t)
     (define b (button 'normal #\w 0 -300 60 30 (if (fthrust-on t) "Stop [W]" "Go [W]")
                       (lambda (x y) (send-commands (command (ob-id t) (not (fthrust-on t)))))))
     (when (not (ship-flying? (get-ship stack)))
       (set-button-draw! b 'disabled))
     (append! buttons (list b))
     (define ob (add-offline-button! t b send-commands))
     (when ob (append! buttons (list ob))))
     
    ((dock? t)
     (define b (button 'normal #\c -150 -300 100 30 (if (dock-on t) "Docking... [C]" "Dock [C]")
                       (lambda (x y) (send-commands (command (ob-id t) (not (dock-on t)))))))
     (when (not (ship-flying? (get-ship stack)))
       (set-button-draw! b 'disabled))
     (append! buttons (list b))
     (define ob (add-offline-button! t b send-commands))
     (when ob (append! buttons (list ob)))
     (define lb (button (if (can-launch? stack) 'normal 'disabled) #\l -250 -300 70 30 "Launch [L]"
                       (lambda (x y) (send-commands (command (ob-id t) "launch")))))
     (append! buttons (list lb))
     (define lob (add-offline-button! t lb send-commands "nolaunch"))
     (when lob (append! buttons (list lob))))
    
    ((shbolt? t)
     (define ship (get-ship stack))
     (define pod (get-pod stack))
     (define b (button 'normal #\space (+ LEFT 40) (+ BOTTOM 10) 50 50 "Shield [_]" #f))
     (cond
       ((and (ship-flying? ship) ((pod-energy pod) . > . (shbolt-shield-size t)))
        (define a (+ (obj-r ship) (pod-facing (get-pod stack))))
        (set-button-f! b (lambda (x y) (send-commands (command (ob-id t) a)))))
       (else
        (set-button-draw! b 'disabled)))
     (append! buttons (list b))
     (define ob (add-offline-button! t b send-commands))
     (when ob (append! buttons (list ob)))))
  buttons)


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

