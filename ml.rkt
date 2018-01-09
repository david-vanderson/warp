#lang racket/base
(require
  racket/gui
  racket/fixnum
  opengl
  mode-lambda
  mode-lambda/static
  mode-lambda/text
  mode-lambda/shot
  (prefix-in gl: mode-lambda/backend/gl)
  ;(prefix-in soft: mode-lambda/backend/software)
  "draw-utils.rkt"
  )

(define gl? #t)

(define frames '())  ; list of last few frame times
(define (add-frame-time current-time)
  (set! frames (cons current-time (take frames (min 10 (length frames))))))
(define (output-fps)
  (define start (list-ref frames (- (length frames) 1)))
  (define end (first frames))
  (define span (/ (- end start) 1000))
  (printf "FPS: ~a\n" (truncate (/ (- (length frames) 1) span))))


(define W 500)
(define H 500)

(define tx 0.0)
(define ty 0.0)
(define tm 1.0)

(define sd (make-sprite-db))

(let ()
  (local-require pict)
  (add-sprite!/file sd 'one "images/blue-frigate.png")
  (add-sprite!/value sd 'white (colorize (filled-rectangle 300 100) "white"))
  (add-sprite!/value sd 'red (colorize (filled-rectangle 1 1) "red"))
  (add-sprite!/value sd 'blue (colorize (filled-rectangle 1 2) "blue"))
  (add-sprite!/value sd 'green (colorize (filled-rectangle 2 2) "green")))
(define textfont (load-font! sd #:size 12.0
                             #:family 'modern
                             ;#:face "Verdana"
                             #:weight 'bold
                             ;#:smoothing 'smoothed
                             ;#:hinting 'aligned
                             ))

#;(define textfont (load-font! sd
                #:size 14.0
                #:face "Triplicate T4c"
                #:smoothing 'smoothed
                #:family 'modern))
;A
(define csd (compile-sprite-db sd))
(save-csd! csd "csd" #:debug? #t)
(define textr (make-text-renderer textfont csd))
(define textar (make-text-aligned-renderer textfont csd))

(define NLAYERS 20)
(define (layers)
  (for/vector ((i NLAYERS))
    (layer (/ W 2.0) (/ H 2.0))))

(define render (
               #;(if gl? gl:stage-draw/dc soft:stage-draw/dc)
               gl:stage-draw/dc csd W H NLAYERS))

(define nsprites 1)
(define scale 1.0)
(define rot 0.0)

;(gl:gl-screenshot! (screenshot-in-dir! "."))
(gl:gl-smoothing? #t)

(define (draw-screen canvas dc)
  ;(set! nsprites (+ 10 nsprites))
  ;(time
   (define s
     (for/list ((i nsprites))
       (sprite (+ (* 1 i) 105.0 tx) (+ (* 1 i) 105.0 ty) (sprite-idx csd 'one)
               #:m scale #:theta rot #:r 0 #:g 0 #:b 0
               #:layer (modulo i NLAYERS)
               )))
  (define rs (sprite (+ 200.0 tx) (+ 200.0 ty) (sprite-idx csd 'red) #:m scale))
  (define bs (sprite (+ 210.0 tx) (+ 210.0 ty) (sprite-idx csd 'blue) #:m scale))
  (define gs (sprite (+ 220.0 tx) (+ 220.0 ty) (sprite-idx csd 'green) #:m scale))
  (define bg (sprite 250.0 250.0 (sprite-idx csd 'white) #:m scale #:a 1.0))
  (define ss (sprite 215.0 215.0 (sprite-idx csd 'one) #:layer 0))
  (set! s (append (list #;ss #;bg rs bs gs #;ss) null s))
  (define t (textr "IIIII"
                    (+ 200.0 tx) (+ 200.0 ty) #:layer 1
                    #:r 255 #:g 255 #:b 255
                    #:mx tm #:my tm))
  (define t2 (textar "IIIII"
                    (+ 200.0 tx) (+ 210.0 ty) #:layer 1
                    #:r 255 #:g 255 #:b 255
                    #:mx tm #:my tm))
   (define r (render (layers) '() (append s (list t t2))))
   (r (send canvas get-width) (send canvas get-height) dc)
  )


(define frame (new frame%
                   (label "mode-lambda test")
                   (width W)
                   (height H)
                   ; use below instead for fullscreen
                   ; (x (- left-inset))
                   ; (y (- top-inset))
                   ; (width screen-w)
                   ; (height screen-h)
                   ; (style '(hide-menu-bar no-caption no-resize-border))
                   ))

(define glconfig (new gl-config%))
(send glconfig set-legacy? #f)

(define my-canvas%
  (class canvas%
    (super-new)
    (define/override (on-char event)
      (define kc (send event get-key-code))
      (case kc
        ((escape)
         (exit 0))
        ((#\j)
         (set! scale (* 1.1 scale)))
        ((#\k)
         (set! scale (/ scale 1.1)))
        ((#\r)
         (set! rot (+ rot 0.1)))
        ((#\n)
         (set! nsprites (+ nsprites 10)))
        ((left)
         (set! tx (+ tx 0.1)))
        ((down)
         (set! ty (+ ty 0.1)))
        ((up)
         (set! tm (+ tm 0.1))))
      (send this refresh))))

(define canvas
  (new my-canvas%
       (parent frame)
       (min-width (inexact->exact (round (/ W 1.0))))
       (min-height (inexact->exact (round (/ H 1.0))))
       (paint-callback draw-screen)
       (gl-config glconfig)
       (style '(no-autoclear gl))))

(send frame show #t)

(define (client-loop)
  (add-frame-time (current-milliseconds))
  (send canvas refresh-now)
  (sleep/yield 5)
  (flush-output)
  (collect-garbage 'incremental)
  ;(output-fps)
  (client-loop))

(add-frame-time (current-milliseconds))
(queue-callback client-loop #f)
