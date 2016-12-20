#lang racket/base

(require "defs.rkt"
         "utils.rkt"
         racket/class
         mode-lambda
         "draw-utils.rkt")

(provide (all-defined-out))


(define (dmg-warp tool)
  (cond
    ((null? (tool-dmgs tool))
     (list (chadd (dmg (next-id) "offline" DMG_SIZE 0 DMG_FIX?) (ob-id tool))))
    (else
     #f)))


;; client/server

(define (change-warp! cmd space stack who)
  (define tool (car stack))
  (set-warp-mode! tool cmd)  ; "release" or "hold"
  (values #t '()))


; return list of buttons
(define (draw-warp-ui! csd center scale t stack send-commands)
  (define buttons '())
  (define spr '())
  (define ship (get-ship stack))
  (define pod (get-pod stack))
  (define w (warp-maxe t))
  (define h 30.0)
  (define x (- RIGHT (/ w 2.0) 10))
  (define y (- BOTTOM 35.0))
  (define z (clamp 0.0 1.0 (/ (warp-e t) (warp-maxe t))))

  ; fill
  (append! spr (sprite x y (sprite-idx csd 'square) #:layer LAYER_UI
                       #:mx (/ (* w z) (sprite-width csd (sprite-idx csd 'square)) 1.0)
                       #:my (/ h (sprite-height csd (sprite-idx csd 'square)) 1.0)
                       #:r 255))

  ; we always want the button on the screen so that the mouse cursor looks right
  ; only have the button-f function do something when allowed
  ; always have the holdbutton-frelease function because holding? can be overwritten
  ; - player presses and holds the shortcut key
  ; - player clicks the button with the mouse (overwrites holding?)
  ; this means you can get multiple holdbutton-frelease calls
  (define b (holdbutton 'outline #\q x y w h (string-append "Warp [q]"
                                                            (if (equal? (warp-mode t) "hold")
                                                                " Charging"
                                                                ""))
                        (lambda (x y) (void))
                        (lambda () (send-commands (command (ob-id t) "release")))))
  
  (when (and (equal? "release" (warp-mode t)) ((warp-e t) . = . 0))
    (set-button-f! b (lambda (x y) (send-commands (command (ob-id t) "hold")))))
  (append! buttons b)
  (define ob (add-offline-button! t b send-commands))
  (when ob (append! buttons ob))
  (values buttons spr))
