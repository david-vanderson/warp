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

(define (warping? ship)
  (define w (ship-tool ship 'warp))
  (and w
       (= 0 (tool-count w ship))
       ((caddr (tool-val w)) . > . 0)))

(define (warp-charging? ship)
  (define w (ship-tool ship 'warp))
  (and w
       ((tool-count w ship) . > . 0)))


; return list of buttons
(define (draw-warp-ui! csd center scale t stack send-commands)
  (define buttons '())
  (define spr '())
  (define ship (get-ship stack))
  (define vals (tool-val t))
  (define maxw (* 2.0 (cadr vals)))
  (define w (* 2.0 (caddr vals)))
  (define h 30.0)
  (define x 0.0)
  (define y (- BOTTOM 135.0))
  (define z (clamp 0.0 1.0 (/ w maxw)))

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
  (define b (holdbutton 'outline #\q x y maxw h "Warp [q]"
                        (lambda (x y) (void))
                        (lambda ()
                          (send-commands (command (ob-id (car stack)) (tool-name t) #f)))))

  (cond
    ((warping? ship)
     (set-button-label! b "Warping!"))
    (else
     (when (warp-charging? ship)
       (set-button-label! b "Warp [q] Charging"))
     (set-button-f! b (lambda (x y)
                        (send-commands (command (ob-id (car stack)) (tool-name t) #t))))))
  
  (append! buttons b)
  (define ob (add-offline-button! t b send-commands))
  (when ob (append! buttons ob))
  (values buttons spr))
