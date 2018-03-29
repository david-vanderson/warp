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

(define (warp-speed w)
  (car (tool-val w)))

(define (warp-threshold w)
  (cadr (tool-val w)))

(define (warp-energy w)
  (caddr (tool-val w)))

(define (warp-charging? ship)
  (define w (ship-tool ship 'warp))
  (and w
       ((warp-energy w) . < . (warp-threshold w))
       ((tool-count w ship) . > . 0)))

(define (warping? ship)
  (define w (ship-tool ship 'warp))
  (and w
       ((warp-energy w) . = . (warp-threshold w))))

(define (cancel-warp! ship)
  (define w (ship-tool ship 'warp))
  (when w
    (set-tool-val! w (list (warp-speed w)
                           (warp-threshold w)
                           0.0))
    (for ((p (in-list (ship-players ship))))
      (set-player-commands! p (remove* '(warp) (player-commands p))))))


; return list of buttons
(define (draw-warp-ui! csd center scale ship t stack send-commands)
  (define buttons '())
  (define spr '())
  (define vals (tool-val t))
  (define maxw (* 2.0 (cadr vals)))
  (define w (* 2.0 (caddr vals)))
  (define h 30.0)
  (define x 0.0)
  (define y (- BOTTOM 135.0))
  (define z (clamp 0.0 1.0 (/ w maxw)))

  (define pid (ob-id (car stack)))
  (define cmdlevel (player-cmdlevel (car stack)))

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
  (define b (holdbutton 'outline #\s x y maxw h "Charge Warp [s]"
                        (lambda (x y) (void))
                        (lambda ()
                          (send-commands (command pid cmdlevel (tool-name t) #f)))))

  (cond
    ((warping? ship)
     (set-button-label! b "Stop Warp [s]")
     (set-button-f! b (lambda (x y)
                        (send-commands (command pid cmdlevel (tool-name t) 'stop)))))
    (else
     (when (warp-charging? ship)
       (set-button-label! b "Charging Warp [s]"))
     (set-button-f! b (lambda (x y)
                        (send-commands (command pid cmdlevel (tool-name t) #t))))))

  (when (or (not (ship-flying? ship))
            (and (warping? ship) (not (tool-while-warping? t))))
    (set-button-draw! b 'disabled))
  (append! buttons b)
  (define ob (add-offline-button! t b send-commands))
  (when ob (append! buttons ob))
  (values buttons spr))
