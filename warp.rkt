#lang racket/base

(require "defs.rkt"
         "utils.rkt"
         racket/class
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
(define (draw-warp-ui! dc t stack send-commands)
  (define buttons '())
  (define ship (get-ship stack))
  (define pod (get-pod stack))
  (define w (warp-maxe t))
  (define h 30)
  (define x (- (/ w 2.0)))
  (define y -370)
  (define z (clamp 0.0 1.0 (/ (warp-e t) (warp-maxe t))))

  ; outline
  (send dc set-pen "white" 1 'solid)
  (send dc set-brush "gray" 'solid)
  (send dc draw-rectangle x y w h)
  
  ; fill
  (send dc set-pen nocolor 1 'transparent)
  (send dc set-brush (linear-color "red" "red" z (+ 0.5 (* z 0.5))) 'solid)
  (send dc draw-rectangle x y (floor (* w z)) h)

  ; we always want the button on the screen so that the mouse cursor looks right
  ; only have the button-f function do something when allowed
  ; always have the holdbutton-frelease function because holding? can be overwritten
  ; - player presses and holds the shortcut key
  ; - player clicks the button with the mouse (overwrites holding?)
  ; this means you can get multiple holdbutton-frelease calls
  (define b (holdbutton 'hidden-text #\q x y w h (string-append "Warp [q]"
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
  buttons)
