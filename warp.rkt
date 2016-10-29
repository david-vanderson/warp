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
  (define x -100)
  (define y -350)
  (define w 200)
  (define h 30)
  (define z (clamp 0.0 1.0 (/ (warp-e t) (warp-maxe t))))

  ; fill
  (send dc set-pen nocolor 1 'transparent)
  (send dc set-brush (linear-color "red" "red" z (+ 0.25 (* z 0.5))) 'solid)
  (send dc draw-rectangle x (- y h) (floor (* w z)) h)

  ; outline
  (send dc set-pen "white" 1 'solid)
  (send dc set-brush nocolor 'transparent)
  (send dc draw-rectangle x (- y h) w h)
  
  (send dc set-text-foreground "white")
  (define txt (string-append "Warp [q]" " " (warp-mode t)))
  (define-values (tw th td ta) (send dc get-text-extent txt))
  (draw-text dc txt
             (+ x (/ w 2.0) (- (/ tw 2.0)))
             (+ y (/ h 2.0) (/ th 2.0)))

  (define enabled? (or (equal? "hold" (warp-mode t))
                       (and (equal? "release" (warp-mode t)) ((warp-e t) . = . 0))))
                            
  (define b (button (if enabled? 'normal 'disabled) #\q x y w h txt
                    (lambda (x y)
                      (send-commands (command (ob-id t) (if (equal? "hold" (warp-mode t))
                                                            "release"
                                                            "hold"))))))
  (append! buttons b)
  (define ob (add-offline-button! t b send-commands))
  (when ob
    (append! buttons ob)
    ; change appearance of warp bar
    )
  buttons)
