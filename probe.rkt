#lang racket/base

(require racket/class
         racket/math
         mode-lambda
         racket/draw)

(require "defs.rkt"
         "utils.rkt"
         "ships.rkt"
         "draw-utils.rkt")

(provide (all-defined-out))


(define (dmg-ptube tool)
  (cond
    ((null? (tool-dmgs tool))
     (if ((random) . < . 0.5)
         (list (chadd (dmg (next-id) "offline" DMG_SIZE 0 DMG_FIX?) (ob-id tool)))
         (list (chadd (dmg (next-id) "nofire" DMG_SIZE 0 DMG_FIX?) (ob-id tool)))))
    (((length (tool-dmgs tool)) . < . 2)
     (if (equal? "offline" (dmg-type (car (tool-dmgs tool))))
         (list (chadd (dmg (next-id) "nofire" DMG_SIZE 0 DMG_FIX?) (ob-id tool)))
         (list (chadd (dmg (next-id) "offline" DMG_SIZE 0 DMG_FIX?) (ob-id tool)))))
    (else
     #f)))


;; client/server

(define (change-ptube! cmd space stack who)
  (define tool (car stack))
  (define ship (get-ship stack))
  (define pod (get-pod stack))
  (cond
    ((or (not cmd) (number? cmd))
     (set-ptube-pid! tool cmd)
     (values #t '()))
    ((member cmd '("load" "unload"))
     (set-ptube-mode! tool cmd)
     (values #t '()))
    ((equal? cmd "fire")
     (cond
       ((not (ship-flying? ship))
        (printf "~a discarding message (not flying) ~v\n" who cmd)
        (values #f '()))
       ((not ((ptube-e tool) . = . (ptube-maxe tool)))
        (printf "~a discarding message (not enough energy) ~v\n" who cmd)
        (values #f '()))
       (else
        (define changes '())
        (set-ptube-e! tool 0)
        (when (server?)
          (define a (angle-add (obj-r ship) (pod-angle pod)))
          (define b (angle-add (obj-r ship) (pod-facing pod)))
          (define p (make-ship "probe" "Probe" (ship-faction ship) #:r b))
          (define d (+ (ship-radius ship) (* 2.0 (ship-radius p))))
          (set-obj-posvel! p (posvel (space-time space)
                                     (+ (obj-x ship) (* d (cos a)))
                                     (+ (obj-y ship) (* d (sin a)))
                                     b
                                     (obj-dx ship)
                                     (obj-dy ship)
                                     0))
          
          (append! changes (chadd p #f) (command (ob-id tool) (ob-id p))))
        
        (values #t changes))))))
     

; return list of buttons
(define (draw-ptube-ui! csd center scale space t stack send-commands)
  (define buttons '())
  (define spr '())
  (define ship (get-ship stack))
  (define pod (get-pod stack))
  (define w (ptube-maxe t))
  (define h 30.0)
  (define x (- RIGHT (/ w 2.0) 10))
  (define y (- BOTTOM 35.0))
  (define z (clamp 0.0 1.0 (/ (ptube-e t) (ptube-maxe t))))

  (define probe (find-id space (ptube-pid t)))
  
  ; fill
  (append! spr (sprite x y (sprite-idx csd 'square) #:layer LAYER_UI
                       #:mx (/ (* w z) (sprite-width csd (sprite-idx csd 'square)) 1.0)
                       #:my (/ h (sprite-height csd (sprite-idx csd 'square)) 1.0)
                       #:r 255))

  (define loadb
    (button 'outline #\l x y w h (string-append (if (equal? (ptube-mode t) "load")
                                                    "Unload"
                                                    "Load")
                                                " [l]")
            (lambda (x y) (send-commands (command (ob-id t) (if (equal? (ptube-mode t) "load")
                                                                "unload"
                                                                "load"))))))

  (append! buttons loadb)
  (define ob (add-offline-button! t loadb send-commands))
  (when ob (append! buttons ob))
  
  (define b (button 'disabled #\f (- x 60) (- BOTTOM 35) 50 50 "Launch [f]" #f))
  (when (and (ship-flying? ship)
             (not probe)
             ((ptube-e t) . = . (ptube-maxe t)))
    (set-button-f! b (lambda (x y) (send-commands (command (ob-id t) "fire"))))
    (set-button-draw! b 'normal))

  (append! buttons b)
  (define oob (add-offline-button! t b send-commands "nofire"))
  (when oob (append! buttons oob))

  (when probe
    (define pod (car (ship-pods probe)))
    (define life (* 10.0 (pod-e pod)))
    (append! spr (sprite 0.0 (- BOTTOM 4) (sprite-idx csd 'square) #:layer LAYER_UI
                         #:mx (/ life (sprite-width csd (sprite-idx csd 'square)) 1.0)
                         #:my (/ 6.0 (sprite-height csd (sprite-idx csd 'square)) 1.0)
                         #:r 255))
    
    (define b (button 'normal #\s (- x 120) (- BOTTOM 35) 50 50 "Stop [s]"
                      (lambda (x y)
                        (send-commands (list (command (ob-id (findf fthrust? (pod-tools pod))) #f)
                                             (command (ob-id t) #f))))))
    (append! buttons b))
  
  (values buttons spr))