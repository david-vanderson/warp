#lang racket/base

(require racket/class
         racket/math
         racket/draw)

(require "defs.rkt"
         "utils.rkt"
         "draw-utils.rkt")

(provide (all-defined-out))


(define bitmap #f)

(define (load-missile)
  (set! bitmap (load-bitmap "missile")))

(define (missile-radius m)
  5.0)

(define (missile-energy space m)
  (- (missile-e m) (obj-age space m)))

(define (missile-dead? space m)
  ((missile-energy space m) . <= . 0))

(define (missile-should-detonate? space m)
  (and (missile-dead? space m)
       ((missile-e m) . > . 0)))

; return list of changes
(define (reduce-missile! space m damage)
  (define changes '())
  ; missiles die instantly
  (set-missile-e! m 0)  ; marks this missile as truly dead (can't detonate)
  (set-space-objects! space (remove m (space-objects space)))
  (when (and (server?) (= -1 damage))
    ; missile is detonating
    (define n 5)
    (define da (degrees->radians 40.0))
    (define starta (- (* (- n 1) (/ da 2.0))))
    (for ((i n))
      (define a (angle-add (obj-r m) (+ starta (* i da))))
      (define p (plasma (next-id) (space-time space)
                        (posvel (space-time space) (obj-x m) (obj-y m) (obj-r m)
                                (+ (* (/ PLASMA_SPEED 3.0) (cos a)) (obj-dx m))
                                (+ (* (/ PLASMA_SPEED 3.0) (sin a)) (obj-dy m))
                                0)
                        5.0 #f))
      (append! changes (chadd p #f))))
  changes)



(define (draw-missile dc m space)
  (keep-transform dc
    (center-on dc m)
    (send dc scale 0.7 0.7)
    (send dc draw-bitmap
          bitmap
          (- (/ (send bitmap get-width) 2))
          (- (/ (send bitmap get-height) 2)))))



(define (dmg-mtube tool)
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

(define (change-mtube! cmd space stack who)
  (define tool (car stack))
  (define ship (get-ship stack))
  (define pod (get-pod stack))
  (cond
    ((number? cmd)
     (set-mtube-mid! tool cmd)
     (values #t '()))
    ((member cmd '("load" "unload"))
     (set-mtube-mode! tool cmd)
     (values #t '()))
    ((equal? cmd "fire")
     (cond
       ((not (ship-flying? ship))
        (printf "~a discarding message (not flying) ~v\n" who cmd)
        (values #f '()))
       ((not ((mtube-e tool) . = . (mtube-maxe tool)))
        (printf "~a discarding message (not enough energy) ~v\n" who cmd)
        (values #f '()))
       (else
        (define changes '())
        (set-mtube-e! tool 0)
        (when (server?)
          (define d (+ (ship-radius ship) (* 2.0 (missile-radius #f))))
          (define a (angle-add (obj-r ship) (pod-angle pod)))
          (define b (angle-add (obj-r ship) (pod-facing pod)))
          
          (define m (missile (next-id) (space-time space)
                             (posvel (space-time space)
                                     (+ (obj-x ship) (* d (cos a)))
                                     (+ (obj-y ship) (* d (sin a)))
                                     b
                                     (obj-dx ship)
                                     (obj-dy ship)
                                     0)
                             5000 b))
          (append! changes (chadd m #f) (command (ob-id tool) (ob-id m))))
        
        (values #t changes))))))
     

; return list of buttons
(define (draw-mtube-ui! dc space t stack send-commands)
  (define buttons '())
  (define ship (get-ship stack))
  (define pod (get-pod stack))
  (define w (mtube-maxe t))
  (define h 30)
  (define x (- RIGHT w 10))
  (define y -370)
  (define z (clamp 0.0 1.0 (/ (mtube-e t) (mtube-maxe t))))

  (define m (find-id space (mtube-mid t)))

  ; outline
  (send dc set-pen "white" 1 'solid)
  (send dc set-brush "gray" 'solid)
  (send dc draw-rectangle x y w h)
  
  ; fill
  (send dc set-pen nocolor 1 'transparent)
  (send dc set-brush (linear-color "red" "red" z (+ 0.5 (* z 0.5))) 'solid)
  (send dc draw-rectangle x y (floor (* w z)) h)

  (define loadb
    (button 'hidden-text #\l x y w h (string-append (if (equal? (mtube-mode t) "load")
                                                        "Unload"
                                                        "Load")
                                                    " [l]")
            (lambda (x y) (send-commands (command (ob-id t) (if (equal? (mtube-mode t) "load")
                                                                "unload"
                                                                "load"))))))

  (append! buttons loadb)
  (define ob (add-offline-button! t loadb send-commands))
  (when ob (append! buttons ob))
  
  (define b (button 'disabled #\f (- x 60) (+ BOTTOM 10) 50 50 "Fire [f]" #f))
  (when (and (ship-flying? ship)
             (not m)
             ((mtube-e t) . = . (mtube-maxe t)))
    (set-button-f! b (lambda (x y) (send-commands (command (ob-id t) "fire"))))
    (set-button-draw! b 'normal))

  (append! buttons b)
  (define oob (add-offline-button! t b send-commands "nofire"))
  (when oob (append! buttons oob))

  (when m
    (send dc set-pen nocolor 1 'transparent)
    (send dc set-brush "red" 'solid)
    (define life (/ (missile-energy space m) 10.0))
    (send dc draw-rectangle (- (/ life 2.0)) (+ BOTTOM 2) life 6)
    
    (define b (button 'normal #\d (- x 120) (+ BOTTOM 10) 50 50 "Det [d]"
                      (lambda (x y) (send-commands (chdam (ob-id m) -1)))))
    (append! buttons b))
  
  buttons)