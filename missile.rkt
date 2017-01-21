#lang racket/base

(require racket/class
         racket/math
         mode-lambda
         racket/draw)

(require "defs.rkt"
         "utils.rkt"
         "physics.rkt"
         "draw-utils.rkt")

(provide (all-defined-out))


(define (missile-energy space m)
  (max 0.0 (- (missile-e m) (obj-age space m))))

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



(define (draw-missile csd center scale m space fowa)
  (obj-sprite m csd center scale LAYER_SHIPS 'missile 10.0 fowa (obj-r m) "black"))



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
          (define d (+ (ship-radius ship) (* 2.0 MISSILE_RADIUS)))
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
                             5000 b (ship-faction ship)))
          (append! changes (chadd m #f) (command (ob-id tool) (ob-id m))))
        
        (values #t changes))))))
     

; return list of buttons
(define (draw-mtube-ui! csd center scale space t stack send-commands)
  (define buttons '())
  (define spr '())
  (define ship (get-ship stack))
  (define pod (get-pod stack))
  (define w (mtube-maxe t))
  (define h 30.0)
  (define x (- RIGHT (/ w 2.0) 10))
  (define y (- BOTTOM 35.0))
  (define z (clamp 0.0 1.0 (/ (mtube-e t) (mtube-maxe t))))

  (define m (find-id space (mtube-mid t)))
  
  ; fill
  (append! spr (sprite x y (sprite-idx csd 'square) #:layer LAYER_UI
                       #:mx (/ (* w z) (sprite-width csd (sprite-idx csd 'square)) 1.0)
                       #:my (/ h (sprite-height csd (sprite-idx csd 'square)) 1.0)
                       #:r 255))

  (define loadb
    (button 'outline #\l x y w h (string-append (if (equal? (mtube-mode t) "load")
                                                    "Unload"
                                                    "Load")
                                                " [l]")
            (lambda (x y) (send-commands (command (ob-id t) (if (equal? (mtube-mode t) "load")
                                                                "unload"
                                                                "load"))))))

  (append! buttons loadb)
  (define ob (add-offline-button! t loadb send-commands))
  (when ob (append! buttons ob))

  (set! x (- x (/ w 2) 10 35))
  
  (define b (button 'disabled #\f x y 70 50 "Fire [f]" #f))
  (when (and (ship-flying? ship)
             (not m)
             ((mtube-e t) . = . (mtube-maxe t)))
    (set-button-f! b (lambda (x y) (send-commands (command (ob-id t) "fire"))))
    (set-button-draw! b 'normal))

  (append! buttons b)
  (define oob (add-offline-button! t b send-commands "nofire"))
  (when oob (append! buttons oob))

  (set! x (- x 35 10 35))

  (when m
    (define life (/ (missile-energy space m) 10.0))
    (append! spr (sprite 0.0 (- BOTTOM 4) (sprite-idx csd 'square) #:layer LAYER_UI
                         #:mx (/ life (sprite-width csd (sprite-idx csd 'square)) 1.0)
                         #:my (/ 6.0 (sprite-height csd (sprite-idx csd 'square)) 1.0)
                         #:r 255))
    
    (define b (button 'normal #\d x y 70 50 "Det [d]"
                      (lambda (x y) (send-commands (chdam (ob-id m) -1)))))
    (append! buttons b))
  
  (values buttons spr))


; return a list of changes
(define (mtube-ai! space stack)
  (define changes '())
  (define ownship (get-ship stack))
  (define pod (get-pod stack))
  (define t (findf mtube? (pod-tools pod)))
  (define m (find-id space (mtube-mid t)))
  (when (not m)
    (define chance-per-sec .1)
    (when (and (ship-flying? ownship)
               (tool-online? t)
               ((mtube-e t) . = . (mtube-maxe t))
               ((random) . < . chance-per-sec))
      
      (for/first ((o (in-list (space-objects space)))
                  #:when (and (spaceship? o)
                              ((faction-check (ship-faction ownship) (ship-faction o)) . < . 0)
                              ((distance ownship o) . < . (ship-radar ownship))))
        ;(printf "firing\n")
        (append! changes (command (ob-id t) "fire"))
        (set-pod-npc?! pod 0)  ; make sure we run the missile ai right away
        )))
  (when m
    ; we are piloting the missile
    (define origc (missile-course m))
  
    ; only worry about ships
    (define ships (filter spaceship? (space-objects space)))
  
    ; search space around our original inputs
    (define bestc origc)
    (define bestfit #f)
    (for* ((c (in-list '(0 -10 10 -50 50 -100 100 -150 150 180))))
      (define origpv (struct-copy posvel (obj-posvel m)))
      (set-missile-course! m (angle-add origc (degrees->radians c)))
      (define maxfit -inf.0)
      (define curfit 0.0)
    
      (define predict-hsecs (inexact->exact (round (/ (missile-energy space m) 500.0))))
      (for ((i (in-range predict-hsecs)))
        (for ((s (in-list ships))) (physics! (obj-posvel s) 0.5))
        (update-physics! space m 0.1)
        (update-physics! space m 0.1)
        (update-physics! space m 0.1)
        (update-physics! space m 0.1)
        (update-physics! space m 0.1)
        (define f (missile-fitness space m))
        (set! curfit (+ curfit f))
        (set! maxfit (max maxfit (/ curfit (add1 i)))))
    
      (for ((s (in-list ships))) (physics! (obj-posvel s) (- (/ predict-hsecs 2.0))))
      (set-obj-posvel! m origpv)
    
      ;(printf "fit ~a ~a ~a\n" maxfit c predict-secs)
      
      (when (or (not bestfit)  ; first pass
                (maxfit . > . (* 1.01 bestfit)))
        ;(printf "better fit ~a ~a\n" maxfit (missile-course m))
        (set! bestfit maxfit)
        (set! bestc (missile-course m))))
    
    (set-missile-course! m origc)
    (when (not (equal? origc bestc))
      (append! changes (list (command (ob-id m) bestc)))))
  changes)


(define (missile-fitness space m)
  (define f 0.0)
  
  (for ((o (in-list (space-objects space)))
        #:when (spaceship? o))
    (define d (distance o m))
    (define hd (+ (ship-radius o) MISSILE_RADIUS))
    (define foe? ((faction-check (missile-faction m) (ship-faction o)) . < . 0))
    (cond ((d . < . hd)
           (set! f (+ f (if foe? 1000.0 -1000.0))))
          (else
           (set! f (+ f (* (if foe? 1000.0 -1000.0) (- 1.0 (sigmoid d 100))))))))
  f)