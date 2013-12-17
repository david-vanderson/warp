#lang racket/base

(require racket/class
         racket/math
         racket/list)

(require "defs.rkt"
         "draw.rkt")

(provide (all-defined-out))


; client
(define (click-tactics x y button stack)
  (define role (get-role stack))
  (define pod (get-pod stack))
  (define ship (get-ship stack))
  (cond
    ((and button (equal? button "buff"))
     (struct-copy tactics role (buff #t)))
    ((not (pod-deploying? pod))
     ; we are selecting a new angle to deploy to
     (printf "ship ~a, atan ~a\n" (posvel-r (obj-posvel ship)) (atan y x))
     (define angle (angle-sub (atan y x) (posvel-r (obj-posvel ship))))
     (pod-cmd (obj-id pod) angle))
    (else
     (printf "click-tactics hit ELSE clause\n")
     #f)))


; server
(define (command-tactics cmd space stack)
  (define role (get-role stack))
  (define pod (get-pod stack))
  (define ship (get-ship stack))
  (cond
    ((tactics-buff cmd)
     ; we are buffing shields
     (define s (car (ship-shields ship)))
     (define ss (shield-sections s))
     (define num (vector-length ss))
     (define sections (list (find-shield-section s (pod-angle pod))))
     (define (avg sections) 
       (/ (for/sum ((i sections)) (vector-ref ss i)) (length sections)))
     (let loop ()
       (when ((avg sections) . > . 50)
         (define new-sections
           (remove-duplicates
            (append (list (modulo (sub1 (car sections)) num))
                    sections
                    (list (modulo (add1 (list-ref sections (sub1 (length sections)))) num)))))
         (when ((length new-sections) . > . (length sections))
           (set! sections new-sections)
           (loop))))
     
     (define total (for/sum ((i sections)) (max 0.01 (- (shield-max s) (vector-ref ss i)))))
     (for ((i sections))
       (vector-set! ss i (min 100
                              (+ (* 10.0 (/ (- (shield-max s) (vector-ref ss i)) total))
                                 (vector-ref ss i)))))
     ;(printf "ss ~v\n" ss)
     )
    (else
     (error "command-tactics hit ELSE clause"))))


; client
(define (draw-tactics dc stack)
  (define role (get-role stack))
  (define pod (get-pod stack))
  (define ship (get-ship stack))
  (define spv (obj-posvel ship))
  (define space (get-space stack))
  (define center (get-center stack))
  
  
  (draw-background dc space center)
  ; draw other ships/objects
  (for ((o (space-objects space))
        #:when (not (= (obj-id o) (obj-id ship))))
    (draw-object dc o center space))
  
  (keep-transform dc
    (define-values (x y) (recenter center (posvel-x spv) (posvel-y spv)))
    (send dc translate x y)
    (send dc rotate (- (posvel-r spv)))
    (send dc set-pen fgcolor 1 'solid)
    (send dc set-brush nocolor 'transparent)
    
    ; draw other pods on my ship
    (for ((p (ship-pods ship))
          #:when (not (= (obj-id p) (obj-id pod))))
      (draw-pod dc p))
    
    ; draw my pod
    (draw-pod dc pod)
    
    ; draw my ship and shields
    (keep-transform dc
      (send dc rotate (/ pi 2))
      (send dc set-pen fgcolor 1 'solid)
      (send dc set-brush nocolor 'transparent)
      (send dc draw-polygon ship-external))
    (for ((shield (ship-shields ship)))
      (draw-shield dc shield)))
  
  (define buttons (list leave-button))
  
  ; draw my hud
  (when (or (not pod) (pod-deployed? pod))
    (set! buttons (cons (button -100 -100 120 30 5 5 "buff" "Buff Shields") buttons))
    (keep-transform dc
      (send dc rotate (- (posvel-r spv)))
      (define line-size 50)
      (define w (pod-console pod))
      (send dc set-pen "red" 1 'solid)
      (for ((a (list (+ (tactical-angle w) (/ (tactical-spread w) 2))
                     (- (tactical-angle w) (/ (tactical-spread w) 2)))))
        (send dc draw-line 0 0 (* line-size (cos a)) (* line-size (sin a))))))
  
  
  buttons)
