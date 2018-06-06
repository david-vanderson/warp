#lang racket/base

(require mode-lambda
         mode-lambda/static)

(require "defs.rkt"
         "utils.rkt"
         "draw-utils.rkt")

(provide (all-defined-out))


(define EXPLOSION_FADE 0.33)  ; fraction of maxsize around which we fade

(define (explosion-setup-pre! sd)
  (void))

(define EXPLOSION_SPRITE_IDX #f)
(define EXPLOSION_SPRITE_SIZE #f)

(define (explosion-setup-post! csd)
  (set! EXPLOSION_SPRITE_IDX (sprite-idx csd 'circle))
  (define w (sprite-width csd EXPLOSION_SPRITE_IDX))
  (define h (sprite-height csd EXPLOSION_SPRITE_IDX))
  (set! EXPLOSION_SPRITE_SIZE (max w h)))

(define (explosion-fade e)
  (linear-fade (explosion-size e)
               (- (explosion-maxsize e) (* (explosion-maxsize e) EXPLOSION_FADE))
               (explosion-maxsize e)))

(define (explosion-damage e dt)
  (* (explosion-dmg-rate e) (explosion-fade e) dt))

(define (explosion-radius e)
  (explosion-size e))

(define (explosion-dead? e)
  (= 0.0 (explosion-fade e)))

(define (update-explosion! space e dt)
  (set-explosion-size! e (+ (explosion-size e) (* (explosion-expand e) dt))))

(define (draw-explosion csd center scale e space fowa)
  (define z (/ (min (explosion-size e) (explosion-maxsize e)) (explosion-maxsize e)))
  (define col (linear-color "white" "orange" (* z z) (explosion-fade e)))
  (obj-sprite e csd center scale LAYER_EFFECTS 'circle
              (/ (* 2.0 (explosion-size e)) 100) (* fowa (explosion-fade e)) 0 col))
