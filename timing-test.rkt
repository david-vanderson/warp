#lang racket/base

(define begin-time (current-milliseconds))
(define times-stopped 0)
(define num 0)

(let loop ()
  (define start (current-milliseconds))
  (set! num (+ num 1))
  (define end (current-milliseconds))
  (define loop-time (- end start))
  (define total-secs (/ (- end begin-time) 1000.0))
  (when (loop-time . > . 1)
    (set! times-stopped (+ times-stopped 1))
    (printf "loop-time ~a num ~a\n" loop-time num)
    (set! num 0))
  (cond ((total-secs . < . 1)
         (loop))
        (else
         (printf "times-stopped ~a\n" times-stopped)
         (exit))))
