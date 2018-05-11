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
    (define stops-per-sec (/ times-stopped total-secs))
    (printf "loop-time ~a num ~a stops-per-sec ~a\n"
            loop-time num stops-per-sec)
    (set! num 0))
  (if (total-secs . < . 3)
    (loop)
    (exit)))
