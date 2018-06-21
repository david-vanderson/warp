#lang racket/base

(require racket/place)
(require "defs.rkt"
         "utils.rkt")

(provide start)

(struct client (in-port out-port in-t out-t) #:mutable #:prefab)
(define clients (make-hash))

(define (start pch)
  (define st
    (thread (lambda ()
              (let loop ()
                (define v (thread-receive))
                (place-channel-put pch v)
                (loop)))))
  (let loop ()
    (define v (place-channel-get pch))
    (define type (car v))
    (cond
      ((equal? type 'new)
       ;(printf "fan (~a) processing ~v\n" (hash-count clients) v)
       (define cid (cadr v))
       (define in (caddr v))
       (define out (cadddr v))
       (define c (client in out
                         (make-in-thread cid in st)
                         (make-out-thread cid out st)))
       (hash-set! clients cid c))
      
      ((equal? type 'kill)
       ;(printf "fan (~a) processing ~v\n" (hash-count clients) v)
       (define cid (cadr v))
       (define c (hash-ref clients cid))
       (close-input-port (client-in-port c))
       (with-handlers ((exn:fail:network? (lambda (exn) #f)))
         (close-output-port (client-out-port c)))
       (kill-thread (client-in-t c))
       (kill-thread (client-out-t c))
       (hash-remove! clients cid))
     
      ((equal? type 'msg)
       ;(printf "fan (~a) processing ~v\n" (hash-count clients) v)
       (define cids (cadr v))
       (define msg (caddr v))
       (for ((cid (in-list cids)))
         (define c (hash-ref clients cid))
         (thread-send (client-out-t c) msg))))
    
    (loop)))
                     
         