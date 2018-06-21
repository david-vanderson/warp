#lang racket/base

(require racket/tcp)
(require "defs.rkt"
         "utils.rkt")

(provide start)

(struct client (in-port out-port in-t out-t) #:mutable #:prefab)
(define clients (make-hash))

(define next-id
  (let ((id 0))
    (lambda ()
      (set! id (- id 1))
      id)))

(define (start)

  (define-values (sin sout)
    (tcp-connect "127.0.0.1" (- PORT 1)))
  
  (define listener (tcp-listen PORT 4 #t))
  (printf "waiting for clients...\n")

  (define st (current-thread))
  (thread (lambda ()
            (let loop ()
              (define v (read sin))
              (thread-send st v)
              (loop))))
  
  (let loop ()
    (when (tcp-accept-ready? listener)    
      (define-values (in out)
        (with-handlers ((exn:fail? (lambda (exn) (values #f #f))))
          (tcp-accept listener)))
      (when (and in out)
        (set-tcp-nodelay! out #t)
        (define cid (next-id))
        (printf "fan new client, temp id ~a\n" cid)
        (hash-set! clients cid (client in out #f #f))
        (write (cons cid #f) sout)
        (flush-output sout)))
    
    (define v (thread-try-receive))
    (when v
      (define type (car v))
      (cond
        ((equal? type 'new)
         (define oldid (cadr v))
         (define newid (caddr v))
         (printf "fan (~a) processing ~v\n" (hash-count clients) v)
         (define c (hash-ref clients oldid))
         (hash-remove! clients oldid)
         (set-client-in-t! c (make-in-thread newid (client-in-port c) st))
         (set-client-out-t! c (make-out-thread newid (client-out-port c) st))
         (hash-set! clients newid c))
        
        ((equal? type 'kill)
         (printf "fan (~a) processing ~v\n" (hash-count clients) v)
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
           (thread-send (client-out-t c) msg)))

        (else
         ;(printf "fan forwarding client message ~v\n" v)
         ; it's a message from a client, forward
         (write v sout)
         (flush-output sout))
         ))
    
    (loop)))


(module+ main
  (start))
