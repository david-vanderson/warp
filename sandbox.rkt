#lang racket/base

(require racket/gui)

(require "defs.rkt"
         "client.rkt"
         "server.rkt")

(define b (box #f))

(thread (lambda ()
          (start-server #:spacebox b)))

(start-client "127.0.0.1" PORT #:name "Alice" #:spacebox b)
;(start-client "127.0.0.1" PORT #:name "Bob"
;               #:new-eventspace? #t)

(yield 'wait)

