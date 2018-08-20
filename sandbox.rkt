#lang racket/base

(require racket/gui)

(require "defs.rkt"
         "client.rkt"
         "server.rkt")

(define b (box #f))

(thread (lambda ()
          (start-server #:spacebox b)))

(start-client PORT #:ip "127.0.0.1" #:name "Alice" #:spacebox b)
;(start-client PORT #:ip "127.0.0.1" #:name "Bob"
;               #:new-eventspace? #t)

(yield 'wait)

