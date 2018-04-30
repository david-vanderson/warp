#lang racket/base

(require racket/gui)

(require "defs.rkt"
         "client.rkt"
         "server.rkt")

(define b (box #f))

(thread (lambda ()
          (start-server #:spacebox b)))

(start-client "127.0.0.1" PORT "Dave" #f #:spacebox b)
;(start-client "127.0.0.1" PORT "Andrea" #f #f)

(yield 'wait)

