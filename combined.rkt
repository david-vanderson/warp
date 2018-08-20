#lang racket/base

(require racket/gui)

(require "defs.rkt"
         "client.rkt"
         "server.rkt")

(define b (box #f))

(thread (lambda ()
          (start-server)))

(start-client PORT)

(yield 'wait)

