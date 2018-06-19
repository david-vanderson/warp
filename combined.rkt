#lang racket/base

(require racket/gui)

(require "defs.rkt"
         "client.rkt"
         "server.rkt")

(define b (box #f))

(thread (lambda ()
          (start-server)))

(start-client "127.0.0.1" PORT "Dave")

(yield 'wait)

