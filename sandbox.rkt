#lang racket/base

(require "defs.rkt"
         "client.rkt"
         "server.rkt")

(thread start-server)
(start-client)