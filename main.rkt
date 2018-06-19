#lang racket/base
(require racket/cmdline
         "defs.rkt"
         "server.rkt"
         "client.rkt")

(define show-help #f)
(define port PORT)
(define run-server? #f)
(define gui? #t)
(define name "Name")

(command-line
  #:program "warp"
  #:usage-help "Runs the warp game client (or server with -s)"
  #:once-each
  [("-p" "--port") p
                   "Specify port to use for tcp"
                   (set! port p)]
  [("-g" "--nogui") "Run headless client for testing"
                    (set! gui? #f)]
  [("-n" "--name") n
                   "Set client default name"
                   (set! name n)]
  [("-s" "--server") "Run server"
                     (set! run-server? #t)])
  
(cond
  [run-server?
    (start-server port)]
  [else
    (start-client "127.0.0.1" port name #:gui? gui?)])

