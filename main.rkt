#lang racket/base
(require racket/cmdline
         "defs.rkt"
         "server.rkt"
         "client.rkt")

(define show-help #f)
(define port PORT)
(define run-server? #f)

(command-line
  #:program "warp"
  #:usage-help "Runs the warp game client (or server with -s)"
  #:once-each
  [("-p" "--port") p
                   "Specify port to use for tcp"
                   (set! port p)]
  [("-s" "--server") "Run server"
                     (set! run-server? #t)])
  
(cond
  [run-server?
    (start-server port)]
  [else
    (start-client "127.0.0.1" port "Name" #f #f)])

