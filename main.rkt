#lang racket/base

(require racket/cmdline
         "defs.rkt"
         "server.rkt"
         "client.rkt")

(define show-help #f)
(define port PORT)
(define address "127.0.0.1")
(define run-server? #f)
(define g #f)
(define combined? #f)
(define name "Player")

(command-line
  #:program "warp"
  #:usage-help "Runs the warp game client (or server with -s)"
  #:once-each
  [("-a" "--address") a
                   "Specify ip address to use for tcp"
                   (set! address a)]
  [("-p" "--port") p
                   "Specify port to use for tcp"
                   (set! port p)]
  [("-g" "--nogui") ng
                    "Run # of headless clients for testing"
                    (set! g (string->number ng))]
  [("-c" "--combined")
                    "Run combined server and client for testing"
                    (set! combined? #t)]
  [("-n" "--name") n
                   "Set client default name"
                   (set! name n)]
  [("-s" "--server") "Run server"
                     (set! run-server? #t)])
  
(cond
  [combined?
   (thread (lambda () (start-server)))
   (start-client address port)]
  [run-server?
   (start-server port)]
  [g
   (for ((i g))
     (start-client address port
                   #:name (string-append name "-" (number->string i))
                   #:gui? #f
                   #:new-eventspace? #t))
   (sync never-evt)]
  [else
   (start-client address port #:name name)])

