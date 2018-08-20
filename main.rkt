#lang racket/base

(require racket/cmdline
         "defs.rkt"
         "server.rkt"
         "client.rkt")

(define show-help #f)
(define port PORT)
(define address #f)
(define run-server? #f)
(define g #f)
(define combined? #f)
(define name "player")

(command-line
  #:program "warp"
  #:usage-help "Runs the warp game client (or server with -s)"
  #:once-each
  [("-c" "--combined")
                    "Run combined server and client for testing"
                    (set! combined? #t)]
  [("-a" "--address") a
                   "Specify ip address to use for tcp"
                   (set! address a)]
  [("-p" "--port") p
                   "Specify port to use for tcp"
                   (set! port p)]
  [("-g" "--nogui") ng
                    "Run # of headless clients for testing"
                    (set! g (string->number ng))]
  [("-n" "--name") n
                   "Set client base name for -g"
                   (set! name n)]
  [("-s" "--server") "Run server"
                     (set! run-server? #t)])
  
(cond
  [combined?
   (thread (lambda () (start-server)))
   (start-client port #:ip address)]
  [run-server?
   (start-server port)]
  [g
   (for ((i g))
     (start-client port
                   #:ip address
                   #:name (string-append name "-" (number->string i))
                   #:gui? #f
                   #:new-eventspace? #t))
   (sync never-evt)]
  [else
   (start-client port #:ip address)])

