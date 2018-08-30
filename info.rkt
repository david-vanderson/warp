#lang info
(define collection "warp")
(define deps
  '("base"
    "draw-lib"
    "gui-lib"
    "pict-lib"
    "mode-lambda"))
(define raco-commands
  '(("warp" warp/main "play the warp game" #f)))

(define gracket-launcher-libraries '("main.rkt"))
(define gracket-launcher-names     '("Warp"))

