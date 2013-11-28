#lang racket/base

(require racket/async-channel
         racket/serialize)

(require "defs.rkt"
         "physics.rkt")

(provide command-channel
         start-server)

(define command-channel (make-async-channel))
(define client-channels '())

(define SERVER_LOOP_DELAY .03)  ; don't loop more often than X secs
(define SERVER_SEND_DELAY .25)  ; don't send updates more often than X secs
(define ownspace #f)


(define (receive-command cmd)
  (printf "receive-command\n")
  (cond
    ((async-channel? cmd)  ; new client
     (set! client-channels (list* cmd client-channels)))
    ((plasma? cmd)  ; add a plasma
     (set-space-objects! ownspace (append (space-objects ownspace) (list cmd))))
    ((role? cmd)
     ; find the ship that this role is on
     (define ownship (car (space-objects ownspace)))
     (cond ((helm? cmd)
            (set-ship-helm! ownship cmd))))))


(define previous-loop-time (current-inexact-milliseconds))
(define previous-send-time (current-inexact-milliseconds))

(define (server-loop)
  (define current-time (current-inexact-milliseconds))
  (define dt (/ (- current-time previous-loop-time) 1000))
  (set! previous-loop-time current-time)
  (define need-update #f)
  
  ; physics
  ;(printf "server physics:\n")
  ;(vector-set! (shield-sections (car (ship-shields (car (space-objects ownspace))))) 0 100)
  (update-physics! ownspace dt)
  (update-effects! ownspace)
  
  ; process commands
  (let loop ()
    (define command (async-channel-try-get command-channel))
    (when command
      (set! need-update #t)
      (receive-command command)
      (loop)))
  
  ; send out updated world
  (when (or need-update
            (> (/ (- current-time previous-send-time) 1000) SERVER_SEND_DELAY))
    (set! previous-send-time current-time)
    ;    (printf "server sending\n")
    (for ((c client-channels))
      (async-channel-put c (serialize ownspace))))
  
  ; sleep so we don't hog the whole racket vm
  (define loop-time (/ (- (current-inexact-milliseconds) current-time) 1000))
  (if (loop-time . < . SERVER_LOOP_DELAY)
    (sleep (- SERVER_LOOP_DELAY loop-time))
    (begin (printf "SERVER LOOP TOO LONG: ~a\n" loop-time)))
  
  (server-loop))

(define (start-server new-space)
  (set! ownspace new-space)
  (server-loop))
