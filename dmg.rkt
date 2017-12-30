#lang racket/base

(require racket/list)

(require "defs.rkt"
         "utils.rkt"
         "warp.rkt"
         "missile.rkt"
         "probe.rkt"
         "pbolt.rkt")

(provide (all-defined-out))


(define (dmg-fthrust tool)
  (cond
    ((null? (tool-dmgs tool))
     (list (chadd (dmg (next-id) "offline" DMG_SIZE 0 DMG_FIX?) (ob-id tool))
           (command (ob-id tool) #f)))
    (else
     #f)))

(define (dmg-steer tool ship)
  (cond
    ((null? (tool-dmgs tool))
     (define cmds
       (list (chadd (dmg (next-id) "offline" DMG_SIZE 0 DMG_FIX?) (ob-id tool))))
     (when (ship-flying? ship)
       (append! cmds (list (command (ob-id tool) (obj-r ship)))))
     cmds)
    (else
     #f)))

(define (dmg-shbolt tool)
  (cond
    ((null? (tool-dmgs tool))
     (list (chadd (dmg (next-id) "offline" DMG_SIZE 0 DMG_FIX?) (ob-id tool))))
    (else
     #f)))

(define (dmg-dock tool)
  (cond
    ((null? (tool-dmgs tool))
     (if ((random) . < . 0.5)
         (list (chadd (dmg (next-id) "offline" DMG_SIZE 0 DMG_FIX?) (ob-id tool))
               (command (ob-id tool) #f))
         (list (chadd (dmg (next-id) "nolaunch" DMG_SIZE 0 DMG_FIX?) (ob-id tool)))))
    (((length (tool-dmgs tool)) . < . 2)
     (if (equal? "nolaunch" (dmg-type (car (tool-dmgs tool))))
         (list (chadd (dmg (next-id) "offline" DMG_SIZE 0 DMG_FIX?) (ob-id tool))
               (command (ob-id tool) #f))
         (list (chadd (dmg (next-id) "nolaunch" DMG_SIZE 0 DMG_FIX?) (ob-id tool)))))
    (else
     #f)))

; return a list of commands that represent a single dmg to one of the tools on this ship
; list could be empty if all tools already have all dmgs
; pod chosen so that pods closer to r,t are more likely to be damaged
; r is dist from ship center
; t is angle with respect to ship
#;(define (dmg-ship ship r t)
  ; order pods with closest first
  (define (pod< a b)
    (define da (dist-polar r t (pod-dist a) (pod-angle a)))
    (define db (dist-polar r t (pod-dist b) (pod-angle b)))
    (< da db))
  (define pods (sort (ship-pods ship) pod<))

  (define ret
    (for*/first ((p (in-list pods))
                 (t (in-list (shuffle (pod-tools p))))
                 (cmds (in-value (dmg-tool t ship)))
                 #:when cmds)
      cmds))

  (if ret ret '()))


#;(define (dmg-tool t ship)
  (cond
    ((pbolt? t) (dmg-pbolt t))
    ((fthrust? t) (dmg-fthrust t))
    ((warp? t) (dmg-warp t))
    ((mtube? t) (dmg-mtube t))
    ((ptube? t) (dmg-ptube t))
    ((steer? t) (dmg-steer t ship))
    ((shbolt? t) (dmg-shbolt t))
    ((dock? t) (dmg-dock t))))