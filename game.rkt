#lang racket

(require (only-in srfi/1 iota)
         unstable/function)
(require "utilities/utilities.rkt"
         "utilities/terminal.rkt")
(require "engine/common.rkt"
         "engine/player.rkt"
         "engine/scheduler.rkt")
(require "ui/ui.rkt")


(define debug #f)

(when (not debug) (intercept-tty)) ;; TODO wrap that in exception handler
;; strangely, clear-to-bottom does not clear the bottom of the screen as it
;; should
(for-each (lambda (dummy) (newline)) (iota 50))

(define (game)
  (reschedule player)
  (let loop ()
    (for-each call (find-next-active))
    (increment-turn-no)
    (loop)))

(new-player (getenv "LOGNAME"))
(when (not debug) (game))
