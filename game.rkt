#lang racket

(require (only-in srfi/1 iota))
(require "utilities/terminal.rkt")
(require "engine/game-loop.rkt")


(define debug #f)

(when (not debug) (intercept-tty)) ;; TODO wrap that in exception handler

;; strangely, clear-to-bottom does not clear the bottom of the screen as it
;; should
(for-each (lambda (dummy) (newline)) (iota 50))

(when (not debug) (new-game (getenv "LOGNAME")))
