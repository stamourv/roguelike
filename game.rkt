#lang racket

(require "utilities/terminal.rkt")
(require "engine/game-loop.rkt")


(define debug #f)

(when (not debug) (intercept-tty))

;; strangely, clear-to-bottom does not clear the bottom of the screen as it
;; should
(for ([i (in-range 50)]) (newline))

(when (not debug) (new-game (getenv "LOGNAME")))
