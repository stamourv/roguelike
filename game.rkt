#lang racket

(require "utilities.rkt" "common.rkt" "player.rkt" "scheduler.rkt"
         (only-in srfi/1 iota)
         unstable/function)

;; some more imports, that are not necessary here, but could be for the console
;; (import character)
;; (import monsters)
;; (import objects)
;; (import cell)
;; (import grid)
;; (import visibility) ;; TODO fix console
;; TODO more ?

(define debug #f)

(when (not debug)
  (system "stty raw -echo opost")
  (system "setterm -cursor off")) ;; TODO wrap that in exception handler
;; strangely, clear-to-bottom does not clear the bottom of the screen as it
;; should
(for-each (lambda (dummy) (newline)) (iota 50))

(define (game)
  (reschedule player)
  (let loop ()
    (for-each call (find-next-active))
    (increment-turn-no)
    (loop)))

(set-player! (new-player (getenv "LOGNAME")))

(when (not debug) (game))
