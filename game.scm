(random-source-randomize! default-random-source)

(import utilities)
(import common)
(import player)
(import scheduler)

;; some more imports, that are not necessary here, but could be for the console
(import character)
(import monsters)
(import objects)
;; TODO more ?

(define debug #f)

(port-settings-set! ##stdout-port (list char-encoding: 'UTF-8))
(tty-mode-set! (current-input-port) #t #t #t #f 0)
(shell-command "setterm -cursor off")
;; strangely, clear-to-bottom does not clear the bottom of the screen as it
;; should
(for-each (lambda (dummy) (display "\n")) (iota 50))

(define (game)
  (reschedule player)
  (let loop ()
    (for-each call (find-next-active))
    (set! turn-no (+ turn-no 1))
    (loop)))

(set! player (new-player (getenv "LOGNAME")))

(if (not debug) (game))
