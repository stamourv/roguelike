#lang racket

(require racket/require)
(require "../utilities/class.rkt"
         "common.rkt"
         "character.rkt"
         "player.rkt"
         "visibility.rkt"
         "scheduler.rkt"
         (multi-in "../ui" ("utilities.rkt" "input.rkt" "display.rkt")))
(provide new-game)

(define (new-game player-name)
  (new-player player-name)
  (game-loop))

(define (game-loop)
  (reschedule player)
  (let loop ()
    (for-each (lambda (x) (x)) (find-next-active))
    (increment-turn-no)
    (loop)))


(define-method (turn (p struct:player-character) reschedule?)
  (if (and (<= (character-hp player) 0)
	   (not (unbox god-mode?))) ; for debugging
      (begin (display "You die.\n")
	     (quit #:force #t))
      (begin
	;; if we don't move, we can get multiple attacks (if we have more
	;; than one attack). these "attacks" can also be used to drink
	;; potions or anything else apart from moving. moving stops the
	;; sequence of attacks (so the last "attack" could be a move)
	(let ((pos (character-pos player))
	      (bab (character-base-attack-bonus player))) ; to check if we moved
	  (let loop ((n            (character-nb-attacks player))
		     (attack-bonus bab))
	    (when (and (> n 0) (equal? (character-pos player) pos))
		(set-character-current-attack-bonus! player attack-bonus)
                (update-visibility! (player-view player)
                                    (player-map player)
                                    (character-pos player))
                (print-state)
                ;; if we didn't move, we can keep attacking
                (when (not (eq? (read-command) 'move))
                  (loop (- n 1) (- attack-bonus 5))))))
	(when reschedule? (reschedule player)))))
