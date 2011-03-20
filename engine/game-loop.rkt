#lang racket

(require unstable/function)
(require "../utilities/class.rkt")
(require "common.rkt"
         "character.rkt"
         "player.rkt"
         "scheduler.rkt")
(require "../ui/utilities.rkt"
         "../ui/input.rkt"
         "../ui/display.rkt")
(provide new-game)

(define (new-game player-name)
  (new-player player-name)
  (game-loop))

(define (game-loop)
  (reschedule player)
  (let loop ()
    (for-each call (find-next-active))
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
	;; TODO an interesting variant would be to move a certain amount of
        ;;  times, and then only be able to attack, and attack the right
        ;;  number of times, then start the cycle again. would help to display
        ;;  these numbers to the player
	;; TODO would be nice to have both a move action and an attack, but to
        ;;  be able to do a full attack only if we didn't move
	(let ((pos (character-pos player))
	      (bab (character-base-attack-bonus player))) ; to check if we moved
	  (let loop ((n            (character-nb-attacks player))
		     (attack-bonus bab))
	    (when (and (> n 0) (equal? (character-pos player) pos))
		(set-character-current-attack-bonus! player attack-bonus)
                ;; if we didn't move, we can keep attacking
                ;; TODO handle other full-turn actions
                (update-visibility)
                (show-state)
                (when (not (eq? (read-command) 'move))
                  (loop (- n 1) (- attack-bonus 5))))))
        ;; TODO call-next-method, especially with multiple attack handling
	(when reschedule? (reschedule player)))))
