(include "utilities.scm")
(include "grid.scm")
(include "cell.scm")
(include "player.scm")
(include "maze.scm")
(include "dungeon.scm")
(include "visibility.scm")
(include "terminal.scm")
(include "input.scm")
(include "names.scm")
(include "help.scm")

(define debug #f) ;; TODO find a better way

(random-source-randomize! default-random-source)
(tty-mode-set! (current-input-port) #t #t #t #f 0)
(shell-command "setterm -cursor off")
;; clear the screen. ugly, but the clear terminal code does not seem to be
;; supported by gambit
(for-each (lambda (dummy) (display "\n")) (iota 50)) ;; TODO use window size


(define test-dungeon
  (string->grid
   (string-append "   |     | |   \n"
		  "   |     |     \n"
		  "   |     | |   \n"
		  "-+ +-- --+ +- -\n"
		  " | |     | |   \n"
		  " |       | |- -\n"
		  " | |     | |   \n"
		  "   |       | | \n")))

(define (game level player victory-fun)
  (occupant-set! (grid-get level (player-pos player)) player)
  (let loop ()
    (if (victory-fun level player)
	(begin (display "You win!") (quit)) ;; TODO have something more dramatic
	(let ((pos (copy-point (player-pos player))))
	  (update-visibility player)
	  (show-grid level
		     print-fun: (visibility-printer (player-view player)))
	  (read-command player) ; side-effects the player
	  ;; tries to move to the new position, if it fails, return where
	  ;; we were
	  (player-pos-set! player (move level pos (player-pos player)))
	  (loop)))))

(define (maze h w name)
  ;; simple maze game, start at the top left, and get to the bottom right
  (let ((level (generate-maze h w)))
    ;; add a treasure
    (add-object (grid-get level (new-point (- (grid-height level) 1)
					   (- (grid-width level)  1)))
		(new-treasure))
    (game level
	  (new-player name level (new-point 0 0))
	  (lambda (level player) #f))))

(define (dungeon name)
  (let ((level (generate-level)))
    (game level
	  (new-player name level
		      (let ((start #f))
			(grid-for-each (lambda (pos)
					 (if (stairs-up? (grid-get level pos))
					     (set! start pos)))
					  level)
			start))
	  (lambda (level player) #f))))

(define (quit)
  ;; restore tty
  (tty-mode-set! (current-input-port) #t #t #f #f 0)
  (shell-command "setterm -cursor on")
  (exit))

;; (if (not debug) (maze 8 8 (random-element character-names)))
(if (not debug) (dungeon (random-element character-names)))

(if (not debug) (quit))
;; TODO change the probabilities in dungeon generation depending on which room we start from
