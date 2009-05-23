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

(define (game player victory-fun) ;; TODO get a level function, to generate new levels, or just use generate-level ?
  (let loop ()
    (if (victory-fun (player-map player) player)
	(begin
	  (display "You win!")
	  (quit)) ;; TODO have something more dramatic
	(begin
	  (update-visibility player)
	  (show-state player)
	  (read-command player) ; side-effects the player
	  (loop)))))

(define (maze h w name)
  ;; simple maze game, start at the top left, and get to the bottom right
  (let ((level (generate-maze h w)))
    ;; add a treasure
    (add-object (grid-get level (new-point (- (grid-height level) 1)
					   (- (grid-width level)  1)))
		(new-treasure))
    (game (new-player name level (new-point 0 0)) ;; TODO obsolete, won't work with the new signatures, with starting position, would need stairs up, and level is generated another way...
	  (lambda (level player) #f))))

(define (dungeon n-levels name)
  (game (new-player name) (lambda (level player) #f))) ;; TODO use n-levels

(define (quit)
  ;; restore tty
  (tty-mode-set! (current-input-port) #t #t #f #f 0)
  (shell-command "setterm -cursor on")
  (exit))

;; (if (not debug) (maze 8 8 (random-element character-names)))
(if (not debug) (dungeon 3 (random-element character-names)))

(if (not debug) (quit))
