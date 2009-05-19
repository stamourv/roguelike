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

(define (maze h w name)
  ;; simple maze game, start at the top left, and get to the bottom right
  (let* ((level  (generate-maze h w))
	 (player (new-player name level (new-point 0 0))))
    ;; player at the top left
    (occupant-set! (grid-get level (new-point 0 0)) player)
    ;; treasure at the bottom right
    (add-object (grid-get level (new-point (- (grid-height level) 1)
					   (- (grid-width level)  1)))
		(new-treasure))
    (let loop ()
      (define (victory?) #f) ;; TODO add a victory condition
      (if (victory?)
	  (quit) ;; TODO have something more dramatic
	  (let ((pos (copy-point (player-pos player))))
	    (update-visibility player)
	    (show-grid level print-fun: (visibility-printer
					 (player-view player)))
	    (read-command player) ; side-effects the player
	    ;; tries to move to the new position, if it fails, return where
	    ;; we were
	    (player-pos-set! player (move level pos (player-pos player)))
	    (loop))))))

(define (dungeon name)
  (let* ((level  (generate-level))
	 (player (new-player name level #f))) ;; TODO abstract what's common with maze to have a generic "game" function (that would also take the victory condition)
    ;; determine a valid player starting point
    ;; TODO instead, find the entrance, once the dungeon identifies it
    (let loop ((pos (random-position level)))
      (if (walkable-cell? (grid-get level pos))
	  (begin (occupant-set! (grid-get level pos) player) ;; TODO for the generic game function, have a function that is called to determine the starting position, then use it to set the player there
		 (player-pos-set! player pos))
	  (loop (random-position level))))
    ;; TODO place some random treasure
    (let loop () ;; TODO the same game loop as maze, abstract
      (define (victory?) #f) ;; TODO add a victory condition
      (if (victory?)
	  (quit) ;; TODO have something more dramatic
	  (let ((pos (copy-point (player-pos player))))
	    (update-visibility player)
	    (show-grid level print-fun: (visibility-printer
					 (player-view player)))
	    (read-command player) ; side-effects the player
	    ;; tries to move to the new position, if it fails, return where
	    ;; we were
	    (player-pos-set! player (move level pos (player-pos player)))
	    (loop))))))

(define (quit)
  ;; restore tty
  (tty-mode-set! (current-input-port) #t #t #f #f 0)
  (shell-command "setterm -cursor on")
  (exit))

;; (if (not debug) (maze 8 8 (random-element character-names)))
(if (not debug) (dungeon (random-element character-names)))

(if (not debug) (quit))
