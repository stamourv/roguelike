(include "utilities.scm")
(include "grid.scm")
(include "player.scm")
(include "maze.scm")
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

(define (maze h w name)
  ;; simple maze game, start at the top left, and get to the bottom right
  (let* ((maze   (generate-maze h w))
	 (player (new-player name maze (new-point 0 0)
			     (empty-grid (grid-height maze) (grid-width maze)
					 cell-fun: (lambda (pos) 'unknown)))))
    ;; player at the top left
    (occupant-set! (grid-get maze (new-point 0 0)) player)
    ;; treasure at the bottom right
    (add-object (grid-get maze (new-point (- (grid-height maze) 1)
					  (- (grid-width maze)  1)))
		(new-treasure))
    (let loop ()
      (let ((pos (copy-point (player-pos player))))
	(cond ;; ((let ((o (get-object (grid-get maze pos))))
;; 		 (and o (treasure? o) o))
;; 	       => (lambda (treasure) ;; TODO instead, win if we pick up (p), we can still only look at it (?), or keep (?) for help ?
;; 		    (display (string-append
;; 			      (player-name player) " has recovered "
;; 			      (object-name treasure) " from "
;; 			      (random-element dungeon-names) "\n"))))
	      (else (update-visibility player)
		    (show-grid maze (player-view player))
		    (read-command player) ; side-effects the player
		    ;; tries to move to the new position, if it fails, return
		    ;; where we were
		    (player-pos-set! player
				     (move maze pos (player-pos player)))
		    (loop)))))))

(if (not debug) (maze 5 5 (random-element character-names)))

;; restore tty, needed if launched from the REPL
(tty-mode-set! (current-input-port) #t #t #f #f 0)
(shell-command "setterm -cursor on")
