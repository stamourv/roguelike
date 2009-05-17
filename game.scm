(include "utilities.scm")
(include "grid.scm")
(include "maze.scm")
(include "input.scm")
(include "names.scm")

(define debug #f) ;; TODO find a better way

(random-source-randomize! default-random-source)
(tty-mode-set! (current-input-port) #t #t #t #f 0)
;; (##tty-mode-set! port input-allow-special input-echo input-raw output-raw speed)

(define (maze h w player-name maze-name)
  ;; simple maze game, start at the top left, and get to the bottom right
  (let* ((maze   (generate-maze h w))
	 (view   (empty-grid (grid-height maze) (grid-width maze)
			     cell-fun: (lambda (pos) 'unknown)))
	 (player (new-player player-name (new-point 0 0))))
    ;; player at the top left
    (occupant-set! (grid-get maze (new-point 0 0)) player)
    ;; treasure at the bottom right
    (add-object (grid-get maze (new-point (- (grid-height maze) 1)
					  (- (grid-width maze)  1)))
		(new-treasure))
    (let loop ()
      (let ((pos (copy-point (player-pos player))))
	(cond ((let ((o (get-object (grid-get maze pos))))
		 (and o (treasure? o) o))
	       => (lambda (treasure)
		    (display (string-append player-name " has recovered "
					    (treasure-name treasure) " from "
					    maze-name "\n"))))
	      (else (update-visibility view pos maze)
		    (show-grid maze view)
		    (read-command player) ; side-effects the player
		    ;; tries to move to the new position, if it fails, return
		    ;; where we were
		    (player-pos-set! player
				     (move maze pos (player-pos player)))
		    (loop)))))))

(if (not debug)
    (maze 5 5
	  (random-element character-names) 
	  (random-element dungeon-names)))

;; restore tty, needed if launched from the REPL
(tty-mode-set! (current-input-port) #t #t #f #f 0)
