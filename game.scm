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
  (let* ((maze   (generate-maze h w)) ;; TODO players and treasures really need not to be kinds of tiles
	 (view   (empty-grid (grid-height maze) (grid-width maze)
			     cell-fun: (lambda (pos) 'unknown)))
	 (player (new-player player-name (new-point 0 0))))
    ;; player at the top left
    ;; TODO better function name
    (walkable-cell-occupant-set! (grid-get maze (new-point 0 0))
				 player)
    ;; treasure at the bottom right
    ;; TODO have a better function name that that
    (walkable-cell-object-set! (grid-get maze
					 (new-point (- (grid-height maze) 1)
						    (- (grid-width maze) 1)))
			       (new-treasure))
    (let loop ()
      (let ((pos (player-pos player))) ;; TODO copy, and use it instead of old-pos below
	(cond ((walkable-cell-object (grid-get maze pos)) ;; TODO better function name
	       => (lambda (treasure) ;; TODO no check is done to see if the object is really the treasure, but in this case, it's always it
		    (display (string-append player-name " has recovered "
					    (treasure-name treasure) " from "
					    maze-name "\n"))))
	      (else (let ((old-pos (copy-point pos)))
		      (update-visibility view pos maze)
		      (show-grid maze view)
		      (read-command player) ; side-effects the player
		      ;; tries to move to the new position, if it fails,
		      ;; return where we were
		      (player-pos-set! player (move maze old-pos
						    (player-pos player)))
		      (loop))))))))

(if (not debug)
    (maze 5 5
	  (random-element character-names) 
	  (random-element dungeon-names)))

;; restore tty, needed if launched from the REPL
(tty-mode-set! (current-input-port) #t #t #f #f 0)
