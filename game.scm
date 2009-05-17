(include "utilities.scm")
(include "grid.scm")
(include "maze.scm")
(include "input.scm")
(include "names.scm")

(define debug #f) ;; TODO find a better way

(random-source-randomize! default-random-source)
(tty-mode-set! (current-input-port) #t #t #t #f 0)
;; (##tty-mode-set! port input-allow-special input-echo input-raw output-raw speed)

(define (maze h w player-name maze-name treasure-name)
  ;; simple maze game, start at the top left, and get to the bottom right
  (let* ((maze   (generate-maze h w)) ;; TODO players and treasures really need not to be kinds of tiles
	 (view   (empty-grid (grid-height maze) (grid-width maze)
			     cell-fun: (lambda (pos) 'unknown)))
	 (player (new-player player-name (new-point 0 0))))
    ;; player at the top left
    (grid-set! maze (new-point 0 0) (new-player-cell player))
    ;; treasure at the bottom right
    (grid-set! maze (new-point (- (grid-height maze) 1)
			       (- (grid-width maze) 1))
	       (new-treasure-cell))
    (let loop ()
      (let ((pos (player-pos player)))
	(if (treasure-cell? (grid-get maze pos)) ;; TODO oops, the treasure gets overwritten by the player...
	    (display (string-append player-name " has recovered "
				    treasure-name " from " maze-name "\n"))
	    (let ((old-pos (copy-point pos))) ;; TODO once we don't consider players as terrain, should not be necessary anymore ;; TODO even now, pos should do the trick
	      (update-visibility view pos maze)
	      (show-grid maze view)
	      (read-command player) ; side-effects the position of the player
	      ;; tries to move to the new position, if it fails, return where
	      ;; we were
	      (player-pos-set! player (move maze old-pos (player-pos player)))
	      (loop)))))))

(if (not debug)
    (maze 5 5
	  (random-element character-names) 
	  (random-element dungeon-names)
	  (random-element object-names)))

;; restore tty, needed if launched from the REPL
(tty-mode-set! (current-input-port) #t #t #f #f 0)
