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
  (let* ((maze (generate-maze h w))
	 (view (empty-grid (grid-height maze) (grid-width maze)
			    cell-fun: (lambda (pos) 'unknown))))
    ;; player at the top left
    (grid-set! maze (new-point 0 0) (new-player-cell player-name)) ;; TODO have the player in a variable, not reasonable to always create a new one
    ;; treasure at the bottom right
    (grid-set! maze (new-point (- (grid-height maze) 1)
			       (- (grid-width maze) 1))
	       (new-treasure-cell))
    (let loop ((pos (new-point 0 0))) ;; TODO use player-pos, or something
      (if (treasure-cell? (grid-get maze pos))
	  (display (string-append player-name " has recovered "
				  treasure-name " from " maze-name "\n"))
	  (begin (update-visibility view pos maze)
		 (show-grid maze view)
		 (cond ((move maze pos (read-command pos)) ;; TODO once read-command takes and side-effects the player structure, put it on the line before
			=> (lambda (new-pos) (loop new-pos)))
		       (else (loop pos))))))))

(if (not debug)
    (maze 5 5
	  (random-element character-names) 
	  (random-element dungeon-names)
	  (random-element object-names)))

;; restore tty, needed if launched from the REPL
(tty-mode-set! (current-input-port) #t #t #f #f 0)
