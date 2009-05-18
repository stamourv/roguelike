(define (read-command player) ;; TODO define all this inside a macro, so that a description can be included with the commands ? or keep 2 separate lists ? or just a lookup list of commands, functions, and doc ? yeah, probably that last one
  (let* ((pos   (player-pos player))
	 (grid  (player-map player))
	 (x     (point-x pos))
	 (y     (point-y pos))
	 (char  (read-char)))

    (clear-to-bottom)

    (case char
      ;; movement
      ((#\esc)
       (read-char) ; should be [ TODO check
       (case (read-char)
	 ((#\A) (point-x-set! pos (- x 1)))
	 ((#\B) (point-x-set! pos (+ x 1)))
	 ((#\C) (point-y-set! pos (+ y 1)))
	 ((#\D) (point-y-set! pos (- y 1)))))

      ;; inventory
      ((#\p) (pick-up   player pos))
      ((#\d) (drop      player))
      ((#\i) (inventory player))

      ;; help
      ((#\?) (show-help))
      ((#\n) (info grid pos))
      ((#\l) (look grid pos))

      ((#\q) (quit)))))
