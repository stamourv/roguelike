(define (read-command player)
  (let* ((pos   (player-pos player))
	 (x     (point-x pos))
	 (y     (point-y pos)))
    (case (read-char)
      ;; movement
      ((#\w) (point-x-set! pos (- x 1)))
      ((#\a) (point-y-set! pos (- y 1)))
      ((#\s) (point-x-set! pos (+ x 1)))
      ((#\d) (point-y-set! pos (+ y 1)))

      ;; actions
      ((#\p) (pick-up player)) ;; TODO pick up an object at the position
      ((#\d) (drop    player))

      ;; help
      ((#\?) (show-help))
      ((#\i) (info pos))
      ((#\l) (look pos))
      )))
