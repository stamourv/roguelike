;; simple roguelike game

(load "utilities.scm")
(load "grid.scm")
(load "maze.scm")

(random-source-randomize! default-random-source)

(define (maze h w player-name maze-name treasure-name) ;; TODO have random name generators
  ;; simple maze game, start at the top left, and get to the bottom right
  (let* ((maze  (generate-maze h w))
	 (known (empty-grid (grid-height maze) (grid-width maze))))
    ;; treasure at the bottom corner
    (grid-set! maze (- (grid-height maze) 1) (- (grid-width maze) 1)
	       (new-treasure-cell))
    (let loop ((posx 0) (posy 0)) ;; TODO have a 2d-point type, and use it everywhere
      (define (move x y)
	(if (and (inside-grid?   maze x y)
		 (walkable-cell? (grid-get maze x y)))
	    (begin (grid-set! maze posx posy (new-walkable-cell))
		   (set! posx x)
		   (set! posy y))))
      (grid-set! maze posx posy (new-player-cell (make-player player-name)))
      ;; update our map with what's around us
      (for-each (lambda (x y) (if (inside-grid? maze x y)
				  (grid-set! known x y (grid-get maze x y))))
		(list (- posx 1) (- posx 1) (- posx 1)
		      posx       posx       posx
		      (+ posx 1) (+ posx 1) (+ posx 1))
		(list (- posy 1) posy       (+ posy 1)
		      (- posy 1) posy       (+ posy 1)
		      (- posy 1) posy       (+ posy 1)))
      (show-grid known)
      (let ((newposx posx) ;; TODO ugly
	    (newposy posy))
	(case (read-char) ;; TODO having to press enter sucks, and counts as 2 commands, find a better way
	  ((#\w) (set! newposx (- posx 1)))
	  ((#\a) (set! newposy (- posy 1)))
	  ((#\s) (set! newposx (+ posx 1)))
	  ((#\d) (set! newposy (+ posy 1))))
	(move newposx newposy)
	(if (treasure-cell? (grid-get maze newposx newposy))
	    (display (string-append player-name " has recovered the "
				    treasure-name " from the " maze-name "\n"))
	    (loop posx posy))))))

(maze 5 5 "Bob" "Maze of Doom" "box of kittens")
;; TODO maze generators won't generate rooms, so might not be interesting, try other algorithms, there probably are some for that
;; TODO maybe start with the maze (or at least, the same kind of wall outline), then open rooms
