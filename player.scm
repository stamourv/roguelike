(define-type-of-occupant player
  map
  pos
  view)
(define (new-player name map pos view)
  (make-player name
	       (lambda () #\@)
	       map
	       pos
	       view))
(define player-name occupant-name)


(define (update-visibility player)
  ;; set the fog of war
  (let ((view (player-view player))
	(pos  (player-pos  player))
	(g    (player-map  player)))
    (for-each (lambda (x)
		(for-each (lambda (y)
			    (if (eq? (grid-get view (new-point x y)) 'visible)
				(grid-set! view (new-point x y) 'visited)))
			  (iota (grid-width view))))
	      (iota (grid-height view)))
    ;; set visible area TODO have something better, maybe see until we see a wall, in each direction + sides to see rooms, if complex enough, put in its own file
    (let ((posx (point-x pos))
	  (posy (point-y pos)))
      (for-each (lambda (x y) (if (inside-grid? g (new-point x y))
				  (grid-set! view (new-point x y) 'visible)))
		(list (- posx 1) (- posx 1) (- posx 1)
		      posx       posx       posx
		      (+ posx 1) (+ posx 1) (+ posx 1))
		(list (- posy 1) posy       (+ posy 1)
		      (- posy 1) posy       (+ posy 1)
		      (- posy 1) posy       (+ posy 1))))))



