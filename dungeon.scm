(load "utilities.scm") ;; TODO have dependencies for all other files
(load "grid.scm")

(define (generate-level #!optional (trace? #f))
  ;; TODO have a limit linked to the size of the screen, or scroll ? if scrolling, query the terminal size
  ;; for now, levels are grids of 20 rows and 60 columns, to fit in a 80x25
  ;; terminal
  (let* ((level-height 20)
	 (level-width  60)
	 (level (empty-grid level-height level-width
			    cell-fun: (lambda (pos) (new-solid-wall-cell)))))
    
    (define trace 0) ;; to trace the generation of the dungeon
    (define (trace-cell)
      (let ((x (number->string (modulo trace 10))))
	(make-walkable-cell (lambda () x) #f #f)))
    
    (define (add-rectangle pos height width direction)
      ;; height and width consider a wall of one cell wide on each side
      (let* ((x     (point-x pos))
	     (y     (point-y pos))
	     (pos-x (case direction
		      ((south) x)
		      ;; expanding north, we have to move the top of the room
		      ;; up so the bottom reaches the starting point
		      ((north) (+ (- x height) 1))
		      ;; expanding east or west, position ourselves so the
		      ;; middle of the wall of the new room starts here
		      (else    (- x (floor (/ height 2))))))
	     (pos-y (case direction
		      ;; same idea as for x
		      ((east) y)
		      ((west) (+ (- y width) 1))
		      (else   (- y (floor (/ width 2)))))))
	(if (foldl ; can this new feature fit ?
	     (lambda (acc x)
	       (and acc
		    (foldl
		     (lambda (acc y)
		       (let ((p (new-point (+ x pos-x) (+ y pos-y))))
			 (and acc
			      (inside-grid? level p)
			      (wall-cell? (grid-get level p)))))
		     #t (iota width))))
	     #t (iota height))
	    
	    (let ((new-walls '())) ; yes it can, add it
	      (for-each
	       (lambda (x)
		 (for-each
		  (lambda (y)
		    (let ((p (new-point (+ x pos-x) (+ y pos-y))))
		      (grid-set!
		       level p
		       ;; find out the appropriate cell type
		       ((cond ((and (or (= x 0) (= x (- height 1)))
				    (or (= y 0) (= y (- width 1))))
			       ;; one of the four corners
			       new-corner-wall-cell)
			      ((or (= x 0) (= x (- height 1)))
			       ;; horizontal-wall
			       (set! new-walls (cons p new-walls))
			       new-horizontal-wall-cell)
			      ((or (= y 0) (= y (- width 1)))
			       ;; vertical wall
			       (set! new-walls (cons p new-walls))
			       new-vertical-wall-cell)
			      ;; inside of the room
			      (else (if trace?
					trace-cell
					new-walkable-cell)))))))
		  (iota width)))
	       (iota height))
	      ;; TODO add doors (for now free space) where we chose a wall to expand
	      ;; TODO the first on should not be a door, but the stairs up
	      ;; put a doorway between the 2 rooms
	      (if trace?
		  (begin (pp (list trace pos: (point-x pos) (point-y pos)
				   dir: direction height: height width: width))
			 (grid-set! level pos (trace-cell))
			 (set! trace (+ trace 1)))
		  (grid-set! level pos (new-walkable-cell)))
	      new-walls)
	    
	    #f))) ; no it can't, give up
    
    (define (add-small-room pos direction)
      ;; both dimensions 5-7 units (including walls)
      (add-rectangle pos (random-between 5 7) (random-between 5 7) direction))
    (define (add-large-room pos direction)
      ;; both dimensions 8-12 units (including walls)
      (add-rectangle pos (random-between 8 12) (random-between 8 12) direction))
    (define (add-corridor   pos direction)
      ;; width: 3, length: 5-17 (including walls)
      ;; TODO maybe wider corridors ?
      (if (or (eq? direction 'east) (eq? direction 'west))
	  ;; we generate an horizontal corridor
	  (add-rectangle pos 3 (random-between 5 17) direction)
	  (add-rectangle pos (random-between 5 17) 3 direction)))

    (define (add-random-feature pos)
      ;; find in which direction to expand from this wall
      (let ((direction
	     (let loop ((around     (four-directions pos))
			(directions '(south north east west)))
	       (cond ((null? around) ; walls all around
		      (random-element '(north south west east)))
		     ((and (inside-grid? level (car around))
			   (walkable-cell? (grid-get level (car around))))
		      ;; there is a free space in that direction, we must
		      ;; expand the opposite way
		      (car directions))
		     (else ;; keep looking
		      (loop (cdr around) (cdr directions))))))
	    (r (random-real)))
	;; 30% chance of corridor, 40% of large room, 30% of small room
	((cond ((< r 0.3) add-corridor)
	       ((< r 0.7) add-large-room)
	       (else      add-small-room))
	 pos direction)))

    ;; the number of features was chosen arbitrarily, with the placement
    ;; failures, this should give some nice results
    (let loop ((n 1000)
	       (walls (let loop ((res #f)) ; we place the first feature
			(if res
			    res
			    (loop (add-random-feature
				   (random-position level)))))))
      (if (> n 0)
	  (let* ((i     (random-integer (length walls)))
		 (start (list-ref walls i)))
	    (loop (- n 1)
		  (cond ((add-random-feature start) ;; TODO maybe instead of just being a list of positions, also have the type of room they are part of, so we can alter the probabilities of generating the same type of room from there (lower it, most likely)
			 => (lambda (more)
			      (append (remove-at-index walls i) more)))
			(else walls))))))
    ;; TODO maybe have a pass to randomly add doors to 2 rooms that are not directly connected, but still have a width 1 wall between them
    level))
;; TODO how to return the starting point ? so that we may add stairs, the player, etc, maybe also return a list of the free spaces
;; TODO maybe instead, just recalculate all that when we receive the map down the line, just just some stairs at this stage (and afterwards, find the stairs up to place the player)
