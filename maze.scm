;; maze generation using randomized Kruskal's algorithm
;; http://en.wikipedia.org/wiki/Maze_generation_algorithm
(define (generate-maze height width)
  ;; since walls must be cells, a maze of width n needs a grid of width 2n-1
  ;; same for height
  (let* ((grid-h (- (* 2 height) 1))
	 (grid-w (- (* 2 width)  1))
	 ;; cells (0,0), (0,2), (0,4), ..., (2,0), ... are always free
	 ;; the rest start out as walls, but can become free
	 (grid (empty-grid grid-h grid-w
			   cell-fun: (lambda (pos)
				       (let* ((x  (point-x pos))
					      (y  (point-y pos))
					      (mx (modulo  x 2))
					      (my (modulo  y 2)))
					 (cond ((and (= mx 0) (= my 0))
						(new-walkable-cell))
					       ((and (= mx 1) (= my 1))
						(new-corner-wall))
					       ((and (= mx 1) (= my 0))
						(new-horizontal-wall))
					       ((and (= mx 0) (= my 1))
						(new-vertical-wall)))))))
	 (sets    (empty-grid height width
			      cell-fun: (lambda (pos) (new-set))))
	 (wall-list '()))

    ;; if the wall separates cells that are not in the same set, remove it
    ;; note : since corners do not actually separate cells, they are not
    ;; removed (and they won't need to, since they will always be linked to
    ;; another wall)
    (define (maybe-remove-wall pos)
      (let ((x    (point-x pos))
	    (y    (point-y pos))
	    (wall (grid-get grid pos)))
	(if (not (corner-wall? wall))
	    (let* ((ax    (/ (if (horizontal-wall? wall) (+ x 1) x) 2))
		   (ay    (/ (if (vertical-wall?   wall) (+ y 1) y) 2))
		   (bx    (/ (if (horizontal-wall? wall) (- x 1) x) 2))
		   (by    (/ (if (vertical-wall?   wall) (- y 1) y) 2))
		   (a     (new-point ax ay))
		   (b     (new-point bx by))
		   (set-a (grid-get sets a))
		   (set-b (grid-get sets b)))
	      (if (not (set-equal? set-a set-b))
		  (let ((new (set-union set-a set-b)))
		    (grid-set! grid pos (new-walkable-cell)) ; remove wall
		    (grid-set! sets a new)
		    (grid-set! sets a new)))))))

    ;; fill the list of wall cells
    (grid-for-each (lambda (pos)
		     (if (wall? (grid-get grid pos))
			 (set! wall-list (cons pos wall-list))))
		   grid)

    ;; randomly remove walls to get a connected area
    (set! wall-list (randomize-list wall-list))
    (for-each maybe-remove-wall wall-list)
    
    grid))
