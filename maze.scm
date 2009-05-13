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
			   cell-fun: (lambda (x y)
				       (let ((mx (modulo x 2))
					     (my (modulo y 2)))
					 (cond ((and (= mx 0) (= my 0))
						(new-walkable-cell))
					       ((and (= mx 1) (= my 1))
						(new-corner-wall-cell))
					       ((and (= mx 1) (= my 0))
						(new-horizontal-wall-cell))
					       ((and (= mx 0) (= my 1))
						(new-vertical-wall-cell)))))))
	 (sets    (empty-grid height width
			      cell-fun: (lambda (x y) (new-set))))
	 (wall-list '()))

    ;; if the wall separates cells that are not in the same set, remove it
    ;; note : since corners do not actually separate cells, they are not
    ;; removed (and they won't need to, since they will always be linked to
    ;; another wall)
    (define (maybe-remove-wall w)
      (let* ((x    (car w))
	     (y    (cdr w))
	     (wall (grid-get grid x y)))
	(if (not (corner-wall-cell? wall))
	    (let* ((ax   (/ (if (horizontal-wall-cell? wall) (+ x 1) x) 2))
		   (ay   (/ (if (vertical-wall-cell?   wall) (+ y 1) y) 2))
		   (bx   (/ (if (horizontal-wall-cell? wall) (- x 1) x) 2))
		   (by   (/ (if (vertical-wall-cell?   wall) (- y 1) y) 2))
		   (a    (grid-get sets ax ay))
		   (b    (grid-get sets bx by)))
	      (if (not (set-equal? a b))
		  (let ((new (set-union a b)))
		    (grid-set! grid x y (new-walkable-cell)) ; remove wall
		    (grid-set! sets ax ay new)
		    (grid-set! sets bx by new)))))))

    ;; fill the list of wall cells
    (for-each (lambda (x)
		(for-each (lambda (y)
			    (if (wall-cell? (grid-get grid x y))
				(set! wall-list
				      (cons (cons x y) wall-list))))
			  (iota grid-w)))
	      (iota grid-h))

    ;; randomly remove walls to get a connected area
    (set! wall-list (randomize-list wall-list))
    (for-each maybe-remove-wall wall-list)
    
    grid))
