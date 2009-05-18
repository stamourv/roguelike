(load "utilities.scm") ;; TODO have dependencies for all other files
(load "grid.scm")

(define (generate-level) ;; TODO have a limit linked to the size of the screen, or scroll ? if scrolling, query the terminal size
  ;; for now, levels are grids of 20 rows and 60 columns, to fit in a 80x25
  ;; terminal
  (let* ((level-height 20)
	 (level-width  60)
	 (level (empty-grid level-height level-width
			    cell-fun: (lambda (pos) (new-solid-wall-cell)))))
    (define (add-rectangle pos height width)
      ;; height and width consider a wall of one cell wide on each side
      ;; TODO if we add a layer of walls, how to make sure we can reach inside ? maybe corridors don't have these, or have doors at the ends ? and maybe on some sides, or maybe add doors after, add doors to adjacent rooms (separated by 1-2 (depends on how I implement this) walls) until all rooms are in the same set
      (let ((pos-x (point-x pos))
	    (pos-y (point-y pos)))
	(if (foldl
	     (lambda (acc x)
	       (and (foldl
		     (lambda (acc y)
		       (let ((p (new-point (+ x pos-x) (+ y pos-y))))
			 (and (inside-grid? level p)
			      (wall-cell? (grid-get level p))
			      acc)))
		     #t
		     (iota width))
		    acc))
	     #t
	     (iota height))
	    (let ((new-walls '()))
	      (for-each
	       (lambda (x)
		 (for-each
		  (lambda (y)
		    (let ((p (new-point (+ x pos-x) (+ y pos-y))))
		      (grid-set!
		       level p
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
			      (else new-walkable-cell))))))
		  (iota width)))
	       (iota height))
	      new-walls)
	    #f)))
    (define (add-small-room pos) ; both dimensions 5-7 units (including walls)
      (add-rectangle pos (random-between 5 7) (random-between 5 7)))
    (define (add-large-room pos) ; both dimensions 8-12 units (including walls)
      (add-rectangle pos (random-between 8 12) (random-between 8 12)))
    (define (add-corridor   pos) ; width: 3, length: 5-17 (including walls)
      ;; TODO maybe larger corridors ?
      ;; TODO have vertical and horizontal corridors when it makes sense ? see which sides of the current wall are other walls (we can't be on a corner, they are not added to the list)
      (add-rectangle pos 3 (random-between 5 17)))
    (define (add-random-feature pos)
      ;; 50% of corridor, 30% of large room, 20% of small room ;; TODO tweak
      (let ((r (random-real)))
	(cond ((< r 0.5) (add-corridor pos))
	      ((< r 0.8) (add-large-room pos))
	      (else      (add-small-room pos)))))
    (define (random-position)
      (new-point (random-integer level-height) (random-integer level-width)))

    ;; the number of features was chosen arbitrarily, with the placement
    ;; failures, this should give some nice results
    (let loop ((n 20)
	       (walls (let loop ((res (add-random-feature (random-position))))
			(if res ; we place the first feature
			    res
			    (loop (add-random-feature (random-position)))))))
      (if (> n 0)
	  (let* ((i     (random-integer (length walls)))
		 (start (list-ref walls i))
		 (walls (remove-at-index walls i)))
	    (loop (- n 1)
		  (cond ((add-random-feature start)
			 => (lambda (more) (append walls more)))
			(else walls)))))) ;; TODO see if it works properly, and add oriented corridors

;;     (for-each (lambda (x) (add-random-feature (random-position)))
;; 	      (iota 20))
    ;; (add-rectangle (new-point 2 2) 10 12)
;;     (add-small-room (new-point 6 1))
;;     (add-large-room (new-point 1 15))
;;     (add-corridor   (new-point 15 5))
    level))
