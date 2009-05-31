(define (init-visibility g)
  (empty-grid (grid-height g) (grid-width g)
	      cell-fun: (lambda (pos) 'unknown)))

(define (line-of-sight? g a b)
  ;; using Bresenham's algorithm to draw a line between a and b, see if we
  ;; hit any opaque objects
  ;; see: http://en.wikipedia.org/wiki/Bresenham%27s_line_algorithm
  (if (and (= (point-x a) (point-x b))
	   (= (point-y a) (point-y b)))
      #t ; same point, trivial solution
      (let* ((x0 (point-x a)) (y0 (point-y a)) ;; TODO maybe have a generic bresenham, could be used for other things
	     (x1 (point-x b)) (y1 (point-y b))
	     (steep (> (abs (- y1 y0)) (abs (- x1 x0)))))
	(if steep
	    (let ((tmp x0))    (set! x0 y0) (set! y0 tmp)
		 (set! tmp x1) (set! x1 y1) (set! y1 tmp)))
	(if (> x0 x1)
	    (let ((tmp x0))    (set! x0 x1) (set! x1 tmp)
		 (set! tmp y0) (set! y0 y1) (set! y1 tmp)))
	(let* ((delta-x   (- x1 x0))
	       (delta-y   (abs (- y1 y0)))
	       (delta-err (/ delta-y delta-x))
	       (y-step    (if (< y0 y1) 1 -1))
	       (start     (if steep (new-point y0 x0) (new-point x0 y0)))
	       (dest      (if steep (new-point y1 x1) (new-point x1 y1))))
	  (let loop ((error        0)
		     (x            x0)
		     (y            y0))
	    (let ((pos   (if steep (new-point y  x)  (new-point x  y)))
		  
		  (error (+ error delta-err)))
	      ;; TODO if we want it generic, it would be at this point that a user function would be called, I supposed
	      (cond ((equal? pos dest)          #t) ; we see it
		    ((and (opaque? (grid-get g pos)) (not (equal? pos start))) #f) ; we hit an obstacle
		    (else (let ((error (if (>= error 1/2)
					   (- error 1)  error))
				(y     (if (>= error 1/2)
					   (floor (+ y y-step)) y)))
			    (loop error (+ x 1) y))))))))))

(define (update-visibility player) ;; TODO maybe show visible parts in dark yellow instead of white background ? to simulate a lantern
  ;; set the fog of war
  (let ((view (player-view player))
	(pos   (player-pos player)))
    (grid-for-each (lambda (pos)
		     (if (eq? (grid-get view pos) 'visible)
			 (grid-set! view pos 'visited)))
		   view)

    ;; field of vision using shadow casting (spiral path FOV)
    ;; see http://roguebasin.roguelikedevelopment.org/index.php?title=Spiral_Path_FOV
    (let* ((g     (player-map player))
	   (x     (point-x    pos))
	   (y     (point-y    pos)))
      (let loop ((queue (list pos)))
	(define (pass-light pos new)
	  ;; enqueue cells depending on the orientation of new from pos
	  (let* ((pos-x (point-x pos)) (pos-y (point-y pos))
		 (new-x (point-x new)) (new-y (point-y new))
		 (dirs  (four-directions new))
		 (north (list-ref dirs 0))
		 (south (list-ref dirs 1))
		 (west  (list-ref dirs 2))
		 (east  (list-ref dirs 3)))
	    (cond ((< new-x pos-x) ; somewhere north
		   (cond ((= new-y pos-y) (list east north west)) ; due north
			 ((< new-y pos-y) (list north west))      ; north-west
			 ((> new-y pos-y) (list east north))))    ; north-east
		  ((> new-x pos-x) ; somewhere south
		   (cond ((= new-y pos-y) (list west south east)) ; due south
			 ((< new-y pos-y) (list west south))      ; south-west
			 ((> new-y pos-y) (list south east))))    ; south-east
		  ((< new-y pos-y) (list north west south))       ; due west
		  ((> new-y pos-y) (list south east north))       ; due east
		  (else ; we are at the starting point
		   (list east north west south)))))
	(if (not (null? queue))
	    (let ((new (car queue)))
	      (if (and (inside-grid? g new)
		       (not (eq? (grid-get view new) 'visible)) ; already seen
		       (<= (distance pos new) 7) ; within range ;; TODO have range in a variable, maybe a player trait (elves see farther?)
		       ;; do we have line of sight ? helps restrict the
		       ;; visibility down to a reasonable level
		       ;; note: line of sight is not necessary to see walls,
		       ;; this gives better results
		       (or (opaque? (grid-get g new))
			   (line-of-sight? g pos new)))
		  (begin (grid-set! view new 'visible) ; mark as lit
			 (if (not (opaque? (grid-get g new)))
			     (loop (append (cdr queue)
					   (pass-light pos new))))))
	      (loop (cdr queue)))))

      ;; one last pass to solve the problem case of walls that are hard to
      ;; see, which gives ugly results
      ;; to solve the problem, any wall next to a visible square is visible
      (grid-for-each
       (lambda (pos)
	 (if (and (opaque? (grid-get g pos))
		  (eq? (grid-get view pos) 'unknown)
		  (foldl (lambda (acc new)
			   (or acc
			       (and (inside-grid? g new)
				    (not (opaque? (grid-get g new)))
				    (eq? (grid-get view new) 'visible))))
			 #f (eight-directions pos)))
	     (grid-set! view pos 'visited)))
       view))))

;; returns a printing function for show-grid
(define (visibility-printer view)
  (lambda (pos cell)
    (let ((c ((cell-printer cell))))
      (case (grid-get view pos)
	((visible)
	 (if (wall? cell)
	     (display c)
	     (terminal-print c bg: 'white fg: 'black))) ;; TODO can we have colored objects with that ? not sure
	((visited)
	 ;; (terminal-print c bg: 'black fg: 'white)
	 ;; these are the default colors of the terminal, and not having to
	 ;; print the control characters speeds up the game
	 ;; we don't show enemies if they would be in the fog of war
	 (cond ((get-occupant cell) =>
		(lambda (occ)
		  (occupant-set! cell #f)
		  (display ((cell-printer cell)))
		  (occupant-set! cell occ)))
	       (else (display c)))) ; no enemy to hide
	((unknown)
	 (terminal-print " "))))))

(define (opaque? cell)
  (or (wall? cell)
;;       (cond ((get-occupant cell) => (lambda (o) (not (player? o))))
;; 	    (else #f)) ; disabled. gives more interesting monsters
      ))
