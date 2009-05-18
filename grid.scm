(define-type point
  x
  y)
(define new-point make-point) ; for consistency
(define (copy-point p) (new-point (point-x p) (point-y p)))

;; vector of vectors of cells
(define-type grid
  rows)
(define (empty-grid height
		    #!optional (width height)
		    ;; function that takes the position, and returns the content
		    #!key (cell-fun (lambda (pos) (new-walkable-cell))))
  (make-grid (list->vector
	      (map (lambda (x) (list->vector
				(map (lambda (y) (cell-fun (new-point x y)))
				     (iota width))))
		   (iota height)))))
;; in all cases, x is the row, y is the column
(define (grid-get  g pos) ;; TODO call grid-ref ?
  (vector-ref (vector-ref (grid-rows g) (point-x pos))
	      (point-y pos)))
(define (grid-set! g pos v)
  (vector-set! (vector-ref (grid-rows g) (point-x pos))
	       (point-y pos)
	       v))
(define (grid-height g) (vector-length (grid-rows g)))
(define (grid-width  g) (vector-length (vector-ref (grid-rows g) 0)))
(define (inside-grid? g pos)
  (let ((x (point-x pos))
	(y (point-y pos)))
    (and (>= x 0) (< x (grid-height g))
	 (>= y 0) (< y (grid-width  g)))))

(define (show-grid g #!optional (view #f)) ;; TODO have a limit linked to the size of the screen, or scroll ? if scrolling, query the terminal size
  (define (draw-border-line)
    (display "+")
    (for-each (lambda (x) (display "-")) (iota (grid-width g)))
    (display "+\n"))
  (cursor-home)
  (draw-border-line)
  (for-each
   (lambda (x)
     (display "|")
     (for-each
      (lambda (y)
	(let ((cell       (grid-get g (new-point x y)))
	      (visibility (if view (grid-get view (new-point x y)) 'visible)))
	  (case visibility
	    ((visible)
	     (terminal-print ((cell-printer cell)) bg: 'white fg: 'black)) ;; TODO have the visibility as a parameter to the printer ? if not, we can't have colored objects, since they absolutely have to return a char, maybe have them return a string, which could contain vt100 commands ?
	    ((visited)
	     (terminal-print ((cell-printer cell)) bg: 'black fg: 'white))
	    ((unknown)
	     (terminal-print "?")))))
      (iota (grid-width g)))
     (display "|\n"))
   (iota (grid-height g)))
  (draw-border-line))

(define (string->grid s)
  ;; the string must obviously give a rectangular shape
  (let* ((rows (split-string s #\newline))
	 (x    (length rows))
	 (y    (string-length (car rows)))
	 (g    (empty-grid x y)))
    (for-each (lambda (x)
		(for-each (lambda (y)
			    (grid-set! g (new-point x y)
				       (case (string-ref (list-ref rows x) y)
					 ((#\ ) (new-walkable-cell))
					 ((#\+) (new-corner-wall-cell))
					 ((#\|) (new-vertical-wall-cell))
					 ((#\-) (new-horizontal-wall-cell)))))
			  (iota y)))
	      (iota x))
    g))

(define (move g pos new-pos)
  ;; moves the occupant of pos to new-pos, and returns the position of the
  ;; occupant (the new one or the original, if the move fails)
  (if (and (inside-grid?   g new-pos)
	   (walkable-cell? (grid-get g new-pos)))
      (let* ((cell     (grid-get g pos))
	     (new-cell (grid-get g new-pos))
	     (to-move  (walkable-cell-occupant cell)))
	(walkable-cell-occupant-set! cell     #f)
	(walkable-cell-occupant-set! new-cell to-move)
	new-pos)
      pos)) ; move failed

(define (set-cursor-on-grid grid pos)
  (let ((x (point-x pos))
	(y (point-y pos)))
    ;; we have to account for the borders
    (if (inside-grid? grid pos)
	(cursor-position-set! (+ x 2) (+ y 2))
	#f)))

(define (octant c p) ;; TODO might have bugs, an algorithm built on top of it (the old shadow casting) failed, so it might be because of this (algorithm now in garbage)
  ;; returns in which octant p is with c as the center
  ;; octants are n, ne, e, se, s, sw, w, nw (or c if we are on the center)
  (let ((c-x (point-x c))
	(c-y (point-y c))
	(p-x (point-x p))
	(p-y (point-y p)))
    (if (= c-y p-y)
	;; get due north and due south out of the way, to infinite slopes
	(cond ((> c-x p-x) 'n)
	      ((< c-x p-x) 's)
	      (else        'c)) ; right on the center
	(let ((slope (/ (- p-x c-x) (- p-y c-y))))
	  (cond ((< (abs slope) 1/2) (if (> c-y p-y) 'w  'e))
		((> (abs slope) 2)   (if (> c-x p-x) 'n  's))
		((> slope 0)         (if (> c-x p-x) 'ne 'sw)) ;; TODO really not sure for the last 2 lines, the right theoretical result gives wrong practical results
		(else                (if (> c-x p-x) 'nw 'se)))))))

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
	       (delta-y   (- y1 y0))
	       (delta-err (/ delta-y delta-x))
	       (y-step    (if (< y0 y1) 1 -1)))
	  (let loop ((error        0)
		     ;; we can see opaque squares, but not behind
		     (seen-opaque? #f)
		     (x            x0)
		     (y            y0))
	    (if (> x x1)
		#t ; no collision, we have line of sight
		(let ((pos   (if steep (new-point y x) (new-point x y)))
		      (error (+ error delta-err)))
		  ;; TODO if we want it generic, it would be at this point that a user function would be called, I supposed
		  (if seen-opaque?
		      #f ; we hit an obstacle, we don't have line of sight
		      (let ((error (if (>= error 1/2) (- error 1)  error))
			    (y     (if (>= error 1/2) (+ y y-step) y))
			    (seen-opaque? (opaque-cell? (grid-get g pos))))
			(loop error seen-opaque? (+ x 1) y))))))))))


(define-type cell
  printer    ; thunk that returns a character
  extender: define-type-of-cell)

(define-type-of-cell walkable-cell
  object ;; TODO have a list instead
  occupant ; player, monster, ...
  extender: define-type-of-walkable-cell)
(define (new-walkable-cell)
  (let ((cell (make-walkable-cell #f #f #f)))
    (cell-printer-set! cell (lambda ()
			      (cond ((walkable-cell-occupant cell)
				     => (lambda (o) ((occupant-printer o))))
				    ((walkable-cell-object cell)
				     => (lambda (o) ((object-printer o))))
				    (else #\space))))
    cell))
(define (get-object cell)
  (if (walkable-cell? cell)
      (walkable-cell-object cell)
      #f)) ;; TODO change with multiple objects
(define (add-object cell object)
  (if (walkable-cell? cell)
      (walkable-cell-object-set! cell object)
      #f)) ;; TODO change it when we have multiple objects
(define (remove-object cell object)
  (if (walkable-cell? cell)
      (walkable-cell-object-set! cell #f)
      #f)) ;; TODO change with multiple objects
(define (get-occupant cell)
  (if (walkable-cell? cell)
      (walkable-cell-occupant cell)
      #f))
(define (occupant-set! cell occupant)
  (if (walkable-cell? cell)
      (walkable-cell-occupant-set! cell occupant)
      #f))

(define-type object
  name
  printer
  extender: define-type-of-object)
(define-type-of-object treasure) ;; TODO more
(define (new-treasure) (make-treasure (random-element object-names)
				      (lambda () #\T)))

(define-type occupant
  name
  printer
  extender: define-type-of-occupant)

(define-type-of-cell wall-cell
  extender: define-type-of-wall-cell)
(define-type-of-wall-cell vertical-wall-cell)
(define-type-of-wall-cell horizontal-wall-cell)
(define-type-of-wall-cell corner-wall-cell)
(define (new-wall-cell) (make-wall-cell (lambda () #\#)))
(define (new-vertical-wall-cell)   (make-vertical-wall-cell   (lambda () #\|)))
(define (new-horizontal-wall-cell) (make-horizontal-wall-cell (lambda () #\-)))
(define (new-corner-wall-cell)     (make-corner-wall-cell     (lambda () #\+)))

(define (opaque-cell? cell) (wall-cell? cell)) ;; TODO add as other opaque cell types are added
