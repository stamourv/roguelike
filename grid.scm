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

(define (grid-for-each f g #!key
		       (start-x 0)                (start-y 0)
		       (length-x (grid-height g)) (length-y (grid-width g)))
  (for-each (lambda (x)
	      (for-each (lambda (y)
			  (f (new-point (+ x start-x) (+ y start-y))))
			(iota length-y)))
	    (iota length-x)))

(define (distance a b)
  (let ((x (abs (- (point-x b) (point-x a))))
	(y (abs (- (point-y b) (point-y a)))))
    (sqrt (+ (expt x 2) (expt y 2)))))

;; these functions return a list of points in the given directions from pos
;; these points might NOT be inside the grid
;; the order of the points is important, some functions depend on it
(define (up pos) ;; TODO FOO propagate the use of this, is probably done in an ad-hoc way everywhere
  (let ((x (point-x pos))
	(y (point-y pos)))
    (new-point (- x 1) y)))
(define (down pos)
  (let ((x (point-x pos))
	(y (point-y pos)))
    (new-point (+ x 1) y)))
(define (left pos)
  (let ((x (point-x pos))
	(y (point-y pos)))
    (new-point x (- y 1))))
(define (right pos)
  (let ((x (point-x pos))
	(y (point-y pos)))
    (new-point x (+ y 1))))
(define (up-down pos)    (list (up   pos) (down  pos)))
(define (left-right pos) (list (left pos) (right pos)))
(define (four-directions pos)
  (append (up-down pos) (left-right pos)))
(define (eight-directions pos)
  ;; same, but with diagonals
  (append (four-directions pos)
	  (let ((x (point-x pos))
		(y (point-y pos))) ;; TODO maybe have functions for these 4 other directions too ?
	    (list (new-point (- x 1) (- y 1)) (new-point (+ x 1) (- y 1))
		  (new-point (- x 1) (+ y 1)) (new-point (+ x 1) (+ y 1))))))

;; given a wall, returns the cells that are either perpendicular or
;; parrallel to the direction of the wall
(define (wall-perpendicular g pos)
  (let ((wall (grid-get g pos)))
    ((cond ((horizontal-wall? wall) up-down)
	   ((vertical-wall?   wall) left-right)
	   (else                    (lambda (x) '()))) ; not an appropriate wall
     pos)))
(define (wall-parrallel     g pos)
  (let ((wall (grid-get g pos)))
    ((cond ((horizontal-wall? wall) left-right)
	   ((vertical-wall?   wall) up-down)
	   (else                    (lambda (x) '()))) ; not an appropriate wall
     pos)))

(define (random-position g)
  (new-point (random-integer (grid-height g))
	     (random-integer (grid-width g))))

(define (show-grid g #!key (print-fun (lambda (pos) display)))
  (define (draw-border-line)
    (display "+")
    (for-each (lambda (x) (display "-")) (iota (grid-width g)))
    (display "+\n"))
  (cursor-home)
  (draw-border-line)
  (grid-for-each
   (lambda (pos)
     (if (= (point-y pos) 0) ; beginning of line
	 (display "|"))
     ((print-fun pos) ((cell-printer (grid-get g pos))))
     (if (= (point-y pos) (- (grid-width g) 1)) ; end of line
	 (display "|\n")))
   g)
  (draw-border-line))

(define (string->grid s)
  ;; the string must obviously give a rectangular shape
  (let* ((rows (split-string s #\newline))
	 (x    (length rows))
	 (y    (string-length (car rows)))
	 (g    (empty-grid x y)))
    (grid-for-each (lambda (pos)
		     (let ((x (point-x pos)) (y (point-y pos)))
		       (grid-set! g pos
				  (case (string-ref (list-ref rows x) y)
				    ((#\ ) (new-walkable-cell))
				    ((#\+) (new-corner-wall))
				    ((#\|) (new-vertical-wall))
				    ((#\-) (new-horizontal-wall))
				    ((#\#) (new-solid-wall)))))) ;; TODO add as we add
		   g)
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
