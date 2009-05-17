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
(define (grid-get  g pos)
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
  (terminal-command "[H") ; go home
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
  (walkable-cell-object cell)) ;; TODO change with multiple objects
(define (add-object cell object)
  (walkable-cell-object-set! cell object)) ;; TODO change it when we have multiple objects
(define (remove-object cell object)
  (walkable-cell-object-set! cell #f)) ;; TODO change with multiple objects
(define (get-occupant cell)
  (walkable-call-occupant cell))
(define (occupant-set! cell occupant) ;; TODO add a check to see if it's a walkable cell ?
  (walkable-cell-occupant-set! cell occupant))

(define-type object
  printer
  extender: define-type-of-object)
(define-type-of-object treasure
  name) ;; TODO more
(define (new-treasure) (make-treasure (lambda () #\T)
				      (random-element object-names)))

(define-type occupant
  printer
  extender: define-type-of-occupant)
(define-type-of-occupant player
  name
  map
  pos
  view)
(define (new-player name map pos view)
  (make-player (lambda () #\@)
	       name
	       map
	       pos
	       view))

(define-type-of-cell wall-cell
  extender: define-type-of-wall-cell)
(define-type-of-wall-cell vertical-wall-cell)
(define-type-of-wall-cell horizontal-wall-cell)
(define-type-of-wall-cell corner-wall-cell)
(define (new-wall-cell) (make-wall-cell (lambda () #\#)))
(define (new-vertical-wall-cell)   (make-vertical-wall-cell   (lambda () #\|)))
(define (new-horizontal-wall-cell) (make-horizontal-wall-cell (lambda () #\-)))
(define (new-corner-wall-cell)     (make-corner-wall-cell     (lambda () #\+)))
