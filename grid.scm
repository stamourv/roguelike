;; vector of vectors of cells
(define-type grid
  rows)
(define (empty-grid height
		    #!optional (width height)
		    ;; function that takes the x and y, and returns the content
		    #!key (cell-fun (lambda (x y) (new-walkable-cell))))
  (make-grid (list->vector
	      (map (lambda (x) (list->vector
				(map (lambda (y) (cell-fun x y))
				     (iota width))))
		   (iota height)))))
;; in all cases, x is the row, y is the column
(define (grid-get  g x y)   (vector-ref  (vector-ref (grid-rows g) x) y))
(define (grid-set! g x y v) (vector-set! (vector-ref (grid-rows g) x) y v))
(define (grid-height g) (vector-length (grid-rows g)))
(define (grid-width  g) (vector-length (vector-ref (grid-rows g) 0)))
(define (inside-grid? g x y) (and (>= x 0) (< x (grid-height g))
				  (>= y 0) (< y (grid-width  g))))
(define (show-grid g #!optional (view #f)) ;; TODO have a limit linked to the size of the screen, or scroll ? if scrolling, query the terminal size
  (define (draw-border-line)
    (display "+")
    (for-each (lambda (x) (display "-")) (iota (grid-width g)))
    (display "+\n"))
  ;; clear the screen. ugly, but the clear code does not seem to be supported
  ;; by gambit
  (for-each (lambda (dummy) (display "\n")) (iota 50)) ;; TODO use window size
  (terminal-command "[H") ; go home
  (draw-border-line)
  (for-each (lambda (x)
	      (display "|")
	      (for-each (lambda (y)
			  (let ((cell       (grid-get g x y))
				(visibility (if view
						(grid-get view x y)
						'visible)))
			    (case visibility
			      ((visible)
			       (terminal-print ((cell-printer cell))
					       bg: 'white fg: 'black)) ;; TODO actually have the visibility as a parameter to the printer ?
			      ((visited)
			       (terminal-print ((cell-printer cell))
					       bg: 'black fg: 'white))
			      ((unknown)
			       (terminal-print "?")))))
			(iota (grid-width g)))
	      (display "|\n"))
	    (iota (grid-height g)))
  (draw-border-line))


(define-type cell
  printer    ; thunk that returns a character
  extender: define-type-of-cell)

(define-type-of-cell walkable-cell
  extender: define-type-of-walkable-cell)
(define (new-walkable-cell) (make-walkable-cell (lambda () #\space)))

(define-type-of-cell wall-cell
  extender: define-type-of-wall-cell)
(define-type-of-wall-cell vertical-wall-cell)
(define-type-of-wall-cell horizontal-wall-cell)
(define-type-of-wall-cell corner-wall-cell)
(define (new-wall-cell) (make-wall-cell (lambda () #\#)))
(define (new-vertical-wall-cell)   (make-vertical-wall-cell   (lambda () #\|)))
(define (new-horizontal-wall-cell) (make-horizontal-wall-cell (lambda () #\-)))
(define (new-corner-wall-cell)     (make-corner-wall-cell     (lambda () #\+)))

(define-type player
  name)
(define-type-of-cell player-cell ;; TODO not have as a cell, this is not terrain
  player)
(define (new-player-cell player) (make-player-cell (lambda () #\@) player))

(define-type-of-cell treasure-cell)
(define (new-treasure-cell) (make-treasure-cell (lambda () #\T)))
