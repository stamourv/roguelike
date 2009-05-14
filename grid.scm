;; vector of vectors of cells
(define-type grid
  rows)
(define (empty-grid height
		    #!optional (width height)
		    ;; function that takes the x and y, and returns the content
		    #!key (cell-fun (lambda (x y) (new-unknown-cell))))
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
(define (show-grid g)
  (define (draw-border-line)
    (display "+")
    (for-each (lambda (x) (display "-")) (iota (grid-width g)))
    (display "+\n"))
  (draw-border-line)
  (for-each (lambda (row)
	      (display "|")
	      (for-each (lambda (cell) (display ((cell-printer cell))))
			(vector->list row))
	      (display "|\n"))
	    (vector->list (grid-rows g)))
  (draw-border-line))


(define-type cell
  printer ; thunk that returns a character
  extender: define-type-of-cell)
(define-type-of-cell unknown-cell)
(define (new-unknown-cell) (make-unknown-cell (lambda () #\?)))

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
(define-type-of-cell player-cell
  player)
(define (new-player-cell player) (make-player-cell (lambda () #\@) player))

(define-type-of-cell treasure-cell)
(define (new-treasure-cell) (make-treasure-cell (lambda () #\T)))
