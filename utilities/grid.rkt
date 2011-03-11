#lang racket

(require (only-in srfi/1 iota))
(provide (all-defined-out))

(define-struct point (x y) #:mutable #:transparent)
(define new-point    make-point) ; for consistency
(define (copy-point p) (make-point (point-x p) (point-y p)))

(define (point-add p1 p2) (make-point (+ (point-x p1) (point-x p2))
                                      (+ (point-y p1) (point-y p2))))

;; vector of cells
(define-struct grid (height width cells) #:transparent)
(define (empty-grid height (width height)
		    ;; function that takes the position, and returns the content
		    #:cell-fun [cell-fun (lambda (pos) #f)])
  (make-grid height width
	     (list->vector
	      (map (lambda (p) (cell-fun (new-point (quotient p width)
						    (modulo   p width))))
		   (iota (* height width))))))
;; in all cases, x is the row, y is the column
(define (pos->index     g pos)
  (+ (* (point-x pos) (grid-width g)) (point-y pos)))
(define (grid-ref       g pos)
  (vector-ref (grid-cells g) (pos->index g pos)))
(define (grid-ref-check g pos)
  (if (inside-grid? g pos) (grid-ref g pos) #f))
(define (grid-set!      g pos v)
  (vector-set! (grid-cells g) (pos->index g pos) v))
(define (grid-copy g)
  (make-grid (grid-height g)
	     (grid-width  g)
	     (vector-copy (grid-cells g))))

(define (inside-grid? g pos)
  (let ((x (point-x pos))
	(y (point-y pos)))
    (and (>= x 0) (< x (grid-height g))
	 (>= y 0) (< y (grid-width  g)))))


(define (grid-for-each f g
		       #:start-x (start-x 0) #:start-y (start-y 0)
		       #:length-x (length-x (grid-height g))
                       #:length-y (length-y (grid-width g)))
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
(define (up pos)    (point-add pos (new-point -1 0)))
(define (down pos)  (point-add pos (new-point 1  0)))
(define (left pos)  (point-add pos (new-point 0  -1)))
(define (right pos) (point-add pos (new-point 0  1)))
(define (up-down pos)    (list (up   pos) (down  pos)))
(define (left-right pos) (list (left pos) (right pos)))
(define (four-directions pos) (append (up-down pos) (left-right pos)))
(define (up-left pos)    (point-add pos (new-point -1 -1)))
(define (down-left pos)  (point-add pos (new-point 1  -1)))
(define (up-right pos)   (point-add pos (new-point -1  1)))
(define (down-right pos) (point-add pos (new-point 1   1)))
(define (diagonals pos)
  (list (up-left pos) (down-left pos) (up-right pos) (down-right pos)))
(define (eight-directions pos) (append (four-directions pos) (diagonals pos)))


(define (grid-find g p)
  (let ((cell #f))
    (grid-for-each (lambda (pos) (when (p (grid-ref g pos)) (set! cell pos)))
		   g)
    cell))

(define (random-position g)
  (new-point (random (grid-height g))
	     (random (grid-width g))))


(define (octant c p)
  ;; TODO might have bugs, an algorithm built on top of it (the old shadow
  ;;  casting) failed, so it might be because of this (algorithm now in
  ;;  garbage) (not used for now)
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
                ;; TODO really not sure for the last 2 lines, the right
                ;;  theoretical result gives wrong practical results
		((> slope 0)         (if (> c-x p-x) 'ne 'sw))
		(else                (if (> c-x p-x) 'nw 'se)))))))
