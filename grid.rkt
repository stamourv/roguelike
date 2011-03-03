#lang racket

(require "utilities.rkt" "cell.rkt" "display.rkt" "terminal.rkt"
         (only-in srfi/1 iota))
(provide (all-defined-out))

(define-struct point (x y) #:mutable #:transparent)
(define new-point    make-point) ; for consistency
(define (copy-point p) (make-point (point-x p) (point-y p)))

;; vector of cells
(define-struct grid (height width cells) #:transparent)
(define (empty-grid height (width height)
		    ;; function that takes the position, and returns the content
		    #:cell-fun (cell-fun (lambda (pos) (new-empty-cell))))
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
(define (up pos)
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
(define (up-left pos)
  (let ((x (point-x pos))
	(y (point-y pos)))
    (new-point (- x 1) (- y 1))))
(define (down-left pos)
  (let ((x (point-x pos))
	(y (point-y pos)))
    (new-point (+ x 1) (- y 1))))
(define (up-right pos)
  (let ((x (point-x pos))
	(y (point-y pos)))
    (new-point (- x 1) (+ y 1))))
(define (down-right pos)
  (let ((x (point-x pos))
	(y (point-y pos)))
    (new-point (+ x 1) (+ y 1))))
(define (diagonals pos)
  (list (up-left pos) (down-left pos) (up-right pos) (down-right pos)))
(define (eight-directions pos) (append (four-directions pos) (diagonals pos)))

(define (cartesian-product l1 l2)
  (apply append
	 (map (lambda (x) (map (lambda (y) (new-point x y))
			       l2))
	      l1)))

;; given a wall, returns the cells that are either perpendicular or
;; parrallel to the direction of the wall
(define (wall-perpendicular g pos)
  (let ((wall (grid-ref g pos)))
    ((cond ((horizontal-wall? wall) up-down)
	   ((vertical-wall?   wall) left-right)
	   (else                    (lambda (x) '()))) ; not an appropriate wall
     pos)))
(define (wall-parrallel     g pos)
  (let ((wall (grid-ref g pos)))
    ((cond ((horizontal-wall? wall) left-right)
	   ((vertical-wall?   wall) up-down)
	   (else                    (lambda (x) '()))) ; not an appropriate wall
     pos)))

(define (next-to-a-door? g pos)
  (ormap (lambda (new) (door? (grid-ref-check g new)))
         (four-directions pos)))

(define (grid-find g p)
  (let ((cell #f))
    (grid-for-each (lambda (pos) (when (p (grid-ref g pos)) (set! cell pos)))
		   g)
    cell))

(define (random-position g)
  (new-point (random (grid-height g))
	     (random (grid-width g))))

(define (show-grid
	 g
	 #:print-fun (print-fun (lambda (pos cell) (print-sprite (show cell))))
	 #:border? (border? #f))
  (define (draw-border-line)
    (when border?
      (display "+")
      (for-each (lambda (x) (display "-")) (iota (grid-width g)))
      (display "+\n")))
  (draw-border-line)
  (grid-for-each
   (lambda (pos)
     (when (and border? (= (point-y pos) 0)) ; beginning of line
       (display "|"))
     (print-fun pos (grid-ref g pos))
     (when (= (point-y pos) (- (grid-width g) 1)) ; end of line
       (when border? (display "|"))
       (display "\n")))
   g)
  (draw-border-line))

(define (set-cursor-on-grid grid pos)
  (let ((x (point-x pos))
	(y (point-y pos)))
    ;; we have to account for the borders
    (if (inside-grid? grid pos)
	(set-cursor-position! (+ x 2) (+ y 2))
	#f)))

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
