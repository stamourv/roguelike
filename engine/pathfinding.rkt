#lang racket

(require "../utilities/grid.rkt"
         "../utilities/cell.rkt")
(provide find-path)

;; simple pathfinding using A*
(define (find-path g a b
                   #:extra-heuristic [extra-heuristic (lambda (g pos) 0)])
  ;; grid of pairs (cost . previous)
  (let* ((height  (grid-height g))
	 (width   (grid-width g))
	 (maximum (* width height)) ; arbitrarily high value
	 (costs (empty-grid
		 height width
		 #:cell-fun (lambda (pos)
                              (cons (if (walkable-cell? (grid-ref g pos))
                                        maximum
                                        #f) ; we can't even get there
                                    #f)))))
    (grid-set! costs a (cons 0 #f)) ; initialize
    (let loop ((queue (list a))) ; list of positions
      (if (null? queue)
	  ;; we have found a path, we return its first step
	  (let loop ((pos  b)
		     (prev #f))
	    (let ((parent (cdr (grid-ref costs pos))))
	      (if parent
		  (loop parent pos)
		  prev)))
	  (let* ((next (foldl (lambda (new best) ; least expensive neighbor
			       (if (< (car (grid-ref costs new))
				      (car (grid-ref costs best)))
				   new
				   best))
			     (car queue)
			     queue))
		 (queue (remove next queue))
		 (neighbors
		  (filter
		   (lambda (pos)
		     (if (inside-grid? g pos)
			 (cond
			  ((car (grid-ref costs pos)) =>
			   (lambda (cost)
			     (let ((new-cost
				    (+ (car (grid-ref costs next))
				       ;; heuristic cost
				       (distance pos b)
                                       (extra-heuristic g pos))))
			       (if (< new-cost cost)
				   (begin
				     (grid-set! costs pos (cons new-cost next))
				     #t)
				   #f))))
			  (else #f))
			 #f))
		   (four-directions next))))
	    (loop (append neighbors queue)))))))
