(define-type behavior
  fun
  nb-turns-idle)

(define (new-behavior fun)
  (let ((b (make-behavior #f 0)))
    (behavior-fun-set! b (fun b))
    b))

(define (rush-behavior) (new-behavior rush))
(define (rush b)
  (lambda (monster floor player-pos)
    (let ((pos (character-pos monster))
	  (map (floor-map floor)))
      (if (line-of-sight? map pos player-pos)
	  (let ((next (find-path map pos player-pos)))
	    (if next (move-or-increment-idle map monster next)))))))

(define (pursue-behavior) (new-behavior pursue))
(define (pursue b)
  (let ((last-player-pos #f)) ; AI state
    (lambda (monster floor player-pos)
      (let* ((pos    (character-pos monster))
	     (map    (floor-map floor))
	     (target (cond ((line-of-sight? map pos player-pos)
			    (set! last-player-pos player-pos)
			    player-pos)
			   (else last-player-pos)))) ; we try to pursue
	(let ((next (and target (find-path map pos target))))
	  (if next (move-or-increment-idle map monster next)))))))

(define (move-or-increment-idle map monster dest)
  (let ((pos (character-pos monster)))
    (move map monster dest)
    (if (equal? pos (character-pos monster))
	;; we did not move, increment idle counter
	(let ((b (monster-behavior monster)))
	  (behavior-nb-turns-idle-set! b (+ (behavior-nb-turns-idle b) 1))))))

;; simple pathfinding using A*
(define (find-path g a b)
  ;; grid of pairs (cost . previous)
  (let* ((height  (grid-height g))
	 (width   (grid-width g))
	 (maximum (* width height)) ; arbitrarily high value
	 (costs (empty-grid
		 height width
		 cell-fun: (lambda (pos)
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
	  (let* ((next (foldl (lambda (best new) ; least expensive neighbor
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
				       ;; if we would pass through another
				       ;; monster, take into account the number
				       ;; of turns it has been stuck there, to
				       ;; avoid congestion
				       (let ((occ (cell-occupant
						   (grid-ref g pos))))
					 (if (and occ (monster? occ))
					     (* (behavior-nb-turns-idle
						 (monster-behavior occ))
						5)
					     0)))))
			       (if (< new-cost cost)
				   (begin
				     (grid-set! costs pos (cons new-cost next))
				     #t)
				   #f))))
			  (else #f))
			 #f))
		   (four-directions next))))
	    (loop (append neighbors queue)))))))
