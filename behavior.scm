(define (rush)
  (lambda (monster floor player-pos)
    (let ((pos (character-pos monster))
	  (map (floor-map floor)))
      (if (line-of-sight? map pos player-pos)
	  (let ((next (find-path map pos player-pos)))
	    (if next (move map monster next)))))))
(define (pursue)
  (let ((last-player-pos #f)) ; AI state
    (lambda (monster floor player-pos)
      (let* ((pos    (character-pos monster))
	     (map    (floor-map floor))
	     (target (cond ((line-of-sight? map pos player-pos)
			    (set! last-player-pos player-pos)
			    player-pos)
			   (else last-player-pos)))) ; we try to pursue
	(let ((next (and target (find-path map pos target))))
	  (if next (move map monster next)))))))
;; TODO another behavior (pursue?) that, even if it does not see the player, remember where it was, and goes there

;; simple pathfinding using A*
(define (find-path g a b) ;; TODO avoid piling up on other monsters. to do that, consider other monsters as walkable, but eith an additionnal cost that is equal (proportional) to the number of turns the monster has been staying there. for now, not useful, since monsters can't even see over other monsters
  ;; grid of pairs (cost . previous)
  (let* ((height  (grid-height g))
	 (width   (grid-width g))
	 (maximum (* width height)) ; arbitrarily high value
	 (costs (empty-grid
		 height width
		 cell-fun: (lambda (pos)
			     (cons (if (walkable-cell? (grid-get g pos))
				       maximum
				       #f) ; we can't even get there
				   #f)))))
    (grid-set! costs a (cons 0 #f)) ; initialize
    (let loop ((queue (list a))) ; list of positions
      (if (null? queue)
	  ;; we have found a path, we return its first step
	  (let loop ((pos  b)
		     (prev #f))
	    (let ((parent (cdr (grid-get costs pos))))
	      (if parent
		  (loop parent pos)
		  prev)))
	  (let* ((next (foldl (lambda (best new) ; least expensive neighbor
				(if (< (car (grid-get costs new))
				       (car (grid-get costs best)))
				    new
				    best))
			      (car queue)
			      queue))
		 (queue     (remove next queue))
		 (neighbors (filter
			     (lambda (pos)
			       (if (inside-grid? g pos)
				   (cond
				    ((car (grid-get costs pos)) =>
				     (lambda (cost)
				       (let ((new-cost
					      (+ (car (grid-get costs next))
						 ;; heuristic cost
						 (distance pos b))))
					 (if (< new-cost cost)
					     (begin
					       (grid-set! costs pos
							  (cons new-cost next))
					       #t)
					     #f))))
				    (else #f))
				   #f))
			     (four-directions next))))
	    (loop (append neighbors queue)))))))
