#lang racket

(require "utilities/cell.rkt"
         "utilities/grid.rkt"
         "utilities/floor-utils.rkt"
         "utilities/display.rkt")
(require "character.rkt"
         "monsters.rkt"
         "items.rkt"
         "visibility.rkt")
(provide (all-defined-out))

(define (rush-behavior)
  (new-behavior
   (lambda (b)
     (lambda (monster player-pos) ;; TODO have a macro for all that ?
       (let ((pos (character-pos monster))
	     (map (floor-map (character-floor monster))))
	 (when (line-of-sight? map pos player-pos)
           (let ((next (find-path map pos player-pos)))
             (when next (move-or-increment-idle map monster next)))))))))

(define (pursue-behavior)
  (new-behavior
   (lambda (b)
     (let ((last-player-pos #f)) ; AI state
       (lambda (monster player-pos)
	 (let* ((pos    (character-pos monster))
		(map    (floor-map (character-floor monster)))
		(target (cond ((line-of-sight? map pos player-pos)
			       (set! last-player-pos player-pos)
			       player-pos)
			      (else last-player-pos)))) ; we try to pursue
	   (let ((next (and target (find-path map pos target))))
	     (when next (move-or-increment-idle map monster next)))))))))

(define (flee-behavior)
  (new-behavior
   (lambda (b)
     (lambda (monster player-pos)
       (let ((pos (character-pos monster))
	     (map (floor-map (character-floor monster))))
	 (cond ((member player-pos (four-directions pos))
		;; we are next to the player, attack
                ;; TODO have another that flees even if next to the player, and
                ;;  one that flees when wounded
		=> (lambda (pl)
		     (move-or-increment-idle map monster player-pos)))
	       ((line-of-sight? map pos player-pos)
		;; flee
		(let* ((x     (point-x pos))
		       (y     (point-y pos))
		       (dx    (- (point-x player-pos) x))
		       (dy    (- (point-y player-pos) y))
		       (adx   (abs dx))
		       (ady   (abs dy))
		       (vert  (lambda ()
				(move-or-increment-idle
				 map monster
                                 (new-point (- x (/ dx (max adx 1))) y))))
		       (horiz (lambda ()
				(move-or-increment-idle
				 map monster
                                 (new-point x (- y (/ dy (max ady 1))))))))
		  (if (> adx ady)
		      ;; try to move vertically first
		      (when (not (vert))  (horiz))
		      ;; try to move horizontally first
		      (when (not (horiz)) (vert)))))))))))

(define (ranged-behavior)
  (new-behavior
   (lambda (b)
     (lambda (monster player-pos)
       (when (not (ranged-weapon? (equipment-main-hand
                                   (character-equipment monster))))
         (error "monster " monster " has no ranged weapon"))
       (let ((pos (character-pos monster)) ;; TODO have that in a macro
	     (map (floor-map (character-floor monster))))
         ;; TODO not very interesting for the moment
	 (if (clear-shot? map pos player-pos)
	     (ranged-attack monster (cell-occupant (grid-ref map player-pos)))
	     #f)))))) ;; stay there


(define (move-or-increment-idle map monster dest)
  (let ((pos (character-pos monster)))
    (move map monster dest)
    (when (equal? pos (character-pos monster))
      ;; we did not move, increment idle counter
      (let ((b (monster-behavior monster)))
        (set-behavior-nb-turns-idle! b (+ (behavior-nb-turns-idle b) 1))))))

;; simple pathfinding using A*
(define (find-path g a b)
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
