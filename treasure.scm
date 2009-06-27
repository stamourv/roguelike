;; contains the probability of eahc kind of item, and the probability of each
;; item within each category
(define treasure-table
  `((0.45
     ;; weapons
     (0.25 . ,new-morningstar)
     (0.1  . ,new-greataxe)
     (0.4  . ,new-club)
     (0.25 . ,new-shortspear))
    (0.3
     ;; shields
     (1 . ,new-light-shield))
    (0.25
     ;; body armor
     (0.7 . ,new-leather-armor)
     (0.3 . ,new-studded-leather-armor))))
;; TODO also have gold, gems, potions, and other random items, not just equipment (find a way to sell things ?)

(define (generate-treasure floor)
  (let* ((no              (+ (floor-no floor) 1))
	 (treasure-points (* 100 no)) ; in gp TODO tweak
	 (treasure-cap    (* 20 (expt no 3)))
	 (treasure-bottom (max (* 2  (expt no 3))
			       (foldl
				min
				treasure-cap
				(map (lambda (i) (object-gp-value ((cdr i))))
				     (apply append
					    (map cdr treasure-table))))))
	 (possible-treasure
	  (map (lambda (cat)
		 ;; keep only the items that respect the conditions
		 (let* ((new-items
			 (filter (lambda (i)
				   (let ((value (object-gp-value ((cdr i)))))
				     (and (>= value treasure-bottom)
					  (<= value treasure-cap))))
				 (cdr cat)))
			(factor (foldl (lambda (acc new) (+ acc (car new)))
				       0 new-items)))
		   ;; recalculate the probabilities
		   ;; note: the probability of each category remains unchanged TODO change it ?
		   (cons (car cat)
			 (map (lambda (i) (cons (/ (car i) factor) (cdr i)))
			      new-items))))
	       treasure-table))
	 (actual-bottom (foldl min ; lowest value of the possible treasure
			       treasure-cap
			       (map (lambda (i) (object-gp-value ((cdr i))))
				    (apply append
					   (map cdr possible-treasure)))))
	 ;; the number of chests is level number independent
	 (chests (map (lambda (x) (new-chest '())) ; will be filled later
		      (iota (random-between 4 8)))))

    (if (foldl (lambda (acc new) (and acc (null? (cdr new))))
	       #t possible-treasure)
	(error "no possible treasure for this level")) ;; TODO make sure this can't happen
    
    ;; place the chests randomly on the map
    (let loop ((chests chests)
	       (spaces (filter ;; TODO copied from stairs down placement code, abstract
			(lambda (cell)
			  ;; no one there, not in a corridor, and not in front
			  ;; of a door
			  (and
			   (not (get-occupant (grid-get (floor-map floor)
							cell)))
			   (not (eq? 'corridor
				     (room-type (get-room
						 cell
						 (floor-rooms floor)))))
			   (not (next-to-a-door? (floor-map floor) cell))))
			(floor-walkable-cells floor))))
      (if (not (null? chests))
	  (let ((pos (random-element spaces)))
	    (grid-set! (floor-map floor) pos (car chests))
	    (floor-walkable-cells-set!
	     floor (remove pos (floor-walkable-cells floor)))
	    (loop (cdr chests) (remove pos spaces)))))
    
    ;; fill the chests
    (let loop ((pts treasure-points))
      (if (>= pts actual-bottom)
	  (let* ((cat   (random-choice possible-treasure))
		 (item  (if (null? cat) #f ((random-choice cat))))
		 (value (if item (object-gp-value item) #f)))
	    (if (and value (<= value pts))
		(let ((chest (random-element chests)))
		  (chest-contents-set! chest
				       (cons item (chest-contents chest)))
		  (loop (- pts value)))
		(loop pts))))))) ; try something else
