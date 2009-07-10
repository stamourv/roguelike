;; contains the probability of eahc kind of item, and the probability of each
;; item within each category
(define treasure-table
  `((0.43
     ;; weapons
     (0.2 . ,new-morningstar) ;; TODO with a lot of items, this will end up being unmanageable
     (0.1  . ,new-greataxe)
     (0.3  . ,new-club)
     (0.15 . ,new-shortspear)
     (0.25 . ,new-shortbow))
    (0.25
     ;; shields
     (1 . ,new-light-shield))
    (0.22
     ;; body armor
     (0.7 . ,new-leather-armor)
     (0.3 . ,new-studded-leather-armor))
    (0.1
     ;; potions
     (1 . ,new-light-healing-potion))))
;; TODO maybe have these probabilities a function of the level ? something this is rare early on might become common later on
;; TODO also have gold, gems, potions, and other random items, not just equipment (find a way to sell things ?)

(define (possible-treasure no) ;; TODO maybe be like nethack, and have the same item possibilities, regardless of level ?
  (let* ((treasure-cap    (* 10 (expt no 2)))
	 (treasure-bottom (max (* 2 (expt no 2))
			       (fold
				min
				treasure-cap
				(map (lambda (i) (object-gp-value ((cdr i))))
				     (apply append
					    (map cdr treasure-table)))))))
    (map (lambda (cat)
	   ;; keep only the items that respect the conditions
	   (let* ((new-items
		   (filter (lambda (i)
			     (let ((value (object-gp-value ((cdr i)))))
			       (and (>= value treasure-bottom)
				    (<= value treasure-cap))))
			   (cdr cat)))
		  (factor (fold (lambda (acc new) (+ acc (car new)))
				0 new-items)))
	     ;; recalculate the probabilities
	     ;; note: the probability of each category remains unchanged TODO change it ?
	     (cons (car cat)
		   (map (lambda (i) (cons (/ (car i) factor) (cdr i)))
			new-items))))
	 treasure-table)))

(define (generate-treasure floor)
  (let* ((no              (+ (floor-no floor) 1))
	 (treasure-points (* 100 no)) ; in gp TODO tweak
	 (possible        (possible-treasure no))
	 (actual-bottom (fold min ; lowest value of the possible treasure
			      treasure-points ; generous upper bound
			      (map (lambda (i) (object-gp-value ((cdr i))))
				   (apply append
					  (map cdr possible)))))
	 ;; the number of chests is level number independent
	 (chests (map (lambda (x) (new-chest '())) ; will be filled later
		      (iota (random-between 4 8)))))

    (if (fold (lambda (acc new) (and acc (null? (cdr new)))) #t possible)
	(error "no possible treasure for this level"))
    
    ;; place the chests randomly on the map
    (let loop ((chests chests))
      (if (not (null? chests))
	  (let ((pos (random-free-position floor)))
	    (grid-set! (floor-map floor) pos (car chests))
	    (floor-walkable-cells-set!
	     floor (remove pos (floor-walkable-cells floor)))
	    (loop (cdr chests)))))
    
    ;; fill the chests
    (let loop ((pts treasure-points))
      (if (>= pts actual-bottom)
	  (let* ((cat   (random-choice possible))
		 (item  (if (null? cat) #f ((random-choice cat))))
		 (value (if item (object-gp-value item) #f)))
	    (if (and value (<= value pts))
		(let ((chest (random-element chests)))
		  (add-object chest item)
		  (loop (- pts value)))
		(loop pts))))))) ; try something else
