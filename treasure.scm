(import cell)
(import grid)
(import dungeon)
(import objects)
(import utilities)

;; contains the probability of eahc kind of item, and the probability of each
;; item within each category
(define treasure-table
  (list (list 0.43 ;; FOO had something pretty with quasiquote and unquote, but black hole considered the unquoted parts to be syntaxtic closures, which made no sense
	      ;; weapons
	      (cons 0.2  new-morningstar) ;; TODO with a lot of items, this will end up being unmanageable
	      (cons 0.1  new-greataxe)
	      (cons 0.3  new-club)
	      (cons 0.15 new-shortspear)
	      (cons 0.25 new-shortbow))
	(list 0.25
	      ;; shields
	      (cons 1 new-light-shield))
	(list 0.22
	      ;; body armor
	      (cons 0.7 new-leather-armor)
	      (cons 0.3 new-studded-leather-armor))
	(list 0.1
	      ;; potions
	      (cons 0.5  new-light-healing-potion)
	      (cons 0.13 new-bulls-strength-potion)
	      (cons 0.13 new-cats-grace-potion)
	      (cons 0.13 new-bears-endurance-potion)
	      (cons 0.11 new-barkskin-potion))))
;; TODO maybe have these probabilities a function of the level ? something this is rare early on might become common later on
;; TODO also have gold, gems, potions, and other random items, not just equipment (find a way to sell things ?)

(define (possible-treasure no) ;; TODO maybe be like nethack, and have the same item possibilities, regardless of level ? if so, just use the DM's guide tables
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
