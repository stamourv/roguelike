#lang racket

(require (only-in srfi/1 iota))
(require "../utilities/utilities.rkt"
         "../utilities/floor-utils.rkt"
         "../utilities/cell.rkt"
         "../utilities/grid.rkt")
(require "../objects.rkt")
(require "../data/items.rkt")
(provide place-treasure)

;; contains the probability of each kind of item, and the probability of each
;; item within each category
;; TODO with a lot of items, this will end up being unmanageable
(define treasure-table
  `((0.43
     ;; weapons
     (0.2  . ,new-morningstar)
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
     (0.5  . ,new-light-healing-potion)
     (0.13 . ,new-bulls-strength-potion)
     (0.13 . ,new-cats-grace-potion)
     (0.13 . ,new-bears-endurance-potion)
     (0.11 . ,new-barkskin-potion))))
;; TODO maybe have these probabilities a function of the level ? something this
;;  is rare early on might become common later on
;; TODO also have gold, gems, potions, and other random items, not just
;;  equipment (find a way to sell things ?)

(define (possible-treasure no)
  ;; TODO maybe be like nethack, and have the same item possibilities,
  ;;  regardless of level ? if so, just use the DM's guide tables
  (let* ((treasure-cap    (* 10 (expt no 2)))
	 (treasure-bottom (max (* 2 (expt no 2))
			       (foldl
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
		  (factor (apply + (map car new-items))))
	     ;; recalculate the probabilities
	     ;; note: the probability of each category remains unchanged
             ;; TODO change it ?
	     (cons (car cat)
		   (map (lambda (i) (cons (/ (car i) factor) (cdr i)))
			new-items))))
	 treasure-table)))

(define (generate-treasure no)
  (let* ((treasure-points (* 30 (expt no 1.5))) ; in gp TODO tweak
	 (possible        (possible-treasure no))
	 (actual-bottom (foldl min ; lowest value of the possible treasure
                               treasure-points ; generous upper bound
                               (map (lambda (i) (object-gp-value ((cdr i))))
                                    (apply append
                                           (map cdr possible))))))
    (when (andmap (lambda (new) (null? (cdr new))) possible)
      (error "no possible treasure for this level"))
    (let loop ((pts   treasure-points)
	       (items '()))
      (if (>= pts actual-bottom)
	  (let* ((cat   (random-choice possible))
		 (item  (if (null? cat) #f ((random-choice cat))))
		 (value (if item (object-gp-value item) #f)))
	    (if (and value (<= value pts))
		(loop (- pts value) (cons item items))
		(loop pts items))) ; try something else
	  items))))

(define (place-treasure floor no)
  ;; the number of chests is level number independent
  (let ((chests (map (lambda (x) (new-chest '())) ; will be filled later
		     (iota (random-between 4 8)))))
    ;; place the chests randomly on the map
    (let loop ((chests chests))
      (when (not (null? chests))
        (let ((pos (random-free-position floor)))
          (cond [(next-to-a-door? (floor-map floor) pos)
                 (loop chests)] ; try again
                [else
                 (grid-set! (floor-map floor) pos (car chests))
                 (set-floor-walkable-cells!
                  floor (remove pos (floor-walkable-cells floor)))
                 (loop (cdr chests))]))))
    ;; fill the chests
    (for-each (lambda (item) (add-object (random-element chests) item))
	      (generate-treasure no))))

;; for debugging purposes
(define (show-treasure no) (map object-name (generate-treasure no)))
