(define-type-of-character monster
  challenge-rating
  extender: define-monster-type)

(define-monster-type goblin) ;; TODO do they need to be subtypes ?
(define (new-goblin) ;; TODO give them different AIs
  (make-goblin "goblin" (lambda () #\g) #f (new-equipment) 1/3)) ;; TODO equip
(define-monster-type orc)
(define (new-orc)
  (make-orc "orc" (lambda () #\o) #f (new-equipment) 1/2))


(define-type encounter-type
  points
  monsters ; list of functions that create monsters
  can-be-placed?) ; takes a room as parameter
;; TODO have more, maybe have chests, treasure, dungeon features (campfire), maybe a name for the encounter, to display when we enter the room ?

(define-type encounter
  monsters) ;; TODO have more, espescially other kinds of objects that would need to be placed, such as chests or campfires
(define (new-encounter encounter-type) ; actually creates the monsters
  (make-encounter (map call (encounter-type-monsters encounter-type))))
(define (encounter-points e)
  (foldl + 0 (map monster-challenge-rating (encounter-monsters e))))

(define (new-encounter-type
	 monsters
	 #!optional (restriction (lambda (room)
				   (>= (length (room-cells room))
				       (length monsters)))))
  (let* ((encounter-type  (make-encounter-type
			   #f
			   monsters
			   restriction))
	 (dummy-encounter (new-encounter encounter-type)))
    (encounter-type-points-set! encounter-type
				(encounter-points dummy-encounter))
    encounter-type))

(define encounter-types ;; TODO have weights, since some would be more common, make it so it can use random-choice
  (list (new-encounter-type (list new-goblin new-goblin)) ;; TODO have a "language" to define encounters types
	(new-encounter-type (list new-goblin new-goblin new-goblin))
	(new-encounter-type (list new-orc new-orc))
	(new-encounter-type (list new-orc new-goblin new-goblin))))


(define (generate-encounters floor)
  (let* ((no                       (+ (floor-no floor) 1)) ; floor-no starts at 0
	 (encounter-level-cap      no) ;; TODO maybe have it also a function of the player level ?
	 (encounter-level-bottom   (max (/ no 2)
					(foldl min
					       encounter-level-cap
					       (map encounter-type-points
						    encounter-types))))
	 (possible-encounter-types (filter
				    (lambda (e)
				      (let ((pts (encounter-type-points e)))
					(and (>= pts encounter-level-bottom)
					     (<= pts encounter-level-cap))))
				    encounter-types)))
    (let loop ((pts            (* no 10)) ;; TODO tweak
	       (free-rooms     (floor-rooms floor))
	       (floor-monsters '()))
      (if (and (>= pts encounter-level-bottom)
	       (not (null? free-rooms)))
	  (let* ((type               (random-element possible-encounter-types))
		 (room               (random-element free-rooms)))
	    (if (and (not (room-encounter room)) ; already an encounter there
		     ((encounter-type-can-be-placed? type) room))
		(let loop2 ((monsters     (encounter-monsters
					   (new-encounter type)))
			    (all-monsters '())
			    (space        (room-cells room)))
		  (if (not (null? monsters))
		      (let ((cell (random-element space))
			    (mon  (car monsters)))
			(occupant-set! (grid-get (floor-map floor) cell) mon)
			(character-pos-set! mon cell)
			(loop2 (cdr monsters)
			       (cons mon all-monsters)
			       (remove cell space)))
		      (loop (- pts (encounter-type-points type))
			    (remove room free-rooms)
			    (append floor-monsters all-monsters))))
		(loop pts free-rooms floor-monsters))) ; try something else
	  (floor-monsters-set! floor floor-monsters)))))

;; removes a monster, usually when killed
(define (remove-monster floor monster)
  (occupant-set! (grid-get (floor-map floor) (character-pos monster)) #f)
  (floor-monsters-set! floor (remove monster (floor-monsters floor))))
