(define-type-of-character monster
  challenge-rating
  extender: define-monster-type)

(define-monster-type goblin) ;; TODO do they need to be subtypes ?
(define (new-goblin) ;; TODO give them different AIs
  (make-goblin "goblin" (lambda () #\g) (new-equipment) 1/3)) ;; TODO equip
(define-monster-type orc)
(define (new-orc)
  (make-orc "orc" (lambda () #\o) (new-equipment) 1/2))


(define-type encounter
  points
  monsters ; list of monsters
  can-be-placed?) ; takes a room as parameter
;; TODO have more, maybe have chests, treasure, dungeon features (campfire), maybe a name for the encounter, to display when we enter the room ?
(define (new-encounter monsters #!optional (restriction (lambda (room) #t)))
  (make-encounter (foldl + 0 (map monster-challenge-rating monsters))
		  monsters restriction))

(define encounter-types ;; TODO have weights, since some would be more common, make it so it can use random-choice
  (list (new-encounter (list (new-goblin) (new-goblin))) ;; TODO have a "language" to define encounters
	(new-encounter (list (new-goblin) (new-goblin) (new-goblin)))
	(new-encounter (list (new-orc) (new-orc)))
	(new-encounter (list (new-orc) (new-goblin) (new-goblin)))))

(define (generate-encounters level)
  (let* ((no                     (+ (level-no level) 1)) ; level-no starts at 0
	 (encounter-level-cap    no) ;; TODO maybe have it also a function of the player level ?
	 (encounter-level-bottom (max (/ no 2)
				      (foldl min
					     encounter-level-cap
					     (map encounter-points
						  encounter-types))))
	 (possible-encounters    (filter
				  (lambda (e)
				    (let ((pts (encounter-points e)))
				      (and (>= pts encounter-level-bottom)
					   (<= pts encounter-level-cap))))
				  encounter-types)))
    (let loop ((pts        (* no 10)) ;; TODO tweak
	       (free-rooms (level-rooms level)))
      (if (and (>= pts encounter-level-bottom)
	       (not (null? free-rooms)))
	  (let* ((encounter  (random-element possible-encounters))
		 (monsters   (encounter-monsters encounter))
		 (room       (random-element free-rooms))
		 (room-space (room-cells room)))
	    (if (or (room-encounter room) ; already an encounter there
		    (> (length monsters) (length room-space))) ; too small
		(loop pts free-rooms) ; try something else
		(let loop2 ((monsters monsters)
			    (space    room-space))
		  (if (not (null? monsters))
		      (let ((cell (random-element space)))
			(occupant-set! (grid-get (level-map level) cell)
				       (car monsters))
			(loop2 (cdr monsters) (remove cell space)))
		      (loop (- pts (encounter-points encounter))
			    (remove room free-rooms))))))))))
