(define-class monster (character)
  (slot: challenge-rating)
  ;; function that takes the monster, the floor, and the position of the
  ;; player as parameters and makes the monster act
  (slot: behavior)) ;; TODO have different speeds (maybe even initiative?) to determine which monster moves first

;; to handle the repetitive part of generating the hp ;; TODO could be done with a constructor ?
(define-macro (new-monster f name pos
			   str dex con int wis cha
			   hit-dice max-hp hp
			   base-attack-bonus
			   equipment
			   challenge-rating behavior)
  (let ((m (gensym)))
    `(let ((,m (,f ,name ,pos
		   ,str ,dex ,con ,int ,wis ,cha
		   ,hit-dice ,max-hp ,hp
		   ,base-attack-bonus
		   ,equipment
		   ,challenge-rating ,behavior)))
       (init-hp ,m)
       ,m)))

(define-class goblin (monster))
(define (new-goblin)
  (new-monster make-goblin
	       "goblin" #f ;; TODO add ranged versions too
	       11 13 12 10 9 6
	       '(8) #f #f 1
	       (new-equipment
		main-arm: (new-club)
		off-arm:  (new-light-shield)
		torso:    (new-leather-armor))
	       1/3 (rush-behavior)))
(define-method (print (m goblin)) #\g)

(define-class kobold (monster))
(define (new-kobold)
  (new-monster make-kobold
	       "kobold" #f
	       9 13 10 10 9 8
	       '(8) #f #f 1
	       (new-equipment
		main-arm: (new-shortspear)
		torso:    (new-leather-armor))
	       1/4 (rush-behavior)))
(define-method (print (m kobold)) #\k)

(define-class orc (monster))
(define (new-orc)
  (new-monster make-orc
	       "orc" #f
	       17 11 12 8 7 6
	       '(8) #f #f 1
	       (new-equipment
		main-arm: (new-greataxe)
		torso:    (new-studded-leather-armor))
	       1/2 (pursue-behavior)))
(define-method (print (m orc)) #\o)


(define-class animal (monster))

(define-class bat (animal))
(define (new-bat) ;; TODO these monsters are kind of pointless since they can barely do damage
  (new-monster make-bat
	       "bat" #f
	       1 15 10 2 14 4
	       '(2) #f #f 0
	       (new-equipment) ;; TODO will attack with the 1d4 unarmed strike, is that too much ? (actually, with the strength penalty, it's ridiculous)
	       1/10 (rush-behavior)))
(define-method (print (m bat)) #\b)

(define-class rat (animal))
(define (new-rat)
  (new-monster make-rat
	       "rat" #f
	       2 15 10 2 12 2
	       '(2) #f #f 0
	       (new-equipment) ;; TODO also has unarmed strike, should have a way to represent natural attacks, damage is ridiculous, once again
	       1/8 (rush-behavior)))
(define-method (print (m rat)) #\r)


(define-class undead (monster)) ;; TODO add some


(define-type encounter-type
  points
  monsters ; list of functions that create monsters
  can-be-placed?) ; takes a room as parameter
;; TODO have more, maybe have chests, treasure, dungeon features (campfire), maybe a name for the encounter, to display when we enter the room ?

(define-type encounter
  monsters) ;; TODO have more, espescially other kinds of objects that would need to be placed, such as chests or campfires
;; TODO also have some treasure with the encounter, see DM guide for proportions and amount by encounter level
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
  (map new-encounter-type ;; TODO have a "language" to define encounters types, maybe make the probability a function of the level-no ?
       `((,new-bat ,new-bat ,new-bat)
	 (,new-rat ,new-rat)
	 (,new-rat ,new-rat ,new-rat)
	 (,new-rat ,new-rat ,new-rat ,new-rat)
	 (,new-kobold ,new-kobold)
	 (,new-kobold ,new-kobold ,new-kobold ,new-kobold)
	 (,new-goblin ,new-goblin)
	 (,new-goblin ,new-goblin ,new-goblin)
	 (,new-orc ,new-orc)
	 (,new-orc ,new-goblin ,new-goblin))))


(define (generate-encounters floor)
  (let* ((no                       (+ (floor-no floor) 1)) ; floor-no starts at 0
	 (encounter-level-cap      (/ no 2)) ;; TODO maybe have it also a function of the player level ?
	 (encounter-level-bottom   (max (/ no 4) ;; TODO TWEAK
					(foldl min
					       encounter-level-cap
					       (map encounter-type-points
						    encounter-types))))
	 (possible-encounter-types (filter
				    (lambda (e)
				      (let ((pts (encounter-type-points e)))
					(and (>= pts encounter-level-bottom)
					     (<= pts encounter-level-cap))))
				    encounter-types))
	 (actual-bottom            (foldl min
					  encounter-level-cap
					  (map encounter-type-points
					       possible-encounter-types))))
    (if (null? possible-encounter-types)
	(error "no possible encounters for this level")) ;; TODO make sure this can't happen, currently does for level 3. maybe just don't generate anything instead ?
    (let loop ((pts            (* no 5)) ;; TODO tweak
	       (free-rooms     (floor-rooms floor))
	       (floor-monsters '()))
      (if (and (>= pts actual-bottom)
	       (not (null? free-rooms)))
	  (let* ((type             (random-element possible-encounter-types))
		 (encounter-points (encounter-type-points type))
		 (room             (random-element free-rooms)))
	    (if (and (<= encounter-points pts)
		     (not (room-encounter room)) ; already an encounter there
		     ((encounter-type-can-be-placed? type) room))
		(let loop2 ((monsters     (encounter-monsters
					   (new-encounter type)))
			    (all-monsters '())
			    (space        (room-cells room)))
		  (if (not (null? monsters))
		      (let ((cell (random-element space))
			    (mon  (car monsters)))
			(cell-occupant-set! (grid-get (floor-map floor) cell)
					    mon)
			(character-pos-set! mon cell)
			(loop2 (cdr monsters)
			       (cons mon all-monsters)
			       (remove cell space)))
		      (loop (- pts encounter-points)
			    (remove room free-rooms)
			    (append floor-monsters all-monsters))))
		(loop pts free-rooms floor-monsters))) ; try something else
	  (floor-monsters-set! floor floor-monsters)))))

;; removes a monster, usually when killed
(define (remove-monster monster)
  (let* ((floor (player-floor player))
	 (cell  (grid-get (floor-map floor) (character-pos monster))))
    ;; drop equipment TODO maybe only drop each part with a certain probability, to simulate breaking during combat
    (for-each-equipped (lambda (obj where) (if obj (add-object cell obj)))
		       (character-equipment monster))
    ;; remove the monster
    (cell-occupant-set! cell #f)
    (floor-monsters-set! floor (remove monster (floor-monsters floor)))
    ;; give experience
    (let* ((challenge     (monster-challenge-rating monster))
	   (xp-same-level (* challenge 300))
	   (delta-level   (- challenge (player-level player))))
      (add-experience (if (= delta-level 0)
			  xp-same-level
			  (max 0
			       (ceiling (* xp-same-level
					   (+ 1 (* 1/3 delta-level)))))))))) ;; TODO tweak
