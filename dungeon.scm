;; TODO change the name of the file for level.scm

(define-type floor ;; TODO also have a dungeon type ?
  no
  map
  rooms ;; TODO have a set ?
  stairs-up
  stairs-down
  walkable-cells ;; TODO remove from the list if we add impassable features, also remove from the cell lists in room objects
  monsters)

(define-type room
  type
  cells ; TODO the 3 of these are sets, use hash tables for sets if it becomes slow
  walls
  connected-to
  encounter)
(define (get-room point rooms)
  (find (lambda (room) (member point (room-cells room))) rooms))
(define (connected? a b)
  ;; since it's commutative, no need to check both sides
  ;; TODO need to check for a and b, since this sometimes receives #f, probably due to a bug somewhere, investigate
  (and a b (memq a (room-connected-to b))))
(define (connect! a b)
  (if (and a b) ;; TODO same here
      (begin (room-connected-to-set! a (cons b (room-connected-to a)))
	     (room-connected-to-set! b (cons a (room-connected-to b))))))


(define (generate-floor no #!optional (stairs-down? #t) (step? #f))
  ;; for now, levels are grids of 20 rows and 60 columns, to fit in a 80x25
  ;; terminal
  (let* ((level-height 18)
	 (level-width  60)
	 (level        (empty-grid level-height level-width
				   cell-fun: (lambda (pos) (new-solid-wall))))
	 (new-floor (make-floor no level '() #f #f '() #f)))
        
    (define (add-rectangle pos height width direction)
      ;; height and width consider a wall of one cell wide on each side
      (let* ((x     (point-x pos))
	     (y     (point-y pos))
	     (pos-x (case direction
		      ((south) x)
		      ;; expanding north, we have to move the top of the room
		      ;; up so the bottom reaches the starting point
		      ((north) (+ (- x height) 1))
		      ;; expanding east or west, position ourselves so the
		      ;; middle of the wall of the new room starts here
		      (else    (- x (floor (/ height 2))))))
	     (pos-y (case direction
		      ;; same idea as for x
		      ((east) y)
		      ((west) (+ (- y width) 1))
		      (else   (- y (floor (/ width 2)))))))
	(if (foldl ; can this new feature fit ?
	     (lambda (acc x)
	       (and acc
		    (foldl
		     (lambda (acc y)
		       (let ((p (new-point (+ x pos-x) (+ y pos-y))))
			 (and acc
			      (inside-grid? level p)
			      (wall? (grid-ref level p)))))
		     #t (iota width))))
	     #t (iota height))
	    
	    (let ((new-walls '()) ; yes it can, add it
		  (inside    '()))
	      (grid-for-each
	       (lambda (p)
		 (let ((x (- (point-x p) pos-x)) (y (- (point-y p) pos-y)))
		   (grid-set!
		    level p
		    ;; find out the appropriate cell type
		    ((cond ((or (corner-wall? (grid-ref level p))
				(and (or (= x 0) (= x (- height 1)))
				     (or (= y 0) (= y (- width 1)))))
			    ;; one of the four corners
			    new-corner-wall)
			   ((or (= x 0) (= x (- height 1)))
			    ;; horizontal-wall
			    (set! new-walls (cons p new-walls))
			    new-horizontal-wall)
			   ((or (= y 0) (= y (- width 1)))
			    ;; vertical wall
			    (set! new-walls (cons p new-walls))
			    new-vertical-wall)
			   ;; inside of the room
			   (else (set! inside (cons p inside))
				 new-empty-cell))))))
	       level
	       start-x:  pos-x  start-y:  pos-y
	       length-x: height length-y: width)

	      (if step?
		  (begin (show-grid level)
			 (read-line)))

	      ;; the type will be filled later
	      (make-room #f inside new-walls '() #f))
	    
	    #f))) ; no it can't, give up
    
    (define (add-small-room pos direction)
      ;; both dimensions 5-7 units (including walls)
      (add-rectangle pos (random-between 5 7) (random-between 5 7) direction))
    (define (add-large-room pos direction)
      ;; both dimensions 8-12 units (including walls)
      (let* ((height (random-between 8 12)) ;; TODO maybe have even bigger rooms, or longer ones, that can have rows of columns, maybe just a hallway type of room, with width 6 (or maybe 7 possible too) with 2 rows of columns, handle like a corridor
	     (width  (random-between 8 12))
	     (room   (add-rectangle pos height width direction)))
	;; large rooms may have pillars
	(if (and room (< (random-real) 0.6))
	    (let* ((cells       (room-cells room))
		   (top-left-x   (foldl min level-height (map point-x cells)))
		   (top-left-y   (foldl min level-width  (map point-y cells)))
		   (n-pillars-x (if (< height 10) 1 2))
		   (n-pillars-y (if (< width  10) 1 2))
		   (pts-x       (cond ((= n-pillars-x 1)
				       ;; in the middle
				       (list (+ top-left-x -1
						(quotient height 2))))
				      ((= n-pillars-x 2)
				       ;; 2 rows, at 2 cells from each wall
				       (list (+ top-left-x 2)
					     (- (+ top-left-x height) 5)))))
		   (pts-y       (cond ((= n-pillars-y 1)
				       (list (+ top-left-y -1
						(quotient width 2))))
				      ((= n-pillars-y 2)
				       (list (+ top-left-y 2)
					     (- (+ top-left-y width) 5))))))
	      (for-each
	       (lambda (pos)
		 (grid-set! level pos (new-pillar))
		 (room-cells-set! room (remove pos (room-cells room))))
	       (cartesian-product pts-x pts-y))))
	room))
    (define (add-corridor   pos direction)
      ;; width: 3, length: 5-17 (including walls)
      ;; TODO maybe wider corridors ?
      (if (or (eq? direction 'east) (eq? direction 'west))
	  ;; we generate an horizontal corridor
	  (add-rectangle pos 3 (random-between 5 17) direction)
	  (add-rectangle pos (random-between 5 17) 3 direction)))

    ;; replaces a wall (horizontal or vertical) by a door, adds the doorposts
    ;; and connects the two rooms in the graph
    ;; if we have it (and espescially if it cannot be inferred (we are placing
    ;; a door on a free space, for example)), the direction of the wall can be
    ;; given
    (define (add-door cell #!optional direction)
      ;; add the doorposts
      (for-each (lambda (post) (grid-set! level post (new-corner-wall)))
		(wall-parrallel level cell))
      ;; connect the two rooms
      (let* ((sides (wall-perpendicular level cell))
	     (dirs  (if (null? sides)
			((case direction
			   ((horizontal) up-down)
			   ((vertical)   left-right)) cell)
			sides))
	     (rooms (floor-rooms new-floor))
	     (a     (get-room (car  dirs) rooms))
	     (b     (get-room (cadr dirs) rooms)))
	(if (and a b)
	    (begin (grid-set! level cell (new-door)) ; put the door
		   (connect!  a b))
	    ;; when we place the first room, there is nothing to connect to,
	    ;; and we place the stairs if they were not placed already
	    (if (not (floor-stairs-up new-floor))
		(begin (grid-set! level cell (new-stairs-up))
		       (floor-stairs-up-set! new-floor cell))))))

    (define (add-random-feature start)
      ;; find in which direction to expand from this wall
      (let* ((pos  (car start))
	     (prev (cdr start)) ; type of the room we expand from
	     (direction
	      (let loop ((around     (four-directions pos))
			 (directions '(south north east west)))
		(cond ((null? around) ; walls all around
		       (random-element '(north south west east)))
		      ((and (inside-grid? level (car around))
			    (walkable-cell? (grid-ref level (car around))))
		       ;; there is a free space in that direction, we must
		       ;; expand the opposite way
		       (car directions))
		      (else ;; keep looking
		       (loop (cdr around) (cdr directions))))))
	     (type (random-choice
		    (case prev
		      ((corridor)   `((0.1  corridor   ,add-corridor)
				      (0.6  large-room ,add-large-room)
				      (0.3  small-room ,add-small-room)))
		      ((large-room) `((0.6  corridor   ,add-corridor)
				      (0.2  large-room ,add-large-room)
				      (0.2  small-room ,add-small-room)))
		      ((small-room) `((0.7  corridor   ,add-corridor)
				      (0.2  large-room ,add-large-room)
				      (0.1  small-room ,add-small-room)))
		      ;; should end up here only in the first turn
		      (else         `((0.4  corridor   ,add-corridor)
				      (0.5  large-room ,add-large-room)
				      (0.1  small-room ,add-small-room))))))
	     ;; the higher it is, the more chance this room will be chosen
	     ;; as a starting point for another
	     (weight (case (car type)
		       ((corridor)   5)
		       ((large-room) 2)
		       ((small-room) 3)))
	     ;; returns #f or a room structure
	     (res    ((cadr type) pos direction)))
	(if res
	    (begin
	      ;; add the new room the list of rooms
	      (room-type-set! res (car type))
	      (floor-rooms-set! new-floor (cons res (floor-rooms new-floor)))
	      (add-door pos)
	      ;; return the walls of the room "weight" times, and attach the
	      ;; type of the new room to influence room type probabilities
	      (map (lambda (x) (cons x (car type)))
		   (repeat weight (room-walls res))))
	    #f)))

    ;; generate features
    (let loop ((n 500)
	       (walls (let loop ((res #f)) ; we place the first feature
			(if res
			    res
			    (loop (add-random-feature
				   (cons (random-position level) #f)))))))
      ;; although unlikely, we might run out of walls (happened once, no
      ;; idea how)
      (if (or (> n 0) (null? walls))
	  (let* ((i     (random-integer (length walls)))
		 (start (list-ref walls i)))
	    (loop (- n 1)
		  (cond ((add-random-feature start)
			 => (lambda (more)
			      (append (remove-at-index walls i) more)))
			(else walls))))))

    ;; add doors to anything that looks like a doorway
    (grid-for-each
     (lambda (pos)
       (let* ((around    (four-directions pos))
	      (up        (list-ref around 0))
	      (down      (list-ref around 1))
	      (left      (list-ref around 2))
	      (right     (list-ref around 3))
	      (direction #f))
	 ;; we must either have wall up and down, and free space left and
	 ;; right, or the other way around
	 (if (and (not (door?      (grid-ref level pos))) ; not already a door
		  (not (stairs-up? (grid-ref level pos)))
		  (foldl (lambda (acc cell)
			   (and acc (inside-grid? level cell)))
			 #t around)
		  (let ((c-up    (grid-ref level up))
			(c-down  (grid-ref level down))
			(c-left  (grid-ref level left))
			(c-right (grid-ref level right)))
		    (define (connection-check a b)
		      (let* ((rooms (floor-rooms new-floor))
			     (a     (get-room a rooms))
			     (b     (get-room b rooms)))
			(not (connected? a b))))
		    (or (and (corner-wall?      c-up)
			     (corner-wall?      c-down)
			     (walkable-cell?    c-left)
			     (walkable-cell?    c-right)
			     ;; must not be connected already
			     (connection-check  left right)
			     (begin (set! direction 'vertical)
				    #t))
			(and (corner-wall?      c-left)
			     (corner-wall?      c-right)
			     (walkable-cell?    c-up)
			     (walkable-cell?    c-down)
			     (connection-check  up down)
			     (begin (set! direction 'horizontal)
				    #t)))))
	     ;; yes, we have found a valid doorway, if this doorway is in an
	     ;; existing room, we would separate in into two smaller ones,
	     ;; which is no fun, so only put a door if we would open a wall
	     (let ((room (get-room pos (floor-rooms new-floor))))
	       (if (not room)
		   (add-door pos direction))))))
     level)

    ;; to avoid dead-end corridors, any corridor connected to a single room
    ;; tries to open a door to another room
    (for-each
     (lambda (room)
       (let ((neighbors (room-connected-to room)))
	 (if (and (eq? (room-type room) 'corridor)
		  (= (length neighbors) 1))
	     (let* ((walls        (room-walls room))
		    (current-door (find (lambda (pos)
					  (door? (grid-ref level pos)))
					walls))
		    (door-candidate
		     (foldl
		      ;; we want the candidate farthest from the existing door
		      ;; if there are no suitable candidates, we just choose
		      ;; the existing door
		      (lambda (best new)
			(let* ((best-dist (distance best current-door))
			       (new-dist  (distance new  current-door))
			       (max-dist  (max best-dist new-dist)))
			  (if (= max-dist best-dist)
			      best
			      new)))
		      current-door
		      (filter
		       (lambda (wall)
			 ;; to open a door, both sides must be clear
			 (let ((sides (wall-perpendicular level wall)))
			   (and (not (null? sides))
				(foldl
				 (lambda (acc new)
				   (and acc
					(inside-grid? level new)
					(walkable-cell? (grid-ref level new))))
				 #t sides))))
		       walls))))
	       (if (not (eq? door-candidate current-door))
		   (add-door door-candidate))))))
     (floor-rooms new-floor))

    (floor-walkable-cells-set!
     new-floor
     (filter (lambda (pos)
	       (walkable-cell? (grid-ref (floor-map new-floor) pos)))
	     (apply append
		    (map room-cells (floor-rooms new-floor)))))
    
    ;; if needed, add the stairs down on a random free square in a room
    ;; (not a corridor)
    ;; TODO try to place it as far as possible from the stairs up, see building quantifiably fun maps, or something like that on the wiki
    (if stairs-down?
	(let* ((rooms (floor-rooms new-floor))
	       (pos   (random-element
		       (filter
			(lambda (cell)
			  ;; not in a corridor, and not in front of a door
			  (and
			   (not (eq? 'corridor
				     (room-type (get-room cell rooms))))
			   (not (next-to-a-door? (floor-map new-floor) cell))))
			       (floor-walkable-cells new-floor)))))
	  (grid-set! level pos (new-stairs-down))
	  (floor-stairs-down-set! new-floor pos)
	  (floor-walkable-cells-set!
	   new-floor
	   (remove pos (floor-walkable-cells new-floor)))))

    ;; add everything else on top
    (generate-encounters new-floor)
    (generate-treasure   new-floor)
    new-floor))
