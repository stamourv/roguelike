(define (generate-level #!key (trace? #f) (step? #f))
  ;; TODO have a limit linked to the size of the screen, or scroll ? if scrolling, query the terminal size
  ;; for now, levels are grids of 20 rows and 60 columns, to fit in a 80x25
  ;; terminal
  (let* ((level-height 20)
	 (level-width  60)
	 (level (empty-grid level-height level-width
			    cell-fun: (lambda (pos) (new-solid-wall)))))
    
    (define trace 0) ;; to trace the generation of the dungeon
    (define (trace-cell)
      (let ((x (number->string (modulo trace 10))))
	(make-walkable-cell (lambda () x) #f #f)))

    (define-type room
      cells ; TODO both of these are sets, use hash tables for sets if it becomes slow
      connected-to)
    (define rooms '()) ;; TODO another set, see above
    (define (get-room point)
      (find (lambda (room) (member point (room-cells room))) rooms))
    (define (connected? a b)
      ;; since it's commutative, no need to check both sides
      ;; TODO need to check for a and b, since this sometimes receives #f, probably due to a bug somewhere, investigate
      (and a b (memq a (room-connected-to b))))
    (define (connect! a b)
      (room-connected-to-set! a (cons b (room-connected-to a)))
      (room-connected-to-set! b (cons a (room-connected-to b)))
      #t) ; must return true, since it's used in an and below
    
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
			      (wall? (grid-get level p)))))
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
		    ((cond ((or (corner-wall? (grid-get level p))
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
				 (if trace?
				     trace-cell
				     new-walkable-cell)))))))
	       level
	       start-x:  pos-x  start-y:  pos-y
	       length-x: height length-y: width)

	      (if trace?
		  (begin (pp (list trace pos: (point-x pos) (point-y pos)
				   dir: direction height: height width: width))
			 (set! trace (+ trace 1))))
	      (if step?
		  (begin (show-grid level)
			 (read-line)))
	      
	      ;; add the new room the list of rooms
	      (set! rooms (cons (make-room inside '()) rooms))
	      ;; put a door between the two rooms (the door is at pos)
	      (for-each (lambda (pos) (grid-set! level pos (new-corner-wall)))
			(if (memq direction '(east west))
			    (up-down    pos)
			    (left-right pos)))
	      (let* ((sides (if (memq direction '(east west))
				(left-right pos)
				(up-down    pos)))
		     (a     (get-room (car sides)))
		     (b     (get-room (cadr sides))))
		(if (and a b)
		    (begin (grid-set! level pos (new-door)) ; put the door
			   (connect!  a b))
		    ;; when we place the first room, there is nothing to
		    ;; connect to, and we place the stairs
		    (grid-set! level pos (new-stairs-up))))

	      new-walls)
	    
	    #f))) ; no it can't, give up
    
    (define (add-small-room pos direction)
      ;; both dimensions 5-7 units (including walls)
      (add-rectangle pos (random-between 5 7) (random-between 5 7) direction))
    (define (add-large-room pos direction)
      ;; both dimensions 8-12 units (including walls)
      (add-rectangle pos (random-between 8 12) (random-between 8 12) direction))
    (define (add-corridor   pos direction)
      ;; width: 3, length: 5-17 (including walls)
      ;; TODO maybe wider corridors ?
      (if (or (eq? direction 'east) (eq? direction 'west))
	  ;; we generate an horizontal corridor
	  (add-rectangle pos 3 (random-between 5 17) direction)
	  (add-rectangle pos (random-between 5 17) 3 direction)))

    (define (add-random-feature pos)
      ;; find in which direction to expand from this wall
      (let ((direction
	     (let loop ((around     (four-directions pos))
			(directions '(south north east west)))
	       (cond ((null? around) ; walls all around
		      (random-element '(north south west east)))
		     ((and (inside-grid? level (car around))
			   (walkable-cell? (grid-get level (car around))))
		      ;; there is a free space in that direction, we must
		      ;; expand the opposite way
		      (car directions))
		     (else ;; keep looking
		      (loop (cdr around) (cdr directions))))))
	    (r (random-real)))
	;; 30% chance of corridor, 40% of large room, 30% of small room
	((cond ((< r 0.3) add-corridor)
	       ((< r 0.7) add-large-room)
	       (else      add-small-room))
	 pos direction)))
    ;; TODO problem : the presence of + on a straight wall reveals the structure on the other side of the wall, maybe use # for all wall, but would be ugly

    (let loop ((n 1000)
	       (walls (let loop ((res #f) ; we place the first feature
				 (pos #f))
			(if res
			    res
			    (let ((r (random-position level)))
			      (loop (add-random-feature r)
				    r))))))
      ;; although unlikely, we might run out of walls (happened once, no
      ;; idea how)
      (if (or (> n 0) (null? walls))
	  (let* ((i     (random-integer (length walls)))
		 (start (list-ref walls i)))
	    (loop (- n 1)
		  (cond ((add-random-feature start) ;; TODO maybe instead of just being a list of positions, also have the type of room they are part of, so we can alter the probabilities of generating the same type of room from there (lower it, most likely)
			 => (lambda (more)
			      (append (remove-at-index walls i) more)))
			(else walls))))))

    ;; add doors to anything that looks like a doorway
    ;; TODO maybe only do it with probability p
    (grid-for-each
     (lambda (pos)
       (let* ((around (four-directions pos))
	      (up     (list-ref around 0))
	      (down   (list-ref around 1))
	      (left   (list-ref around 2))
	      (right  (list-ref around 3)))
	 ;; we must either have wall up and down, and free space left and
	 ;; right, or the other way around
	 (if (and (not (door?      (grid-get level pos))) ; not already a door
		  (not (stairs-up? (grid-get level pos)))
		  (foldl (lambda (acc cell)
			   (and acc (inside-grid? level cell)))
			 #t around)
		  (let ((c-up    (grid-get level up))
			(c-down  (grid-get level down))
			(c-left  (grid-get level left))
			(c-right (grid-get level right)))
		    (define (check-and-connect a b)
		      (let ((a (get-room a))
			    (b (get-room b)))
			(if (not (connected? a b))
			    ;; connect them
			    (begin (connect! a b) #t)
			    #f)))
		    (or (and (corner-wall? c-up)
			     (corner-wall? c-down)
			     (walkable-cell?    c-left)
			     (walkable-cell?    c-right)
			     ;; must not be connected already
			     (check-and-connect left right))
			(and (corner-wall? c-left)
			     (corner-wall? c-right)
			     (walkable-cell?    c-up)
			     (walkable-cell?    c-down)
			     (check-and-connect up down)))))
	     ;; yes, we have found a valid doorway
	     (grid-set! level pos (new-door)))))
     level)
    
    level))
