#lang racket

(require (only-in srfi/1 iota lset-difference)
         (rename-in racket/base [floor math-floor]))
(require "../utilities/list.rkt"
         "../utilities/random.rkt"
         "../utilities/floor-utils.rkt"
         "../utilities/cell.rkt"
         "../utilities/grid.rkt"
         (rename-in "../utilities/grid.rkt"
                    [up up-from]     [down down-from]
                    [left left-from] [right right-from]))
(provide generate-dungeon-floor)

;; TODO take internal definitions out and have parameters + parameterize to
;;  keep floor-local state?
(define (generate-dungeon-floor (stairs-up-pos #f) (place-stairs-down? #t))
  ;; for now, levels are grids of 20 rows and 60 columns, to fit in a 80x25
  ;; terminal
  (let* ((level-height 18)
	 (level-width  60)
	 (level        (empty-grid level-height level-width
				   #:cell-fun (lambda (pos) (new-void-cell))))
	 (new-floor (make-floor level '() #f #f '() #f)))
        
    (define (add-rectangle pos height width direction room-type)
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
		      (else    (- x (math-floor (/ height 2))))))
	     (pos-y (case direction
		      ;; same idea as for x
		      ((east) y)
		      ((west) (+ (- y width) 1))
		      (else   (- y (math-floor (/ width 2)))))))
	(if (andmap ; can this new feature fit ?
	     (lambda (x)
	       (andmap (lambda (y)
                         (let* ((p (new-point (+ x pos-x) (+ y pos-y)))
                                (c (grid-ref-check level p)))
                           (or (void-cell? c) ; unused yet
                               (wall?      c)))) ; neghboring room
                       (iota width)))
	     (iota height))
	    
	    (let ((new-walls '()) ; yes it can, add it
		  (inside    '()))
	      (grid-for-each
	       (lambda (p)
		 (let ((x (- (point-x p) pos-x)) (y (- (point-y p) pos-y)))
		   (grid-set!
		    level p
		    ;; find out the appropriate cell type
		    ((cond [(or (= x 0) (= x (- height 1))
                                (= y 0) (= y (- width 1)))
			    ;; wall
			    (set! new-walls (cons p new-walls))
			    new-four-corner-wall]
			   ;; inside of the room
			   [else (set! inside (cons p inside))
				 new-empty-cell])))))
	       level
	       #:start-x  pos-x  #:start-y  pos-y
	       #:length-x height #:length-y width)

	      (make-room room-type inside new-walls '() '() #f))
	    
	    #f))) ; no it can't, give up
    
    (define (add-small-room pos direction)
      ;; both dimensions 5-7 units (including walls)
      (add-rectangle pos (random-between 5 7) (random-between 5 7) direction
                     'small-room))
    (define (add-large-room pos direction)
      ;; both dimensions 8-12 units (including walls)
      (let* ((height (random-between 8 12))
	     (width  (random-between 8 12))
	     (room   (add-rectangle pos height width direction 'large-room)))
        ;; TODO maybe have even bigger rooms, or longer ones, that can have
        ;;  rows of columns, maybe just a hallway type of room, with width 6
        ;;  (or maybe 7 possible too) with 2 rows of columns, handle like a
        ;;  corridor
	;; large rooms may have pillars
	(when (and room (< (random) 0.6))
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
               (set-room-cells! room (remove pos (room-cells room))))
             (cartesian-product pts-x pts-y))))
	room))
    (define (add-corridor pos direction)
      ;; width: 3, length: 5-17 (including walls)
      ;; TODO maybe wider corridors ?
      (let* ([len    (random-between 5 17)]
             [horiz? (memq direction '(east west))]
             [h      (if horiz? 3 len)]
             [w      (if horiz? len 3)]
             [room (add-rectangle pos h w direction 'corridor)])
        (when room ; if we successfully place the room, post-processing
          ;; the end of the corridor is considered a preferred expansion point
          (set-room-preferred-expansion-points!
           room
           (case direction
             [(north)
              (let ([end (point-add pos (new-point (- (sub1 len)) 0))])
                (list end (down-left end) (down-right end)))]
             [(south)
              (let ([end (point-add pos (new-point (sub1 len) 0))])
                (list end (up-left end) (up-right end)))]
             [(east)
              (let ([end (point-add pos (new-point 0 (sub1 len)))])
                (list end (up-left end) (down-left end)))]
             [(west)
              (let ([end (point-add pos (new-point 0 (- (sub1 len))))])
                (list end (up-right end) (down-right end)))])))
        room))

    ;; replaces a wall (horizontal or vertical) by a door, adds the doorposts
    ;; and connects the two rooms in the graph
    ;; if we have it (and espescially if it cannot be inferred (we are placing
    ;; a door on a free space, for example)), the direction of the wall can be
    ;; given
    (define (add-door cell (direction #f))
      (unless (inside-grid? level cell)
        (error "adding door outside of level"))
      ;; connect the two rooms
      (let* ((sides (wall-perpendicular level cell))
	     (dirs  (if (null? sides)
			((case direction
			   ((horizontal) up-down)
			   ((vertical)   left-right)
                           (else (error "trying to add invalid door")))
                         cell)
			sides))
	     (rooms (floor-rooms new-floor))
	     (a     (get-room (car  dirs) rooms))
	     (b     (get-room (cadr dirs) rooms)))
	(if (and a b)
            (if (and (empty-cell? (grid-ref-check level (car  dirs)))
                     (empty-cell? (grid-ref-check level (cadr dirs))))
                ;; clear on both sides, we can add the door
                (begin (grid-set! level cell (new-door))
                       (connect!  a b))
                (error "trying to add an obstructed door"))
	    ;; when we place the first room, there is nothing to connect to,
	    ;; and we place the stairs if they were not placed already
	    (when (not (floor-stairs-up new-floor))
              (grid-set! level cell (new-stairs-up))
              (set-floor-stairs-up! new-floor cell)))))

    (define (add-random-feature start)
      ;; find in which direction to expand from this wall
      (let* ((pos  (car start))
	     (prev (cdr start)) ; type of the room we expand from
	     (direction
	      (let loop ((around     (four-directions pos))
			 (directions '(south north east west)))
		(cond ((null? around) ; walls all around
		       (random-element '(north south west east)))
		      ((walkable-cell? (grid-ref-check level (car around)))
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
	      (set-floor-rooms! new-floor (cons res (floor-rooms new-floor)))
	      (add-door pos)
	      ;; return the walls of the room "weight" times, and attach the
	      ;; type of the new room to influence room type probabilities
              ;; in addition, if the room has preferred expansion points
              ;; (e.g. corridor ends), add them with a heavy weight
              (let ([preferred (room-preferred-expansion-points res)])
                (map (lambda (x) (cons x (car type)))
                     (append (repeat weight (room-walls res))
                             (repeat 5 preferred)
                             ;; the very end of a corridor is even better
                             (if (not (null? preferred))
                                 (repeat 5 (list (car preferred)))
                                 '())))))
	    #f)))

    
    ;; generate features
    (let loop ((n 1000)
	       (walls (let loop ((res #f)) ; we place the first feature
			(if res
			    res
			    (loop (add-random-feature
                                   ;; if this is not the first floor, the
                                   ;; stairs up should be at the same
                                   ;; coordinates as the previous floor's
                                   ;; stairs down
				   (cons (or stairs-up-pos
					     (random-position level))
					 #f)))))))
      ;; although unlikely, we might run out of walls (happened once, no
      ;; idea how)
      (when (and (> n 0) (not (null? walls)))
        (let* ((i     (random (length walls)))
               (start (list-ref walls i)))
          (loop (- n 1)
                (cond ((add-random-feature start)
                       => (lambda (more)
                            (append (remove-at-index walls i) more)))
                      (else walls))))))

    ;; add walls around the stairs up
    (let ([stairs (floor-stairs-up new-floor)])
      (for-each (lambda (p)
                  (when (void-cell? (grid-ref-check level p))
                    (grid-set! level p (new-four-corner-wall))))
                (eight-directions stairs)))

    ;; to find where to add new doors
    (smooth-walls level)

    ;; add doors to anything that looks like a doorway
    (grid-for-each
     (lambda (pos)
       (match-let*
        ([around (four-directions pos)]
         [(list up down left right) around]
         [direction #f])
        ;; we must either have wall up and down, and free space left and
        ;; right, or the other way around
        (when (and (not (door?      (grid-ref level pos))) ; not already a door
                   (not (stairs-up? (grid-ref level pos)))
                   (andmap (lambda (cell) (inside-grid? level cell))
                           around)
                   (let ((c-up    (grid-ref-check level up))
                         (c-down  (grid-ref-check level down))
                         (c-left  (grid-ref-check level left))
                         (c-right (grid-ref-check level right)))
                     (define (doorway-wall? p)
                       (or (corner-wall? p) (tee-wall? p)))
                     (define (connection-check a b)
                       (let* ((rooms (floor-rooms new-floor))
                              (a     (get-room a rooms))
                              (b     (get-room b rooms)))
                         (not (connected? a b))))
                     (or (and (doorway-wall?  c-up)
                              (doorway-wall?  c-down)
                              (walkable-cell? c-left)
                              (walkable-cell? c-right)
                              ;; must not be connected already
                              (connection-check left right)
                              (begin (set! direction 'vertical)
                                     #t))
                         (and (doorway-wall?  c-left)
                              (doorway-wall?  c-right)
                              (walkable-cell? c-up)
                              (walkable-cell? c-down)
                              (connection-check up down)
                              (begin (set! direction 'horizontal)
                                     #t)))))
          ;; yes, we have found a valid doorway, if this doorway is in an
          ;; existing room, we would separate in into two smaller ones,
          ;; which is no fun, so only put a door if we would open a wall
          (let ((room (get-room pos (floor-rooms new-floor))))
            (when (not room)
              (add-door pos direction))))))
     level)

    ;; to avoid dead-end corridors, any corridor connected to a single room
    ;; tries to open a door to another room
    (for-each
     (lambda (room)
       (let ((neighbors (room-connected-to room)))
	 (when (and (eq? (room-type room) 'corridor)
                    (= (length neighbors) 1))
           (let* ((walls        (room-walls room))
                  (current-door (findf (lambda (pos)
                                         (door? (grid-ref-check level pos)))
                                       walls))
                  (door-candidate
                   (foldl
                    ;; we want the candidate farthest from the existing door
                    ;; if there are no suitable candidates, we just choose
                    ;; the existing door
                    (lambda (new best)
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
                              (andmap
                               (lambda (new)
                                 (walkable-cell? (grid-ref-check level new)))
                               sides))))
                     walls))))
             (when (not (eq? door-candidate current-door))
               (add-door door-candidate))))))
     (floor-rooms new-floor))

    ;; replace some doors with open doorways
    (grid-for-each
     (lambda (pos)
       (when (door? (grid-ref level pos))
         (when (cond [(andmap
                       (lambda (p)
                         (eq? (room-type (get-room p (floor-rooms new-floor)))
                              'corridor))
                       (wall-perpendicular level pos))
                      ;; a doorway is more likely between 2 corridors
                      (random-boolean 0.7)] ;; TODO tweak prob.
                     [else (random-boolean)]) ;; TODO tweak prob.
           (grid-set! level pos (new-empty-cell)))))
     level)
    ;; to avoid "phantom doors"
    (smooth-walls level)
    
    ;; register all walkable cells
    (set-floor-walkable-cells!
     new-floor
     (filter (lambda (pos)
	       (walkable-cell? (grid-ref-check (floor-map new-floor) pos)))
	     (apply append
		    (map room-cells (floor-rooms new-floor)))))
    
    ;; if needed, add the stairs down on a random free square in a room
    ;; (not a corridor)
    (when place-stairs-down?
      (let ((pos (random-free-position new-floor)))
        (grid-set! level pos (new-stairs-down))
        (set-floor-walkable-cells!
         new-floor (remove pos (floor-walkable-cells new-floor)))
        (set-floor-stairs-down! new-floor pos)))

    ;; make sure the exit is reachable from the entrance. otherwise start again
    ;; done with simple flood-fill that goes through doors
    (let loop ([queue (list (floor-stairs-up new-floor))]
               [visited '()])
      (cond [(null? queue) ; exit unreachable, reset generation
             (set! new-floor
                   (generate-dungeon-floor stairs-up-pos place-stairs-down?))]
            [else
             (let* ([head    (car queue)]
                    [at-head (grid-ref-check level head)]
                    [tail    (cdr queue)])
               (cond [(stairs-down? at-head) #t] ; done
                     [(member head visited) (loop tail visited)]
                     [(wall? at-head) (loop tail (cons head visited))]
                     [(or (empty-cell? at-head) (door? at-head))
                      (loop (append tail (four-directions head))
                            (cons head visited))]
                     [else ; something bad happened to the flood fill
                      ;; better start again
                      (loop '() '())]))]))
    
    new-floor))
