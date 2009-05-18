(define-type-of-occupant player
  map
  pos
  view ; grid of either visible, visited or unknown
  inventory) ; list of objects
(define (new-player name map pos view)
  (make-player name
	       (lambda () #\@)
	       map
	       pos
	       view
	       '()))
(define player-name occupant-name)


(define (update-visibility player) ;; TODO maybe show visible parts in dark yellow instead of white background ? to simulate a lantern
  ;; set the fog of war
  (let ((view (player-view player)))
    (for-each (lambda (x)
		(for-each (lambda (y)
			    (if (eq? (grid-get view (new-point x y)) 'visible)
				(grid-set! view (new-point x y) 'visited)))
			  (iota (grid-width view))))
	      (iota (grid-height view)))

    ;; field of vision using shadow casting (spiral path FOV)
    ;; see http://roguebasin.roguelikedevelopment.org/index.php?title=Spiral_Path_FOV
    (let* ((g   (player-map player))
	   (pos (player-pos player))
	   (x   (point-x    pos))
	   (y   (point-y    pos)))
      (let loop ((queue (list pos)))
	(define (pass-light pos new) ;; TODO nice, but maybe a bit too permissive, maybe instead of checking the quadrant, be more restrictive ?
	  ;; enqueue cells depending on the orientation of new from pos
	  (let* ((pos-x (point-x pos)) (pos-y (point-y pos))
		 (new-x (point-x new)) (new-y (point-y new))
		 (north (new-point (- new-x 1) new-y))
		 (south (new-point (+ new-x 1) new-y))
		 (west  (new-point new-x (- new-y 1)))
		 (east  (new-point new-x (+ new-y 1))))
	    (cond ((< new-x pos-x) ; somewhere north
		   (cond ((= new-y pos-y) (list east north west)) ; due north
			 ((< new-y pos-y) (list north west))      ; north-west
			 ((> new-y pos-y) (list east north))))    ; north-east
		  ((> new-x pos-x) ; somewhere south
		   (cond ((= new-y pos-y) (list west south east)) ; due south
			 ((< new-y pos-y) (list west south))      ; south-west
			 ((> new-y pos-y) (list south east))))    ; south-east
		  ((< new-y pos-y) (list north west south))       ; due west
		  ((> new-y pos-y) (list south east north))       ; due east
		  (else ; we are at the starting point
		   (list east north west south)))))
	(if (not (null? queue))
	    (let ((new (car queue)))
	      (if (and (inside-grid? g new)
		       (not (eq? (grid-get view new) 'visible))) ; already seen
		  (begin (grid-set! view new 'visible) ; mark as lit
			 (if (not (opaque-cell? (grid-get g new)))
			     (loop (append (cdr queue)
					   (pass-light pos new))))))
	      (loop (cdr queue))))))))



(define (pick-up player pos) ;; TODO adapt when I have more than 1 object per tile, do something like drop does
  (let* ((cell   (grid-get (player-map player) pos))
	 (object (get-object cell)))
    (if (not object)
	(display "Nothing to pick up.")
	(begin (display "Picked up ")
	       (display (object-name object))
	       (remove-object cell object)
	       (player-inventory-set! player
				      (cons object
					    (player-inventory player)))))))
(define (drop player)
  (let ((objects (player-inventory player)))
    (if (null? objects)
	(display "You have nothing to drop.")
	(begin
	  (display "Drop what?\n")
	  (let loop ((objects objects)
		     (i       1))
	    (if (not (null? objects))
		(begin (display (string-append (number->string i)
					       ": "
					       (object-name (car objects))
					       "\n"))
		       (loop (cdr objects) (+ i 1)))))
	  (let loop ((nb (read-char)))
	    (if (not (and (char>=? nb #\1)
			  (<= (- (char->integer nb) (char->integer #\0))
			      (length objects))))
		(loop (read-char))
		(let* ((cell   (grid-get (player-map player)
					 (player-pos player)))
		       (nb     (- (char->integer nb) (char->integer #\0) 1))
		       (object (list-ref objects nb)))
		  (display "Dropped ")
		  (display (object-name object))
		  (player-inventory-set! player (remove-at-index objects nb))
		  (add-object cell object))))))))
(define (inventory player)
  (display "Inventory:\n")
  (for-each (lambda (o) (display (object-name o)) (display "\n"))
	    ;; TODO watch out for the limits of the screen, maybe have more than 1 column, maybe have on its own screen ?
	    (player-inventory player)))
