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
