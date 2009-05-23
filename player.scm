(define-type-of-occupant player
  levels-before ; pairs (map . view)
  level ; views are a grid of either visible, visited or unknown
  level-no
  levels-after
  pos
  inventory) ; list of objects
(define (new-player name)
  (let ((player (make-player name
			     (lambda () #\@)
			     '()
			     #f
			     0
			     '()
			     #f
			     '())))
    (place-player player (new-level))
    player))
(define player-name occupant-name)

(define-type level
  map
  view)
(define (new-level #!optional (stairs-down? #t))
  (let ((map (generate-level stairs-down?)))
    (make-level map (init-visibility map))))
(define (player-map  player) (level-map  (player-level player)))
(define (player-view player) (level-view (player-level player)))

(define (show-state player)
  (cursor-home)
  (display (string-append "Level "
			  (number->string (+ (player-level-no player) 1))
			  "\n"))
  (show-grid (player-map player)
	     print-fun: (visibility-printer (player-view player))))


(define (pick-up player pos) ;; TODO adapt when I have more than 1 object per tile, do something like drop does
  (let* ((cell   (grid-get (player-map player) pos))
	 (object (get-object cell)))
    (if (not object)
	(display "There is nothing to pick up.")
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

;; TODO for now, just for doors, but could be used for chests too
(define (open player)
  (display "Open in which direction? ")
  (let* ((dir  (choose-direction)) ; evaluates to a function
	 (pos  ((eval dir) (player-pos player)))
	 (grid (player-map player))
	 (cell (grid-get grid pos))) ;; TODO lots in common with close
    (cond ((door?      cell) (open-door grid pos player))
	  ((open-door? cell) (display "This door is already open.\n"))
	  (else              (display "I can't open that.\n")))))
(define (close player)
  (display "Close in which direction? ")
  (let* ((dir  (choose-direction)) ; evaluates to a function
	 (pos  ((eval dir) (player-pos player)))
	 (grid (player-mpa player))
	 (cell (grid-get grid pos)))
    (cond ((open-door? cell) (close-door grid pos player)) ;; TODO would be better to send a "close" message to the object
	  ((door?      cell) (display "This door is already closed.\n"))
	  (else              (display "I can't close that.\n")))))

(define (place-player player level #!key (start-pred stairs-up?))
  (let* ((map   (level-map level))
	 (start (grid-find map start-pred)))
    (occupant-set! (grid-get map start) player)
    (player-pos-set!   player start)
    (player-level-set! player level)))

(define (stairs player)
  (let ((cell (grid-get (player-map player) (player-pos player))))
    (let ((level  (player-level         player))
	  (before (player-levels-before player))
	  (after  (player-levels-after  player)))
      (cond ((stairs-up? cell)
	     (cond ((not (null? before))
		    (let ((new (car before)))
		      (place-player player new start-pred: stairs-down?)
		      (player-levels-after-set!  player (cons level after))
		      (player-levels-before-set! player (cdr before))))
		   (else (display "This would lead to the surface.\n"))))
	    ((stairs-down? cell)
	     (player-levels-before-set! player (cons level before))
	     (if (null? after)
		 ;; TODO see if we reached the bottom, or we could be going down ad infinitum, if so, use #f instead of #t
		 (place-player player (new-level #t))
		 (begin (place-player             player (car after))
			(player-levels-after-set! player (cdr after)))))
	    (else (display "There are no stairs here.\n"))))))
