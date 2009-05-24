(define-type-of-character player
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
    (place-player player (new-player-level))
    player))
(define player-name character-name)


(define-type player-level
  level
  view)
(define (new-player-level #!optional (stairs-down? #t))
  (let ((level (generate-level stairs-down?)))
    (make-player-level level (init-visibility (level-map level)))))

(define (place-player
	 player level
	 #!key (start-pos (level-stairs-up (player-level-level level))))
  (let ((map (level-map (player-level-level level))))
    (occupant-set! (grid-get map start-pos) player)
    (player-pos-set!   player start-pos)
    (player-level-set! player level)))

(define (player-map  player)
  (level-map (player-level-level (player-level player))))
(define (player-view player)
  (player-level-view (player-level player)))


(define (show-state player)
  (cursor-home)
  (clear-line)
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
  (let ((dir (choose-direction))) ; evaluates to a function, or #f
    (if dir
	(let* ((pos  ((eval dir) (player-pos player)))
	       (grid (player-map player))
	       (cell (grid-get grid pos))) ;; TODO lots in common with close
	  (cond ((door?      cell) (open-door grid pos player))
		((open-door? cell) (display "This door is already open.\n"))
		(else              (display "I can't open that.\n")))))))
(define (close player)
  (display "Close in which direction? ")
  (let ((dir  (choose-direction))) ; evaluates to a function, or #f
    (if dir
	(let* ((pos  ((eval dir) (player-pos player)))
	       (grid (player-mpa player))
	       (cell (grid-get grid pos)))
	  (cond ((open-door? cell) (close-door grid pos player)) ;; TODO would be better to send a "close" message to the object
		((door?      cell) (display "This door is already closed.\n"))
		(else              (display "I can't close that.\n")))))))

(define (stairs player)
  (let ((cell (grid-get (player-map player) (player-pos player))))
    (let ((level  (player-level         player))
	  (before (player-levels-before player))
	  (after  (player-levels-after  player)))
      (cond ((stairs-up? cell)
	     (cond ((not (null? before))
		    (let ((new (car before)))
		      (place-player
		       player new
		       start-pos: (level-stairs-down (player-level-level new)))
		      (player-levels-after-set!  player (cons level after))
		      (player-levels-before-set! player (cdr before)))
		    (player-level-no-set! player
					  (- (player-level-no player) 1)))
		   (else (display "This would lead to the surface.\n"))))
	    ((stairs-down? cell)
	     (player-levels-before-set! player (cons level before))
	     (if (null? after)
		 ;; if we would generate the last level, don't put stairs down
		 (place-player player
			       (new-player-level (< (player-level-no player) ;; TODO one too many levels
						    (- n-levels 2))))
		 (begin (place-player             player (car after))
			(player-levels-after-set! player (cdr after))))
	     (player-level-no-set! player
				   (+ (player-level-no player) 1)))
	    (else (display "There are no stairs here.\n"))))))
