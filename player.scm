(define-type-of-character player
  levels-before ; pairs (map . view)
  level ; views are a grid of either visible, visited or unknown
  levels-after
  inventory) ; list of objects
(define (new-player name)
  (let ((player (make-player name
			     (lambda () #\@)
			     (new-equipment)
			     #f
			     '()
			     #f
			     '()
			     '())))
    (place-player player (new-level 0))
    player))
(define player-name     character-name)
(define player-pos      character-pos)
(define player-pos-set! character-pos-set!)


(define-type level
  floor
  view)
(define (new-level no)
  (let ((floor (generate-floor no (< no (- n-levels 1)))))
    (make-level floor (init-visibility (floor-map floor)))))


(define (player-floor player)
  (level-floor (player-level player)))
(define (player-map   player)
  (floor-map (player-floor player)))
(define (player-view  player)
  (level-view (player-level player)))

(define (place-player
	 player level
	 #!key (start-pos (floor-stairs-up (level-floor level))))
  (let ((map (floor-map (level-floor level))))
    (occupant-set! (grid-get map start-pos) player)
    (player-pos-set!   player start-pos)
    (player-level-set! player level)))


(define (show-state player)
  (cursor-home)
  (clear-line)
  (display (string-append
	    "Level "
	    (number->string
	     (+ (floor-no (player-floor player)) 1))
	    "\n"))
  (show-grid (player-map player)
	     print-fun: (visibility-printer (player-view player)))
  (clear-to-bottom))


(define (pick-up player pos)
  (let* ((cell    (grid-get (player-map player) pos))
	 (objects (get-objects cell)))
    (if (null? objects)
	(display "There is nothing to pick up.")
	(let ((object (if (= (length objects) 1)
			  (car objects)
			  (choice objects))))
	  (if object
	      (begin (display "Picked up ")
		     (display (object-name object))
		     (remove-object cell object)
		     (player-inventory-set!
		      player (cons object (player-inventory player)))))))))
(define (drop player)
  (let ((objects (player-inventory player)))
    (if (null? objects)
	(display "You have nothing to drop.")
	(begin
	  (display "Drop ")
	  (let ((cell   (grid-get (player-map player) (player-pos player)))
		(object (choice objects)))
	    (if object
		(begin (display "Dropped ")
		       (display (object-name object))
		       (player-inventory-set! player (remove object objects))
		       (add-object cell object))))))))
(define (inventory player)
  (display "Inventory:\n")
  (for-each (lambda (o) (display (object-name o)) (display "\n"))
	    ;; TODO watch out for the limits of the screen, maybe have more than 1 column, maybe have on its own screen ?
	    (player-inventory player))
  (read-char))

;; TODO for now, just for doors, but could be used for chests too
(define (open player)
  (display "Open in which direction? ")
  (let ((dir (choose-direction))) ; evaluates to a function, or #f
    (if dir
	(let* ((pos  ((eval dir) (player-pos player)))
	       (grid (player-map player))
	       (cell (grid-get grid pos))) ;; TODO lots in common with close, and anything else that would ask for a direction
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
		       start-pos: (floor-stairs-down (level-floor new)))
		      (player-levels-after-set!  player (cons level after))
		      (player-levels-before-set! player (cdr before))))
		   (else (display "This would lead to the surface.\n"))))
	    ((stairs-down? cell)
	     (player-levels-before-set! player (cons level before))
	     (if (null? after)
		 ;; if we would generate the last level, don't put stairs down
		 (place-player player
			       (new-level
				(+ (floor-no (player-floor player))
				   1)))
		 (begin (place-player             player (car after))
			(player-levels-after-set! player (cdr after)))))
	    (else (display "There are no stairs here.\n"))))))

(define (kill player) ; insta-kill something TODO replace with a combat system
  (display "Kill in which direction? ")
  (let ((dir (choose-direction))) ; evaluates to a function, or #f
    (if dir
	(let* ((pos  ((eval dir) (player-pos player))) ;; TODO doesn't work
	       (grid (player-map player))
	       (cell (grid-get grid pos)))
	  (cond ((get-occupant cell)
		 => (lambda (occ)
		      (display (string-append "Killed the "
					      (character-name occ)
					      "\n"))
		      (remove-monster (player-floor player) occ)))
		(else (display "There is nothing to kill there.\n")))))))
