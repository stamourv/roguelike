(define-type-of-character player
  levels-before ; pairs (map . view)
  level ; views are a grid of either visible, visited or unknown
  levels-after
  inventory) ; list of objects
(define (new-player name)
  (let ((player (make-player name
			     (lambda () #\@)
			     #f
			     (new-equipment)
			     '()
			     #f
			     '()
			     '())))
    (place-player player (new-level 0))
    player))
(define player-name      character-name)
(define player-pos       character-pos)
(define player-equipment character-equipment)
(define player-pos-set!  character-pos-set!)


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
	     print-fun: (visibility-printer (player-view player))))


(define (inventory player)
  (cursor-home) ;; TODO also done in choice, abstract ?
  (clear-to-bottom)
  (display "Equipment:\n")
  (for-each-equipped
   (lambda (obj where)
     (display (string-append where ":	" (if obj (object-name obj) "") "\n"))) ;; TODO the tab doesn't quite do it, torso is still too short
   (player-equipment player))
  (display "\nInventory:\n")
  (for-each (lambda (o) (display (string-append (object-name o) "\n")))
	    (player-inventory player))
  (let loop ((c #f))
    (if (not (eq? c #\q))
	(begin (display "\nPress q.")
	       (loop (read-char)))))
  (clear-to-bottom))
(define (pick-up player pos) ;; TODO pos can be useful if we can pick up at a distance
  (let* ((cell    (grid-get (player-map player) pos))
	 (objects (get-objects cell)))
    (choice player objects
	    (lambda (object)
	      (remove-object cell object)
	      (player-inventory-set! player
				     (cons object (player-inventory player))))
	    "There is nothing to pick up." "Pick up what?" "Picked up ")))
(define (drop player)
  (let ((cell    (grid-get (player-map player) (player-pos player)))
	(objects (player-inventory player)))
    (choice player objects
	    (lambda (object)
	      (player-inventory-set! player (remove object objects))
	      (add-object cell object))
	    "You have nothing to drop." "Drop what?" "Dropped ")))
(define (equip player)
  (let ((e       (player-equipment player))
	(objects (player-inventory player)))
    (choice player objects
	    (lambda (object)
	      (let* ((place (cond ((weapon?     object) 'main-arm) ;; TODO weapons can go either in the main or the off hand, and also consider 2 handed weapons
				  ((shield?     object) 'off-arm)
				  ((body-armor? object) 'torso)))
		     (old   ((case place
			       ((main-arm) equipment-main-arm) ;; TODO string->symbol and co ?
			       ((off-arm)  equipment-off-arm)
			       ((torso)    equipment-torso))
			     e)))
		(player-inventory-set! player (remove object objects))
		((case place
		   ((main-arm) equipment-main-arm-set!)
		   ((off-arm)  equipment-off-arm-set!)
		   ((torso)    equipment-torso-set!))
		 e object)
		(if old
		    (begin (display (string-append "\nPut "
						   (object-name old)
						   " back in inventory.\n"))
			   (player-inventory-set!
			    player
			    (cons old (player-inventory player)))))))
	    "You have nothing to equip." "Equip what?" "Equipped ")))
(define (take-off player)
  (let* ((e       (player-equipment player))
	 (objects (filter identity (map car (equipment->list e)))))
    (choice player objects
	    (lambda (object)
	      (cond ((weapon?     object)
		     (equipment-main-arm-set! e #f))
		    ((shield?     object)
		     (equipment-off-arm-set!  e #f))
		    ((body-armor? object)
		     (equipment-torso-set!    e #f)))
	      (player-inventory-set! player
				     (cons object (player-inventory player))))
	    "You have nothing to take off." "Take off what?" "Took off ")))

(define (open player)
  (clear-to-bottom)
  (display "Open in which direction? ")
  (let ((dir (choose-direction))) ; evaluates to a function, or #f
    (if dir
	(let* ((pos  ((eval dir) (player-pos player)))
	       (grid (player-map player))
	       (cell (grid-get grid pos))) ;; TODO lots in common with close, and anything else that would ask for a direction
	  (cond ((door?       cell) (open-door  grid pos player))
		((open-door?  cell) (display "This door is already open.\n"))
		((chest?      cell) (open-chest grid pos player))
		((open-chest? cell) (display "This chest is already open.\n"))
		(else              (display "I can't open that.\n")))))))
(define (close player)
  (clear-to-bottom)
  (display "Close in which direction? ")
  (let ((dir  (choose-direction))) ; evaluates to a function, or #f
    (if dir
	(let* ((pos  ((eval dir) (player-pos player)))
	       (grid (player-mpa player))
	       (cell (grid-get grid pos)))
	  (cond ((open-door? cell) (close-door grid pos player)) ;; TODO would be better to send a "close" message to the object
		((door?      cell) (display "This door is already closed.\n"))
		;; chests can't be closed
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
  (clear-to-bottom)
  (display "Kill in which direction? ")
  (let ((dir (choose-direction))) ; evaluates to a function, or #f
    (if dir
	(let* ((pos  ((eval dir) (player-pos player)))
	       (grid (player-map player))
	       (cell (grid-get grid pos)))
	  (cond ((get-occupant cell)
		 => (lambda (occ)
		      (display (string-append "Killed the "
					      (character-name occ)
					      "\n"))
		      (remove-monster (player-floor player) occ)))
		(else (display "There is nothing to kill there.\n")))))))
