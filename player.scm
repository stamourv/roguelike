(define-class player (character)
  (slot: floors-before) ; pairs (map . view)
  (slot: current-floor)
  (slot: floors-after)
  (slot: level)
  (slot: experience) ;; TODO have a way to show level and experience
  (slot: inventory)) ; list of objects
(define (new-player name) ;; TODO constructor ?
  (let ((player (make-player name
			     (lambda () #\@)
			     #f
			     16 14 14 10 10 10 ;; TODO have a way to select (and also display, maybe press r for roster, c for character)
			     12 ; hp TODO consider constitution, also for monsters
			     1  ; base attack bonus
			     (new-equipment) ;; TODO add some starting equipment, to help the player defend itself
			     '()
			     #f
			     '()
			     1
			     0
			     '())))
    (place-player player (new-player-floor 0))
    player))


(define-type player-floor
  floor ; views are a grid of either visible, visited or unknown
  view)
(define (new-player-floor no)
  (let ((floor (generate-floor no (< no (- n-levels 1)))))
    (make-player-floor floor (init-visibility (floor-map floor)))))


(define (player-floor player)
  (player-floor-floor (player-current-floor player)))
(define (player-map   player)
  (floor-map (player-floor player)))
(define (player-view  player)
  (player-floor-view (player-current-floor player)))

(define (place-player
	 player player-floor
	 #!key (start-pos (floor-stairs-up (player-floor-floor player-floor))))
  (let ((map (floor-map (player-floor-floor player-floor))))
    (cell-occupant-set!        (grid-get map start-pos) player)
    (character-pos-set!        player start-pos)
    (player-current-floor-set! player player-floor)))


(define (show-state player)
  (cursor-notification-head)
  (display-notification (string-append (character-name player) "\n"))
  (display-notification
   (string-append "level " (number->string (player-level player)) "\n"))
  (display-notification
   (string-append (number->string (player-experience player))
		  " xp pts\n"))
  (display-notification
   (string-append (number->string (character-hp player))
		  " hp\n"))
  (display-notification
   (string-append "str: " (number->string (character-str player)) "	"
		  "int: " (number->string (character-int player)) "\n"))
  (display-notification
   (string-append "dex: " (number->string (character-dex player)) "	"
		  "wis: " (number->string (character-wis player)) "\n"))
  (display-notification
   (string-append "con: " (number->string (character-con player)) "	"
		  "cha: " (number->string (character-cha player)) "\n"))
  
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
     (display (string-append where (if obj (object-name obj) "") "\n"))) ;; TODO the tab doesn't quite do it, torso is still too short
   (character-equipment player))
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
	 (objects (cell-objects cell)))
    (choice player objects
	    (lambda (object)
	      (remove-object cell object)
	      (player-inventory-set! player
				     (cons object (player-inventory player))))
	    "There is nothing to pick up." "Pick up what?" "Picked up ")))
(define (drop player)
  (let ((cell    (grid-get (player-map player) (character-pos player)))
	(objects (player-inventory player)))
    (choice player objects
	    (lambda (object)
	      (player-inventory-set! player (remove object objects))
	      (add-object cell object))
	    "You have nothing to drop." "Drop what?" "Dropped ")))
(define (equip player)
  (let ((e       (character-equipment player))
	(objects (player-inventory    player)))
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
  (let* ((e       (character-equipment player))
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

(define (cmd-open player)
  (clear-to-bottom)
  (display "Open in which direction? ")
  (let ((dir (choose-direction))) ; evaluates to a function, or #f
    (if dir
	(let* ((grid (player-map player))
	       (cell (grid-get grid ((eval dir) (character-pos player))))) ;; TODO lots in common with close, and anything else that would ask for a direction
	  (open grid cell player)))))
(define (cmd-close player)
  (clear-to-bottom)
  (display "Close in which direction? ")
  (let ((dir  (choose-direction))) ; evaluates to a function, or #f
    (if dir
	(let* ((grid (player-map player))
	       (cell (grid-get grid ((eval dir) (character-pos player)))))
	  (close grid cell player)))))

(define (stairs player)
  (let ((cell (grid-get (player-map player) (character-pos player))))
    (let ((current      (player-current-floor         player))
	  (before       (player-floors-before         player))
	  (after        (player-floors-after          player)))
      (cond ((stairs-up? cell)
	     (cond ((not (null? before))
		    (let ((new (car before)))
		      (place-player
		       player new
		       start-pos: (floor-stairs-down (player-floor-floor new)))
		      (player-floors-after-set!  player (cons current after))
		      (player-floors-before-set! player (cdr before))))
		   (else (display "This would lead to the surface.\n"))))
	    ((stairs-down? cell)
	     (player-floors-before-set! player (cons current before))
	     (if (null? after)
		 ;; if we would generate the last floor, don't put stairs down
		 (place-player player
			       (new-player-floor
				(+ (floor-no (player-floor player))
				   1)))
		 (begin (place-player             player (car after))
			(player-floors-after-set! player (cdr after)))))
	    (else (display "There are no stairs here.\n"))))))

(define (kill player) ; insta-kill something TODO replace with a combat system
  (clear-to-bottom)
  (display "Kill in which direction? ")
  (let ((dir (choose-direction))) ; evaluates to a function, or #f
    (if dir
	(let* ((pos  ((eval dir) (character-pos player)))
	       (grid (player-map player))
	       (cell (grid-get grid pos)))
	  (cond ((cell-occupant cell)
		 => (lambda (occ)
		      (display (string-append "Killed the "
					      (character-name occ)
					      "\n"))
		      (remove-monster (player-floor player) occ player)))
		(else (display "There is nothing to kill there.\n")))))))

(define (add-experience player xp)
  (player-experience-set! player (+ (player-experience player) xp)))
