(define-class player (character)
  (slot: floors-before) ; pairs (map . view)
  (slot: current-floor)
  (slot: floors-after)
  (slot: level)
  (slot: experience) ;; TODO have a way to show level and experience
  (slot: inventory)) ; list of objects
(define (new-player name) ;; TODO constructor ?
  (let ((player (make-player name #f
			     16 14 14 10 10 10 ;; TODO have a way to select (and also display, maybe press r for roster, c for character)
			     '(10) ; hit dice
			     #f #f
			     1  ; base attack bonus
			     (new-equipment main-arm: (new-club))
			     '() #f '()
			     1 0
			     '())))
    (init-hp player #t) ; the player gets maxed out hp
    (place-player player (new-player-floor 0))
    player))
(define-method (print (p player)) #\@)


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
    (cell-occupant-set!        (grid-ref map start-pos) player)
    (character-pos-set!        player start-pos)
    (player-current-floor-set! player player-floor)))


(define (show-state)
  (cursor-notification-head)
  (display-notification (character-name player) "\n")
  (display-notification "level " (number->string (player-level player)) "\n")
  (display-notification (number->string (player-experience player)) " xp pts\n")
  (display-notification (number->string (character-hp player)) " hp\n")
  (display-notification "AC: "  (number->string (get-armor-class player)) "\n")
  (display-notification "str: " (number->string (character-str player))
			"	"
			"int: " (number->string (character-int player)) "\n")
  (display-notification "dex: " (number->string (character-dex player))
			"	"
			"wis: " (number->string (character-wis player)) "\n")
  (display-notification "con: " (number->string (character-con player))
			"	"
			"cha: " (number->string (character-cha player)) "\n")
  
  (cursor-home)
  (clear-line)
  (display (string-append
	    "Level "
	    (number->string
	     (+ (floor-no (player-floor player)) 1))
	    "\n"))
  (show-grid (player-map player)
	     print-fun: (visibility-printer (player-view player))))


(define (inventory)
  (cursor-home) ;; TODO also done in choice, abstract ?
  (clear-to-bottom)
  (display "Equipment:\n")
  (for-each-equipped
   (lambda (obj where)
     (display (string-append where (if obj (object-info obj) "") "\n"))) ;; TODO the tab doesn't quite do it, torso is still too short
   (character-equipment player))
  (display
   (string-append "\nAC: " (number->string (get-armor-class player)) "\n"))
  (display "\nInventory:\n")
  (for-each (lambda (o) (display (string-append (object-info o) "\n")))
	    (player-inventory player))
  (let loop ((c #f))
    (case c
      ((#\e) (equip))
      ((#\r) (take-off))
      ((#\d) (drop))
      ((#\q) #f)
      (else  (display "\ne: Equip\nr: Take off\nd: Drop\nq: Cancel\n")
	     (loop (read-char)))))
  (clear-to-bottom))
(define (pick-up pos) ;; TODO pos can be useful if we can pick up at a distance
  (let* ((cell    (grid-ref (player-map player) pos))
	 (objects (cell-objects cell)))
    (choice objects
	    (lambda (object)
	      (remove-object cell object)
	      (player-inventory-set! player
				     (cons object (player-inventory player))))
	    "There is nothing to pick up." "Pick up what?" "Picked up ")))
(define (drop)
  (let ((cell    (grid-ref (player-map player) (character-pos player)))
	(objects (player-inventory player)))
    (choice objects
	    (lambda (object)
	      (player-inventory-set! player (remove object objects))
	      (add-object cell object))
	    "You have nothing to drop." "Drop what?" "Dropped ")))
(define (equip)
  (let ((e       (character-equipment player))
	(objects (filter (lambda (x) (equipable-object? x))
			 (player-inventory player))))
    (choice objects
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
(define (take-off)
  (let* ((e       (character-equipment player))
	 (objects (filter identity (map car (equipment->list e)))))
    (choice objects
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

(define (cmd-open)
  (clear-to-bottom)
  (display "Open in which direction? ")
  (let ((dir (choose-direction))) ; evaluates to a function, or #f
    (if dir
	(let* ((grid (player-map player))
	       (cell (grid-ref grid ((eval dir) (character-pos player))))) ;; TODO lots in common with close, and anything else that would ask for a direction
	  (open grid cell player)))))
(define (cmd-close)
  (clear-to-bottom)
  (display "Close in which direction? ")
  (let ((dir  (choose-direction))) ; evaluates to a function, or #f
    (if dir
	(let* ((grid (player-map player))
	       (cell (grid-ref grid ((eval dir) (character-pos player)))))
	  (close grid cell player)))))

(define (stairs)
  (let ((cell (grid-ref (player-map player) (character-pos player))))
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

(define (kill) ; insta-kill something TODO replace with a combat system
  (clear-to-bottom)
  (display "Kill in which direction? ")
  (let ((dir (choose-direction))) ; evaluates to a function, or #f
    (if dir
	(let* ((pos  ((eval dir) (character-pos player)))
	       (grid (player-map player))
	       (cell (grid-ref grid pos)))
	  (cond ((cell-occupant cell)
		 => (lambda (occ)
		      (display (string-append "Killed the "
					      (character-name occ)
					      "\n"))
		      (remove-monster occ)))
		(else (display "There is nothing to kill there.\n")))))))

(define (add-experience xp)
  (let ((total (+ (player-experience player) xp))
	(level (player-level player)))
    (player-experience-set! player total)
    (if (>= total (* 1000 (/ (* level (+ level 1)) 2))) ; 1000, 3000, 6000, ...
	(level-up))))

(define (level-up) ;; TODO have attribute upgrades, etc
  (display "Level up!\n")
  (player-level-set! player (+ (player-level player) 1))
  (let ((hd (character-hit-dice player)))
    (character-hit-dice-set! player (cons (car hd) hd))
    (init-hp player #t))
  (character-base-attack-bonus-set!
   player (+ (character-base-attack-bonus player) 1)))
