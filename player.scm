(define-class player (character)
  (slot: floors-before) ; pairs (map . view)
  (slot: current-floor)
  (slot: floors-after)

  (slot: level)
  (slot: experience)
    
  (slot: inventory)) ; list of objects
(define (new-player name) ;; TODO constructor ?
  (let ((player (make-player name #f #f
			     16 14 14 10 10 10 ;; TODO have a way to select (and also display, maybe press r for roster, c for character)
			     (make-table)
			     '(10) ; hit dice
			     #f #f
			     1 ; base attack bonus
			     6 ; speed, 6 seconds for a turn
			     (new-equipment main-hand: (new-club))
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
  (let* ((floor (player-floor-floor player-floor))
	 (map   (floor-map floor)))
    (cell-occupant-set!        (grid-ref map start-pos) player)
    (character-pos-set!        player start-pos)
    (player-current-floor-set! player player-floor)
    (character-floor-no-set!   player (+ (floor-no floor) 1))
    (set! turn-no 0)
    (set! turn-id 0)
    (set! turn-queue '())
    ;; no need to reschedule the player, since he will get rescheduled at the
    ;; end of his turn
    (for-each reschedule (floor-monsters floor))))


(define (show-state)
  (cursor-notification-head)
  (display-notification (character-name player) "\n")
  (display-notification "level " (number->string (player-level player)) "\n")
  (display-notification (number->string (player-experience player)) " xp pts\n")
  (display-notification "")
  (if (altered-attr? player 'hp) (terminal-colors 'white 'black)) ;; TODO abstract that
  (display (number->string (character-hp player)))
  (terminal-reset)
  (display
   (string-append "/" (number->string (character-max-hp player)) " hp\n"))
  
  (display-notification "AC: "  (number->string (get-armor-class player)) "\n")
  
  (display-notification "str: ")
  (if (altered-attr? player 'str) (terminal-colors 'white 'black))
  (display (number->string (character-str player)))
  (terminal-reset)
  (display "	int: ")
  (if (altered-attr? player 'int) (terminal-colors 'white 'black))
  (display (number->string (character-int player)))
  (terminal-reset)
  (display "\n")
  
  (display-notification "dex: ")
  (if (altered-attr? player 'dex) (terminal-colors 'white 'black))
  (display (number->string (character-dex player)))
  (terminal-reset)
  (display "	wis: ")
  (if (altered-attr? player 'wis) (terminal-colors 'white 'black))
  (display (number->string (character-wis player)))
  (terminal-reset)
  (display "\n")
  
  (display-notification "con: ")
  (if (altered-attr? player 'con) (terminal-colors 'white 'black))
  (display (number->string (character-con player)))
  (terminal-reset)
  (display "	cha: ")
  (if (altered-attr? player 'cha) (terminal-colors 'white 'black))
  (display (number->string (character-cha player)))
  (terminal-reset)
  (display "\n")
  
  (cursor-home)
  (clear-line)
  (display (string-append
	    "Floor "
	    (number->string
	     (+ (floor-no (player-floor player)) 1))
	    "\n"))
  (show-grid (player-map player)
	     print-fun: (visibility-printer (player-view player))))


(define (inventory)
  (cursor-home)
  (clear-to-bottom)
  (display "Equipment:\n")
  (for-each-equipped
   (lambda (obj where)
     (display (string-append where (if obj (object-info obj) "") "\n")))
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
	      (let* ((place (cond ((weapon?     object) 'main-hand) ;; TODO string->symbol and co ?
				  ((shield?     object) 'off-hand)
				  ((body-armor? object) 'torso)))
		     (old   ((case place
			       ((main-hand) equipment-main-hand)
			       ((off-hand)  equipment-off-hand)
			       ((torso)     equipment-torso))
			     e)))
		(define (back-in-inventory o)
		  (display (string-append "Put " (object-name o)
					  " back in inventory.\n"))
		  (player-inventory-set!
		   player (cons o (player-inventory player))))
		(player-inventory-set! player (remove object objects))
		((case place
		   ((main-hand) equipment-main-hand-set!)
		   ((off-hand)  equipment-off-hand-set!)
		   ((torso)     equipment-torso-set!))
		 e object)
		(cond ((and old (not (off-hand-placeholder? old))) ;; TODO generalize with all non-removable items
		       (back-in-inventory old))
		      ((two-handed-weapon? old)
		       (equipment-off-hand-set! e #f)) ; remove the placeholder
		      ((off-hand-placeholder? old)
		       ;; we have to remove the two-handed weapon itself
		       (back-in-inventory (equipment-main-hand e))
		       (equipment-main-hand-set! e #f)))
		(if (two-handed-weapon? object)
		    (let ((old-off (equipment-off-hand e)))
		      (if (and old-off (not (off-hand-placeholder? old-off)))
			  (back-in-inventory old-off))
		      (equipment-off-hand-set! e (new-off-hand-placeholder))))))
	    "You have nothing to equip." "Equip what?" "Equipped ")))
(define (take-off)
  (let* ((e       (character-equipment player))
	 (objects (filter (lambda (obj) (and obj (removable? obj)))
			  (map car (equipment->list e)))))
    (choice objects
	    (lambda (object)
	      (cond ((weapon?     object)
		     (equipment-main-hand-set! e #f)
		     (if (two-handed-weapon? object)
			 ;; remove the placeholder
			 (equipment-off-hand-set! e #f)))
		    ((shield?     object)
		     (equipment-off-hand-set!  e #f))
		    ((body-armor? object)
		     (equipment-torso-set!     e #f)))
	      (player-inventory-set! player
				     (cons object (player-inventory player))))
	    "You have nothing to take off." "Take off what?" "Took off ")))

(define (cmd-drink)
  (let* ((e       (character-equipment player))
	 (objects (player-inventory player))
	 (o       #f))
    (choice objects
	    (lambda (object)
	      (set! o object)
	      (player-inventory-set! player (remove object objects)))
	    "You have nothing to drink." "Drink what?" "Drank ")
    ;; necessary to display the messages in the right order
    (if o (drink o))))


(define (direction-command name f)
  (clear-to-bottom)
  (display (string-append name " in which direction? "))
  (let ((dir (choose-direction))) ; evaluates to a function, or #f
    (if dir
	(let* ((grid (player-map player))
	       (cell (grid-ref grid ((eval dir) (character-pos player)))))
	  (f grid cell player)))))
(define (cmd-open)  (direction-command "Open"  open))
(define (cmd-close) (direction-command "Close" close))
(define (kill) ; insta-kill something, for debugging purposes
  (direction-command "Kill"
		     (lambda (grid cell player)
		       (cond ((cell-occupant cell)
			      => (lambda (occ)
				   (display (string-append "Killed the "
							   (character-name occ)
							   "\n"))
				   (remove-monster occ)))
			     (else (display
				    "There is nothing to kill there.\n"))))))

(define (shoot) ;; TODO have shooting monsters too
  (let* ((grid    (player-map player))
	 (weapon  (equipment-main-hand (character-equipment player)))
	 (targets (filter (lambda (m)
			    (and (eq? (grid-ref (player-view player)
						(character-pos m))
				      'visible)
				 (line-of-sight? grid
						 (character-pos player)
						 (character-pos m)
						 #t)))
			  (floor-monsters (player-floor player))))
	 (n       (length targets)))
    (cond
     ((not (ranged-weapon? weapon))
      (display "I can't shoot with that.\n"))
     ((null? targets)
      (display "There is nothing to shoot.\n"))
     (else
      (let ((grid (let ((grid (grid-copy grid)))
		    (for-each
		     (lambda (m n) ;; TODO this, like choice, won't scale over 10
		       (grid-set!
			grid
			(character-pos m)
			(new-display-cell
			 (string-ref (number->string (+ n 1)) 0))))
		     targets
		     (iota n))
		    grid)))
	;; show which monsters are which numbers
	(cursor-home) ;; TODO taken from show-state
	(clear-to-bottom)	  
	(cursor-notification-head)
	(for-each (lambda (m n)
		    (display-notification (number->string (+ n 1)) ": "
					  (character-name m) "\n"))
		  targets
		  (iota n))
	(display "\n")
	(display-notification "q: Cancel\n")
	;; show the map with the target numbers
	(cursor-home)
	(display (string-append
		  "Floor "
		  (number->string
		   (+ (floor-no (player-floor player)) 1))
		  "\n"))
	(show-grid grid
		   print-fun: (visibility-printer (player-view player)))
	;; choose a target
	(let ((nb (read-number n)))
	  (if nb
	      (let ((target (list-ref targets nb))
		    (roll   ((dice 20))))
		(display (string-append (character-name player) ;; TODO abstract with attack in character.scm
					" shoots at the "
					(character-name target)))
		(if (>= (+ roll (get-ranged-attack-bonus player))
			(get-armor-class target))
		    (damage player target)
		    (display " and misses.\n"))))))))))

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
