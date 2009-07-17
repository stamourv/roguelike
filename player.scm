(import class)
(import utilities)
(import cell)
(import grid)
(import scheduler)
(import character)
(import objects)
(import dungeon)
(import encounters)
(import treasure)
(import visibility)
(import common)
(import terminal)

(define-class player (character)
  (slot: floors-before) ; pairs (map . view)
  (slot: current-floor)
  (slot: floors-after)

  (slot: experience)
    
  (slot: inventory)) ; list of objects
(define (new-player name) ;; TODO constructor ?
  (let ((player (make-player name #f #f
			     16 14 14 10 10 10 (make-table) 0 ;; TODO have a way to select (and also display, maybe press r for roster, c for character)
			     1 '(10) ; hit dice
			     #f #f
			     1 ; base attack bonus
			     6 ; speed, 6 seconds for a turn
			     (new-equipment main-hand: (new-club))
			     '() #f '()
			     0 '())))
    (init-hp player #t) ; the player gets maxed out hp
    (place-player player (new-player-floor 0))
    player))
(define-method (print (p player)) #\@)

(define-method (turn (p player))
  (if (<= (character-hp player) 0)
      (begin (display "You die.\n")
	     (quit))
      (begin (update-visibility)
	     (show-state)
	     (read-command)
	     (reschedule player))))

(define-type player-floor
  floor ; views are a grid of either visible, visited or unknown
  view)
(define (new-player-floor no)
  (let ((floor (generate-floor no (< no (- n-levels 1)))))
    ;; add everything else on top
    (generate-encounters floor)
    (generate-treasure   floor)
    (make-player-floor floor (init-visibility (floor-map floor)))))


(define (player-map   player)
  (floor-map (character-floor player)))
(define (player-view  player)
  (player-floor-view (player-current-floor player)))

(define (place-player
	 player player-floor
	 #!key (start-pos #f))
  (if (not start-pos)
      (set! start-pos (floor-stairs-up (player-floor-floor player-floor)))) ;; FOO put as default value once black hole fully supports key parameters
  (let* ((floor (player-floor-floor player-floor))
	 (map   (floor-map floor)))
    (cell-occupant-set!        (grid-ref map start-pos) player)
    (character-pos-set!        player start-pos)
    (player-current-floor-set! player player-floor)
    (character-floor-set!      player floor)
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
  
  (display-notification "AC: ")
  (if (altered-attr? player 'natural-ac) (terminal-colors 'white 'black))
  (display (number->string (get-armor-class player)))
  (terminal-reset)
  (display "\n")
  
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
	     (+ (floor-no (character-floor player)) 1))
	    "\n"))
  (show-grid (player-map player)
	     print-fun: (visibility-printer (player-view player)
					    (player-map  player))))


(define (update-visibility) ;; TODO maybe show visible parts in dark yellow instead of white background ? to simulate a lantern
  ;; set the fog of war
  (let ((view (player-view player))
	(pos  (character-pos player)))
    (grid-for-each (lambda (pos)
		     (if (eq? (grid-ref view pos) 'visible)
			 (grid-set! view pos 'visited)))
		   view)

    ;; field of vision using shadow casting (spiral path FOV)
    ;; see http://roguebasin.roguelikedevelopment.org/index.php?title=Spiral_Path_FOV
    (let* ((g     (player-map player))
	   (x     (point-x    pos))
	   (y     (point-y    pos)))
      (let loop ((queue (list pos)))
	(define (pass-light pos new)
	  ;; enqueue cells depending on the orientation of new from pos
	  (let* ((pos-x (point-x pos)) (pos-y (point-y pos))
		 (new-x (point-x new)) (new-y (point-y new))
		 (dirs  (four-directions new))
		 (north (list-ref dirs 0))
		 (south (list-ref dirs 1))
		 (west  (list-ref dirs 2))
		 (east  (list-ref dirs 3)))
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
	      (if (and (inside-grid? view new)
		       (not (eq? (grid-ref view new)
				 'visible)) ; already seen
		       (<= (distance pos new) 7) ; within range ;; TODO have range in a variable, maybe a player trait (elves see farther?)
		       ;; do we have line of sight ? helps restrict the
		       ;; visibility down to a reasonable level
		       ;; note: line of sight is not necessary to see walls,
		       ;; this gives better results
		       (or (opaque-cell? (grid-ref g new) #f)
			   (line-of-sight? g pos new)))
		  (begin (grid-set! view new 'visible) ; mark as lit
			 (if (not (opaque-cell? (grid-ref g new) #f))
			     (loop (append (cdr queue)
					   (pass-light pos new))))))
	      (loop (cdr queue)))))

      ;; one last pass to solve the problem case of walls that are hard to
      ;; see, which gives ugly results
      ;; to solve the problem, any wall next to a visible square is visible
      (grid-for-each
       (lambda (pos)
	 (if (and (opaque-cell? (grid-ref g pos) #f)
		  (eq? (grid-ref view pos) 'unknown)
		  (fold
		   (lambda (acc new)
		     (or acc
			 (and (not (opaque-cell? (grid-ref-check g new) #f))
			      (eq? (grid-ref-check view new) 'visible))))
		   #f (eight-directions pos)))
	     (grid-set! view pos 'visited)))
       view))))


;; commands
(define (invalid-command) (display "Invalid command.\n"))

(define (which-direction?)
  (if (not (eq? (read-char) #\[))
      (invalid-command))
  (case (read-char)
    ((#\A) 'up)
    ((#\B) 'down)
    ((#\C) 'right)
    ((#\D) 'left)
    (else  (invalid-command))))

(define (read-command) ;; TODO define all this inside a macro, so that a description can be included with the commands ? or keep 2 separate lists ? or just a lookup list of commands, functions, and doc ? yeah, probably that last one, BUT how to have entries for the movement arrows ?
  (let* ((pos   (copy-point (character-pos player)))
	 (grid  (floor-map (character-floor player)))
	 (x     (point-x pos))
	 (y     (point-y pos))
	 (char  (read-char)))

    (clear-to-bottom)

    (case char
      ;; movement
      ((#\esc) (case (which-direction?)
		 ((up)    (point-x-set! pos (- x 1)))
		 ((down)  (point-x-set! pos (+ x 1)))
		 ((right) (point-y-set! pos (+ y 1)))
		 ((left)  (point-y-set! pos (- y 1))))
       ;; tries to move to the new position, if it fails, stay where we were
       (move grid player pos))

      ;; inventory
      ((#\p) (pick-up pos))
      ((#\d) (drop))
      ((#\i) (inventory))
      ((#\e) (equip))
      ((#\r) (take-off))
      ((#\D) (cmd-drink))

      ((#\o) (cmd-open))
      ((#\c) (cmd-close))
      ((#\t) (stairs))

      ((#\s) (shoot))

      ;; help
      ((#\?) (show-help))
      ((#\n) (info grid pos))
      ((#\l) (look grid pos))

      ;; debugging
      ((#\k) (kill)) ; insta-kill a monster
      ((#\:) (console))

      ((#\space) (display "Nothing happens.\n")) ; noop
      ((#\q)     (quit))
      (else      (invalid-command)))))

(define (choose-direction)
  (case (read-char)
    ((#\esc) (case (which-direction?)
	       ((up)    up)
	       ((down)  down) ;; TODO for some reason, since I use black hole, just using eval does not work
	       ((left)  left)
	       ((right) right)))
    (else    (invalid-command) #f)))

(define (read-number n) ; read a number up to n, or q to cancel
  (let loop ((nb (read-char)))
    (cond ((eq? nb #\q)
	   #f) ; cancel
	  ((not (and (char>=? nb #\1)
		     (<= (- (char->integer nb) (char->integer #\0)) n)))
	   (loop (read-char)))
	  (else
	   (- (char->integer nb)
	      (char->integer #\0) 1)))))

(define (choice objects f null-message question feedback)
  (if (null? objects)
      (display (string-append null-message "\n"))
      (begin   (cursor-home)
	       (clear-to-bottom)
	       (display question)
	       (display "\nq: Cancel\n")
	       (for-each (lambda (o i)
			   (display (string-append
				     (number->string (+ i 1)) ": "
				     (object-info o) "\n")))
			 objects
			 (iota (length objects)))
	       (let ((nb (read-number (length objects))))
		 (if nb
		     (let ((object (list-ref objects nb)))
		       (show-state)
		       (f object)
		       (display (string-append feedback
					       (object-info object)
					       ".\n"))))))))

(define (direction-command name f)
  (clear-to-bottom)
  (display (string-append name " in which direction? "))
  (let ((dir (choose-direction))) ; evaluates to a function, or #f
    (if dir
	(let* ((grid (player-map player))
	       (cell (grid-ref grid (dir (character-pos player)))))
	  (f grid cell player)))))

;; console from which arbitrary expressions can be evaluated
(define (console)
  (tty-mode-set! (current-input-port) #t #t #f #f 0)
  (shell-command "setterm -cursor on")
  (display ": ")
  (display (eval (read)))
  (read-char)
  (shell-command "setterm -cursor off")
  (tty-mode-set! (current-input-port) #t #t #t #f 0))

(define (quit)
  (display "\nHall of fame:\n\n") ;; TODO have in a function
  (let* ((name         (string->symbol (player-name player)))
	 (xp           (player-experience player))
	 (level        (player-level player))
	 (floor-no     (floor-no (character-floor player)))
	 (current-game (list name xp level floor-no)))
    (define (update-hall-of-fame name score level floor)
      (let* ((l   (sort-list (cons (list name score level floor) hall-of-fame)
			     (lambda (x y) (> (cadr x) (cadr y))))) ;; TODO if same score, sort with the other factors
	     (new (if (> (length l) 10) ; we keep the 10 best
		      (remove-at-index l 10)
		      l)))
	(set! hall-of-fame new)
	(with-output-to-file "hall-of-fame"
	  (lambda () (display new)))
	new))
    (let loop ((hall (update-hall-of-fame name xp level floor-no))
	       (highlight? #t))
      (if (not (null? hall))
	  (let ((head (car hall)))
	    (terminal-print
	     (string-append (symbol->string (car    head))
			    ":\t" ;; TODO alignment will be messed up anyways
			    (number->string (cadr   head))
			    "\tlevel "
			    (number->string (caddr  head))
			    "\tfloor "
			    (number->string (cadddr head))
			    "\n")
	     bg: (if (and highlight? (equal? (car hall) current-game))
		     'white
		     'black)
	     fg: (if (and highlight? (equal? (car hall) current-game))
		     'black
		     'white))
	    (loop (cdr hall)
		  (and highlight? (not (equal? (car hall) current-game))))))))
  (display "\n")
  ;; restore tty
  (tty-mode-set! (current-input-port) #t #t #f #f 0)
  (shell-command "setterm -cursor on")
  (exit))


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

(define-method (attack (attacker player) defender)
  (display (string-append (character-name attacker)
			  " attacks the "
			  (character-name defender)))
  (check-if-hit attacker defender))

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
			  (floor-monsters (character-floor player))))
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
		   (+ (floor-no (character-floor player)) 1))
		  "\n"))
	(show-grid grid
		   print-fun: (visibility-printer (player-view player)
						  (player-map  player)))
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


(define-method (damage (attacker player) (defender monster))
  (let ((dmg (max (get-damage attacker) 1))) ;; TODO could deal 0 damage ?
    (display (string-append " and deals " (number->string dmg) ;; TODO copied from character.scm, the fallback method, use call-next-method ? (but would mess up with the .\n at the end)
			    " damage"))
    (character-hp-set! defender (- (character-hp defender) dmg))
    (if (<= (character-hp defender) 0)
	(remove-monster defender)
	(display ".\n"))))

;; TODO not all that clean to have it here, but it's the only place where it would not lead to circular dependencies
;; removes a monster, usually when killed
(define (remove-monster monster)
  (display (string-append ", which kills the "
			  (character-name monster)
			  ".\n"))
  (let* ((floor (character-floor monster))
	 (cell  (grid-ref (floor-map floor) (character-pos monster))))
    ;; drop equipment TODO maybe only drop each part with a certain probability, to simulate breaking during combat
    (for-each-equipped (lambda (obj where)
			 (if (and obj (removable? obj)) (add-object cell obj)))
		       (character-equipment monster))
    ;; remove the monster
    (cell-occupant-set! cell #f)
    (floor-monsters-set! floor (remove monster (floor-monsters floor)))
    ;; give experience
    (let* ((challenge     (character-level monster))
	   (xp-same-level (* challenge 300))
	   (delta-level   (- challenge (character-level player))))
      (add-experience (if (= delta-level 0)
			  xp-same-level
			  (max 0
			       (ceiling (* xp-same-level
					   (+ 1 (* 1/3 delta-level))))))))))

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
