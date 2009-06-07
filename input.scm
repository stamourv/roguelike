(define (invalid-command) (display "Invalid command."))

(define (which-direction?)
  (read-char) ; should be [ TODO check
  (case (read-char)
    ((#\A) 'up)
    ((#\B) 'down)
    ((#\C) 'right)
    ((#\D) 'left)
    (else  (invalid-command))))

(define (read-command player) ;; TODO define all this inside a macro, so that a description can be included with the commands ? or keep 2 separate lists ? or just a lookup list of commands, functions, and doc ? yeah, probably that last one, BUT how to have entries for the movement arrows ?
  (let* ((pos   (copy-point (player-pos player)))
	 (grid  (player-map player))
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
       (move (player-map player) player pos))

      ;; inventory
      ((#\p) (pick-up   player pos))
      ((#\d) (drop      player))
      ((#\i) (inventory player))
      ((#\e) (equip     player))
      ((#\r) (take-off  player))

      ((#\o) (open   player))
      ((#\c) (close  player))
      ((#\t) (stairs player))

      ;; help
      ((#\?) (show-help))
      ((#\n) (info grid pos))
      ((#\l) (look grid pos))

      ;; debugging
      ((#\k) (kill player)) ; insta-kill a monster TODO replace with a combat system

      ((#\space) (display "Nothing happened.")) ; noop TODO most roguelikes use .
      ((#\q)     (quit))
      (else      (invalid-command)))))

(define (choose-direction)
  (case (read-char)
    ((#\esc) (which-direction?))
    (else    (invalid-command) #f)))

(define (choice player objects f null-message question feedback)
  (if (null? objects)
      (display null-message)
      (begin   (cursor-home)
	       (clear-to-bottom)
	       (display question)
	       (display "\nq: Cancel\n")
	       (let loop ((objects objects)
			  (i       1))
		 (if (not (null? objects))
		     (begin (display (string-append
				      (number->string i) ": "
				      (object-name (car objects)) "\n"))
			    (loop (cdr objects) (+ i 1)))))
	       (let loop ((nb (read-char)))
		 (cond ((eq? nb #\q)
			#f) ; cancel
		       ((not (and (char>=? nb #\1)
				  (<= (- (char->integer nb) (char->integer #\0))
				      (length objects))))
			(loop (read-char)))
		       (else
			(let ((object (list-ref objects
						(- (char->integer nb)
						   (char->integer #\0) 1))))
			  (show-state player)
			  (f object)
			  (display (string-append feedback
						  (object-name object))))))))))
