(define (invalid-command) (display "Invalid command."))

(define (which-direction?)
  (read-char) ; should be [ TODO check
  (case (read-char)
    ((#\A) 'up)
    ((#\B) 'down)
    ((#\C) 'right)
    ((#\D) 'left)
    (else  (invalid-command))))

(define (read-command) ;; TODO define all this inside a macro, so that a description can be included with the commands ? or keep 2 separate lists ? or just a lookup list of commands, functions, and doc ? yeah, probably that last one, BUT how to have entries for the movement arrows ?
  (let* ((pos   (copy-point (character-pos player)))
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
      ((#\p) (pick-up pos))
      ((#\d) (drop))
      ((#\i) (inventory))
      ((#\e) (equip))
      ((#\r) (take-off))

      ((#\o) (cmd-open))
      ((#\c) (cmd-close))
      ((#\t) (stairs))

      ;; help
      ((#\?) (show-help))
      ((#\n) (info grid pos))
      ((#\l) (look grid pos))

      ;; debugging
      ((#\k) (kill)) ; insta-kill a monster
      ((#\:) (console))

      ((#\space) (display "Nothing happened.\n")) ; noop TODO most roguelikes use .
      ((#\q)     (quit))
      (else      (invalid-command)))))

(define (choose-direction)
  (case (read-char)
    ((#\esc) (which-direction?))
    (else    (invalid-command) #f)))

(define (choice objects f null-message question feedback)
  (if (null? objects)
      (display (string-append null-message "\n"))
      (begin   (cursor-home)
	       (clear-to-bottom)
	       (display question)
	       (display "\nq: Cancel\n")
	       (let loop ((objects objects)
			  (i       1))
		 (if (not (null? objects))
		     (begin (display (string-append
				      (number->string i) ": "
				      (object-info (car objects)) "\n"))
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
			  (show-state)
			  (f object)
			  (display (string-append feedback
						  (object-name object)
						  ".\n")))))))))

;; console from which arbitrary expressions can be evaluated
(define (console)
  (tty-mode-set! (current-input-port) #t #t #f #f 0)
  (shell-command "setterm -cursor on")
  (display ": ")
  (display (eval (read)))
  (read-char)
  (shell-command "setterm -cursor off")
  (tty-mode-set! (current-input-port) #t #t #t #f 0))