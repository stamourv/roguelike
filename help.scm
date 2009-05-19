(define (show-help) ;; TODO maybe generate commands with a macro and have help text that can be displayed by help, everything would be automatic
  TODO)

(define (info grid pos) ;; TODO show a message about the location, occupant first (unless player), objects then, finally terrain
    (let ((cell (grid-get grid pos)))
      (cond ((let ((occ (get-occupant cell)))
	       (and occ (not (player? occ)) occ))
	     => (lambda (occ) (display (occupant-name occ))))
	    ((get-object cell)
	     => (lambda (obj) (display (object-name obj))))
	    (else
	     (display "Nothing to see here.")))))

(define (look grid pos) ;; TODO have a moveable cursor, and when enter is pressed, display the info of the location, pos is starting position of the cursor, if final cursor position is outside visibility, say I can't see there
  ;; TODO use the choose-direction command to control the cursor
  (shell-command "setterm -cursor on")
  (set-cursor-on-grid grid pos)
  (read-char) ;; TODO implement the rest, and it seems that pressing l then an arrow shows some weird text in the background about terminal options
  (shell-command "setterm -cursor ooff"))
