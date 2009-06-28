(define (show-help) ;; TODO maybe generate commands with a macro and have help text that can be displayed by help, everything would be automatic
  TODO)

(define (info grid pos) ;; TODO show a message about the location, occupant first (unless player), objects then, finally terrain
    (let ((cell (grid-get grid pos)))
      (cond ((let ((occ (cell-occupant cell)))
	       (and occ (not (player? occ)) occ))
	     => (lambda (occ) (display (character-name occ))))
	    ((get-object cell)
	     => (lambda (obj) (display (object-name obj))))
	    (else
	     (display "Nothing to see here."))))) ;; TODO describe the terrain, have a description for each type, ideally define with the type

(define (look grid pos) ;; TODO have a moveable cursor, and when enter is pressed, display the info of the location, pos is starting position of the cursor, if final cursor position is outside visibility, say I can't see there
  ;; TODO use the choose-direction command to control the cursor
  (shell-command "setterm -cursor on") ;; TODO maybe just be able to look at immediate squares, and just use choose-direction, but we might want to identify something before we get closer (a monster, for example)
  (set-cursor-on-grid grid pos)
  (read-char) ;; TODO implement the rest, and it seems that pressing l then an arrow shows some weird text in the background about terminal options
  (shell-command "setterm -cursor ooff"))
