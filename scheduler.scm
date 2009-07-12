(define-generic turn)
(define-method (turn (p player))
  (if (<= (character-hp player) 0)
      (begin (display "You die.\n")
	     (quit))
      (begin (update-visibility)
	     (show-state)
	     (read-command)
	     (reschedule player))))
(define-method (turn (m monster))
  (if (and (> (character-hp m) 0)
	   (= (character-floor-no m) (character-floor-no player)))
      (begin ((behavior-fun (monster-behavior m))
	      m (player-floor player) (character-pos player))
	     (reschedule m))))
;; TODO add methods for potions and spells, to dissipate their effect

(define turn-no    0) ; in seconds, reset when the level is changed
;; to preserve the ordering from turn to turn, in case of identical speeds
(define turn-id    0)
(define turn-queue '())
(define (reschedule char)
  (set! turn-queue (cons (list (+ turn-no (character-speed char))
			       turn-id
			       char)
			 turn-queue))
  (set! turn-id (+ turn-id 1)))
(define (find-next-active)
  (let* ((minimum (fold (lambda (acc new) (min acc (car new)))
			(caar turn-queue)
			turn-queue))
	 (next (filter (lambda (x) (= (car x) minimum)) turn-queue))) ;; TODO all these list traversals might be costly
    (set! turn-no minimum)
    (for-each (lambda (x) (set! turn-queue (remove x turn-queue))) next)
    ;; order by turn-id, to preserve ordering in the case of identical speeds
    (map caddr (sort-list next (lambda (x y) (< (cadr x) (cadr y)))))))
