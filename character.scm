(define-type character
  name
  printer
  equipment
  extender: define-type-of-character)

(define-type equipment
  main-arm
  off-arm ; shield or 2nd weapon
  torso) ;; TODO add more
(define (new-equipment #!key (main-arm #f) (off-arm #f) (torso #f))
  (make-equipment main-arm off-arm torso)) ;; TODO when a monster dies, drop its equipment (maybe randomly remove some items, to make as if they broke during combat)


(define (get-armor-class c)
  (let ((e (character-equipment c)))
    (+ 10
       (armor-ac (equipment-torso e))
       (cond ((shield? (equipment-off-hand e))
	      => (lambda (shield) (armor-ac shield)))
	     ((else 0)))))) ;; TODO add more

(define (move g pos new-pos)
  ;; moves the occupant of pos to new-pos, and returns the position of the
  ;; occupant (the new one or the original, if the move fails)
  (if (and (inside-grid? g new-pos)
	   (free-cell?   (grid-get g new-pos)))
      (let* ((cell     (grid-get g pos))
	     (new-cell (grid-get g new-pos))
	     (to-move  (walkable-cell-occupant cell)))
	(occupant-set! cell     #f)
	(occupant-set! new-cell to-move)
	new-pos)
      pos)) ; move failed
