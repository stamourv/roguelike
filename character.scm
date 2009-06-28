(define-class character ()
  (slot: name)
  (slot: printer)
  (slot: pos)

  (slot: str) ;; TODO have a profile type to store all that ?
  (slot: dex)
  (slot: con)
  (slot: int)
  (slot: wis)
  (slot: cha)
  
  (slot: equipment))

(define-type equipment
  main-arm
  off-arm ; shield or 2nd weapon
  torso) ;; TODO add more
(define (new-equipment #!key (main-arm #f) (off-arm #f) (torso #f))
  (make-equipment main-arm off-arm torso))
(define (equipment->list e)
  (list (cons (equipment-main-arm e) "main arm")
	(cons (equipment-off-arm  e) "off arm")
	(cons (equipment-torso    e) "torso")))
(define (for-each-equipped f e)
  (f (equipment-main-arm e) "main arm")
  (f (equipment-off-arm  e) "off arm")
  (f (equipment-torso    e) "torso"))


(define (get-armor-class c)
  (let ((e (character-equipment c)))
    (+ 10
       (armor-ac (equipment-torso e))
       (cond ((shield? (equipment-off-hand e))
	      => (lambda (shield) (armor-ac shield)))
	     ((else 0)))))) ;; TODO add more

(define (move g occ new-pos)
  ;; moves the occupant of pos to new-pos, and returns the position of the
  ;; occupant (the new one or the original, if the move fails)
  (if (inside-grid? g new-pos)
      (let ((cell     (grid-get g (character-pos occ)))
	    (new-cell (grid-get g new-pos)))
	(cond ((free-cell? new-cell)
	       (cell-occupant-set! cell     #f)
	       (cell-occupant-set! new-cell occ)
	       (character-pos-set! occ new-pos))
	      ;; walkable, but occupied already, attack whoever is there
	      ((walkable-cell? new-cell)
	       (attack occ (cell-occupant (grid-get g new-pos))))))))

(define (attack attacker defender) ;; TODO have a true combat system
  (cond ((player? attacker)
	 (display (string-append (character-name attacker)
				 " killed the "
				 (character-name defender)
				 ".\n"))
	 (remove-monster (player-floor attacker) defender attacker))
	((and (monster? attacker)
	      (not (monster? defender))) ; monster don't attack other monsters
	 (display (string-append "The "
				 (character-name attacker)
				 " attacked "
				 (character-name defender)
				 ".\n")))))
