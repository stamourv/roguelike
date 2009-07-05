(define-class character ()
  (slot: name)
  (slot: pos)

  (slot: str) ;; TODO have a profile type to store all that ?
  (slot: dex)
  (slot: con)
  (slot: int)
  (slot: wis)
  (slot: cha)

  (slot: hit-dice)
  (slot: max-hp)
  (slot: hp)
  
  (slot: base-attack-bonus)

  (slot: equipment))

(define (init-hp character #!optional max?)
  (let* ((hd (character-hit-dice character))
	 (hp (max (+ (if max?
			 (foldl + 0 hd)
			 ((apply dice hd)))
		     (get-attribute-bonus 'con character))
		  1)))
    (character-max-hp-set! character hp)
    (character-hp-set!     character hp)))

(define (get-attribute-bonus attr char)
  (quotient (- ((case attr
                  ((str) character-str)
                  ((dex) (lambda (char)
			   (let ((dex (character-dex char))
				 (max (max-dex-bonus
				       (equipment-torso
					(character-equipment char)))))
			     (if max (min (+ 10 (* 2 max)) dex) dex))))
                  ((con) character-con)
                  ((int) character-int)
                  ((wis) character-wis)
                  ((cha) character-cha))
                char)
               10)
            2))

(define (get-melee-attack-bonus  c)
  (+ (character-base-attack-bonus c)
     (get-attribute-bonus 'str c)))
(define (get-ranged-attack-bonus c)
  (+ (character-base-attack-bonus c)
     (get-attribute-bonus 'dex c)))

(define-type equipment
  main-hand
  off-hand ; shield or 2nd weapon ;; TODO no second weapon for now
  torso) ;; TODO add more
(define (new-equipment #!key (main-hand #f) (off-hand #f) (torso #f))
  (make-equipment main-hand off-hand torso))
(define (equipment->list e)
  (list (cons (equipment-main-hand e) "main hand")
        (cons (equipment-off-hand  e) "off hand")
        (cons (equipment-torso     e) "torso")))
(define (for-each-equipped f e)
  (f (equipment-main-hand e) "main hand: ")
  (f (equipment-off-hand  e) "off hand:  ")
  (f (equipment-torso     e) "torso:     "))


(define (get-armor-class c)
  (let ((e (character-equipment c)))
    (+ 10
       (get-attribute-bonus 'dex c)
       (get-ac (equipment-torso    e))
       (get-ac (equipment-off-hand e))))) ;; TODO add more

(define (get-damage c #!optional (add-strength? #t))
  (let ((weapon (equipment-main-hand (character-equipment c))))
    (+ ((get-damage-fun weapon))
       (* (get-attribute-bonus 'str c)
	  (cond ((not add-strength?)         0)
		((two-handed-weapon? weapon) 1.5)
		(else                        1))))))

(define (move g occ new-pos)
  ;; moves the occupant of pos to new-pos, and returns the position of the
  ;; occupant (the new one or the original, if the move fails)
  (if (inside-grid? g new-pos)
      (let ((cell     (grid-ref g (character-pos occ)))
            (new-cell (grid-ref g new-pos)))
        (cond ((free-cell? new-cell)
               (cell-occupant-set! cell     #f)
               (cell-occupant-set! new-cell occ)
               (character-pos-set! occ new-pos))
              ;; walkable, but occupied already, attack whoever is there
              ((walkable-cell? new-cell)
               (attack occ (cell-occupant (grid-ref g new-pos))))))))

(define (attack attacker defender) ;; TODO ranged weapons can currently be used in melee with no penalty
  (if (not (and (monster? attacker)
                (monster? defender))) ; monsters don't attack other monsters
      (let ((roll ((dice 20))))
        (cond ((player? attacker)
               (display (string-append (character-name attacker)
                                       " attacks the "
                                       (character-name defender))))
              ((monster? attacker)
               (display (string-append "The "
                                       (character-name attacker)
                                       " attacks "
                                       (character-name defender)))))
        (if (>= (+ roll (get-melee-attack-bonus attacker))
		(get-armor-class defender))
	    (damage attacker defender)
	    (display " and misses.\n"))))) ;; TODO depending on by how much it missed, say different things

;; returns #t if the opponent dies
(define (damage attacker defender #!optional (melee? #t))
  (let ((dmg (max (get-damage attacker melee?) 1))) ;; TODO could deal 0 damage ?
    (display (string-append " and deals " (number->string dmg)
			    " damage"))
    (character-hp-set! defender (- (character-hp defender) dmg))
    (if (and (<= (character-hp defender) 0) (monster? defender))
	(begin (display (string-append ", which kills the "
				       (character-name defender)
				       ".\n"))
	       (remove-monster defender)
	       #t)
	(begin (display ".\n")
	       #f))))
