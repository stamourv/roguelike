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

  (slot: hp) ;; TODO have hp and max-hp
  (slot: base-attack-bonus)

  (slot: equipment))

(define (get-attribute-bonus attr char)
  (quotient (- ((case attr
                  ((str) character-str)
                  ((dex) character-dex)
                  ((con) character-con)
                  ((int) character-int)
                  ((wis) character-wis)
                  ((cha) character-cha))
                char)
               10)
            2))

(define (get-attack-bonus c) ;; TODO also have it for ranged
  (+ (character-base-attack-bonus c)
     (get-attribute-bonus 'str c)))

(define-type equipment
  main-arm
  off-arm ; shield or 2nd weapon ;; TODO no second weapon for now ;; TODO say hand instead of arm ?
  torso) ;; TODO add more
(define (new-equipment #!key (main-arm #f) (off-arm #f) (torso #f))
  (make-equipment main-arm off-arm torso))
(define (equipment->list e)
  (list (cons (equipment-main-arm e) "main arm")
        (cons (equipment-off-arm  e) "off arm")
        (cons (equipment-torso    e) "torso")))
(define (for-each-equipped f e)
  (f (equipment-main-arm e) "main arm: ")
  (f (equipment-off-arm  e) "off arm:  ")
  (f (equipment-torso    e) "torso:    "))


(define (get-armor-class c)
  (let ((e (character-equipment c)))
    (+ 10
       (get-attribute-bonus 'dex c) ;; TODO only up to what the armor permits
       (get-ac (equipment-torso   e))
       (get-ac (equipment-off-arm e))))) ;; TODO add more

(define (get-damage c)
  (+ ((get-damage-fun (equipment-main-arm (character-equipment c))))
     (get-attribute-bonus 'str c)))

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
  (if (not (and (monster? attacker)
                (monster? defender))) ; monsters don't attack other monsters
      (let ((roll ((dice 20))))
        (cond ((player? attacker)
               (display (string-append (character-name attacker)
                                       " attacked the "
                                       (character-name defender))))
              ((monster? attacker)
               (display (string-append "The "
                                       (character-name attacker)
                                       " attacks " ;; TODO have everything either in the past tense or in the present tense
                                       (character-name defender)))))
        (if (>= (+ roll (get-attack-bonus attacker))
		(get-armor-class defender))
	    (let* ((dmg     (max (get-damage attacker) 1)) ;; TODO could deal 0 damage ?
		   (killed? (damage attacker defender dmg)))
	      (display (string-append " and deals " (number->string dmg)
				      " damage"))
	      (if killed?
		  (display (string-append ", which kills the " ;; TODO different message if the player dies
					  (character-name defender))))
	      (display ".\n"))
	    (display " and missed.\n"))))) ;; TODO depending on by how much it missed, say different things

(define (damage attacker defender n) ; returns #t if the opponent dies
  (character-hp-set! defender (- (character-hp defender) n))
  (if (and (<= (character-hp defender) 0) (monster? defender)) ;; TODO the player cannot die for the moment
      (begin (remove-monster defender)
	     #t)
      #f))
