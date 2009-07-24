(import (path class))
(import common)
(import utilities)

(define-class object ()
  (slot: name)
  (slot: gp-value))

(define-generic object-info)
(define-method (object-info o) (object-name o))

(define-generic removable?)
(define-method (removable? o) #t)

(define-class equipable-object (object))


(define-generic get-ac)
(define-method (get-ac o) 0)
(define-generic max-dex-bonus)
(define-method (max-dex-bonus o) #f)

(define-class armor (equipable-object)
  (slot: ac))
(define-method (get-ac      (o armor)) (armor-ac o))
(define-method (object-info (o armor))
  (string-append (object-name o)
		 " (ac: "
		 (number->string (armor-ac o))
		 ")"))

(define-class body-armor (armor)
  (slot: max-dex-bonus)) ;; TODO have light, medium and heavy armor and have different display characters for each type of armor ?
(define-method (max-dex-bonus (o body-armor)) (body-armor-max-dex-bonus o))
(define-method (print (o body-armor)) #\&)

(define-class shield (armor))
(define-method (print (o shield)) #\0)


(define-generic get-damage-fun)
(define-method (get-damage-fun o) (dice 4)) ; unarmed strike

(define-class weapon (equipable-object)
  (slot: damage-dice) ; function that returns the damage
  (slot: damage-type)) ;; TODO maybe have subtypes for 1 and 2 handed (or weapon size), meelee and ranged, ...
(define-method (print (o weapon)) #\!)
(define-method (get-damage-fun (o weapon)) (apply dice (weapon-damage-dice o)))
(define-method (object-info (o weapon))
  (string-append (object-name o)
		 " (damage: "
		 (show-dice (weapon-damage-dice o))
		 " "
		 (symbol->string (weapon-damage-type o))
		 ")"))
;; TODO have small version of items ? (small weapons do less damage) goblins now have clubs instead of small morningstars

(define-class two-handed-weapon (weapon)) ;; TODO have a different character to display two-handed weapons ? maybe / or \
(define-class off-hand-placeholder (object))
(define (new-off-hand-placeholder)
  (make-off-hand-placeholder "<two-handed weapon>" 0))
(define-method (removable? (o off-hand-placeholder)) #f)

(define-class ranged-weapon (two-handed-weapon)) ;; TODO what about slings, darts, shuriken, etc, whice are one handed
(define-method (print (o ranged-weapon)) #\))


(define-class potion (object)
  (slot: thunk)
  (slot: message))
(define-method (print (o potion)) #\;)

;; important: make sure there is at least as many colors as potion types
(define potion-colors
  (let ((types ;; TODO keep this up to date, maybe have a macro that generates the potion type, and adds it to the list ?
	 '("light healing potion"
	   "bull's strength potion"
	   "cat's grace potion"
	   "bear's endurance potion"
	   "potion of barkskin"))
	(colors
	 '("red" "blue" "green" "yellow" "white" "black" "pink" "teal" "purple"
	   "brown" "amber" "grey" "silver" "beige" "cloudy" "shimmering" "gold"
	   "milky")))
    (map cons types (take (randomize-list colors) (length types)))))
;; potion types that have been identified ;; TODO put this with the player ?
(define identified-potions '())

(define-method (object-info (o potion))
  (let ((name (object-name o)))
    (cond ((member name identified-potions) name)
	  ;; unindentified, show the color
	  (else (string-append (cdr (assoc name potion-colors))
			       " potion")))))

(define-generic drink)
(define-method (drink o)          (display "I can't drink that."))
(define-method (drink (o potion))
  ((potion-thunk o))
  (display (potion-message o))
  (let ((name (object-name o)))
    (if (not (member name identified-potions))
	(set! identified-potions (cons name identified-potions)))))


(define-class food (object)
  (slot: thunk))
;; TODO add some, should be cheaper than potions, but should heal less
