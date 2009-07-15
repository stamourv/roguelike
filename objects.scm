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
(define (new-leather-armor)
  (make-body-armor "leather armor"         10 2 6))
(define (new-studded-leather-armor)
  (make-body-armor "studded leather armor" 25 3 5))

(define-class shield (armor))
(define-method (print (o shield)) #\0)
(define (new-light-shield) (make-shield "light shield" 3 1))


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
(define (new-club)        (make-weapon "club"        1  '(6)  'bludgeoning))
(define (new-morningstar) (make-weapon "morningstar" 8  '(8)  'bludgeoning)) ;; TODO also piercing
(define (new-shortspear)  (make-weapon "shortspear"  1  '(6)  'piercing))

(define-class two-handed-weapon (weapon)) ;; TODO have a different character to display two-handed weapons ? maybe / or \
(define-class off-hand-placeholder (object))
(define (new-off-hand-placeholder)
  (make-off-hand-placeholder "<two-handed weapon>" 0))
(define-method (removable? (o off-hand-placeholder)) #f)
(define (new-greataxe) (make-two-handed-weapon "greataxe" 20 '(12) 'slashing))

(define-class ranged-weapon (two-handed-weapon)) ;; TODO what about slings, darts, shuriken, etc, whice are one handed
(define-method (print (o ranged-weapon)) #\))
(define (new-shortbow) (make-ranged-weapon "shortbow" 30 '(6) 'piercing))


(define-class potion (object) ;; TODO have randomly assigned colors, and show a message about the true nature of the potion when drunk (and maybe even set! the new-X functions to give them their proper name (and rename all in inventory))
  (slot: thunk))
(define-method (print (o potion)) #\;)
(define-generic drink)
(define-method (drink o)          (display "I can't drink that."))
(define-method (drink (o potion)) ((potion-thunk o)))
(define (new-light-healing-potion)
  (make-potion "light healing potion" 50 ;; TODO at this price, is oly seen on the 3rd level. might be nice to see on the second
	       (lambda ()
		 (character-hp-set! player
				    (min (+ (character-hp player) ((dice 8 1))) ;; TODO have this in a "heal" function
					 (character-max-hp player))))))
(define (new-bulls-strength-potion)
  (make-potion "bull's strength potion" 300 ;; TODO at this price, will take a while to see
	       (lambda ()
		 (alter-attr player 'str 4 180)))) ; 3 minutes/level @ level 3
(define (new-cats-grace-potion)
  (make-potion "cat's grace potion" 300
	       (lambda ()
		 (alter-attr player 'dex 4 180))))
(define (new-bears-endurance-potion)
  (make-potion "bear's endurance potion" 300
	       (lambda ()
		 (alter-attr player 'con 4 180)
		 (alter-attr player 'hp  (* (player-level player) 2) 180))))
;; TODO add others for int, wis, cha once they get useful
;; TODO have more potions, so that random colors actually matter

(define-class food (object)
  (slot: thunk))
;; TODO add some, should be cheaper than potions, but should heal less
