(import objects)
(import character)
(import display)
(import common)
(import utilities)

;; TODO have generation probabilities here, not separately

(define (new-leather-armor)
  (make-body-armor "leather armor"         10 2 6))
(define (new-studded-leather-armor)
  (make-body-armor "studded leather armor" 25 3 5))

(define (new-light-shield) (make-shield "light shield" 3 1))


(define (new-club)        (make-weapon "club"        1  '(6)  'bludgeoning))
(define (new-morningstar) (make-weapon "morningstar" 8  '(8)  'bludgeoning)) ;; TODO also piercing
(define (new-shortspear)  (make-weapon "shortspear"  1  '(6)  'piercing))

(define (new-greataxe) (make-two-handed-weapon "greataxe" 20 '(12) 'slashing))

(define (new-shortbow) (make-ranged-weapon "shortbow" 30 '(6) 'piercing))


(define (new-light-healing-potion)
  (make-potion "light healing potion" 50 ;; TODO at this price, is oly seen on the 3rd level. might be nice to see on the second
	       (lambda ()
		 (character-hp-set! player
				    (min (+ (character-hp player) ((dice 8 1))) ;; TODO have this in a "heal" function
					 (character-max-hp player))))
	       "You feel healthier.\n"))
(define (new-bulls-strength-potion)
  (make-potion "bull's strength potion" 300 ;; TODO at this price, will take a while to see
	       (lambda ()
		 (alter-attr player 'str 4 180)) ; 3 minutes/level @ level 3
	       "You could lift boulders.\n"))
(define (new-cats-grace-potion)
  (make-potion "cat's grace potion" 300
	       (lambda ()
		 (alter-attr player 'dex 4 180))
	       "You feel lighter.\n"))
(define (new-bears-endurance-potion)
  (make-potion "bear's endurance potion" 300
	       (lambda ()
		 (alter-attr player 'con 4 180)
		 (alter-attr player 'hp  (* (player-level player) 2) 180))
	       "You could run a thousand miles.\n"))
;; TODO add others for int, wis, cha once they get useful
(define (new-barkskin-potion)
  (make-potion "potion of barkskin" 300
	       (lambda ()
		 (alter-attr player 'natural-ac 2 180))
	       "Your skin becomes thick and rough.\n"))
;; TODO have some bad potions (with the same price as the good ones, to avoid running only into bad potions early on) to make it riskier
