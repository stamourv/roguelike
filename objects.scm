(define-type object
  name
  gp-value
  printer
  extender: define-type-of-object)


(define-type-of-object treasure) ;; TODO more
(define (new-treasure) (make-treasure (random-element object-names)
				      0
				      (lambda () #\T)))


(define-type-of-object equipable-object
  extender: define-type-of-equipable-object)

(define-type-of-equipable-object armor
  ac
  extender: define-type-of-armor)

(define-type-of-armor body-armor)
(define (new-body-armor name gp ac) ;; TODO have light, medium and heavy armor TODO have different printers for each type of armor ?
  (make-body-armor name gp (lambda () #\&) ac))
(define (new-leather-armor)
  (new-body-armor "leather armor"         10 2))
(define (new-studded-leather-armor)
  (new-body-armor "studded leather armor" 25 3))

(define-type-of-armor shield)
(define (new-shield name gp ac)
  (make-shield name gp (lambda () #\0) ac))
(define (new-light-shield) (new-shield "light shield" 3 1))

(define-type-of-equipable-object weapon
  damage-fun ; function that returns the damage
  damage-type) ;; TODO maybe have subtypes for 1 and 2 handed (or weapon size), meelee and ranged, ...
(define (new-weapon name gp dmg-fun dmg-type)
  (make-weapon name gp (lambda () #\!) dmg-fun dmg-type))
(define (new-morningstar) (new-weapon "morningstar" 8  (dice 8)  'slashing)) ;; TODO do small versions of items? goblins have small morningstars
(define (new-greataxe)    (new-weapon "greataxe"    20 (dice 12) 'slashing))
