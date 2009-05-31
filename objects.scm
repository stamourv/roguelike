(define-type object
  name
  printer
  extender: define-type-of-object)


(define-type-of-object treasure) ;; TODO more
(define (new-treasure) (make-treasure (random-element object-names)
				      (lambda () #\T)))


(define-type-of-object equipable-object
  extender: define-type-of-equipable-object)

(define-type-of-equipable-object armor
  ac
  extender: define-type-of-armor)

(define-type-of-armor body-armor)
(define (new-body-armor name ac) ;; TODO have light, medium and heavy armor TODO have different printers for each type of armor ?
  (make-body-armor name (lambda () #\&) ac))
(define (new-leather-armor)         (new-body-armor "leather armor"         2))
(define (new-studded-leather-armor) (new-body-armor "studded leather armor" 3))

(define-type-of-armor shield)
(define (new-shield name ac)
  (make-shield name (lambda () #\0) ac))
(define (new-light-shield) (new-shield "light shield" 1))

(define-type-of-equipable-object weapon
  damage-fun ; function that returns the damage
  damage-type) ;; TODO maybe have subtypes for 1 and 2 handed (or weapon size), meelee and ranged, ...
(define (new-weapon name dmg-fun dmg-type)
  (make-weapon name (lambda () #\!) dmg-fun dmg-type))
(define (new-morningstar) (new-weapon "morningstar" (dice 8)  'slashing)) ;; TODO do small versions of items? goblins have small morningstars
(define (new-greataxe)    (new-weapon "greataxe"    (dice 12) 'slashing))
