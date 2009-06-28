(define-class object ()
  (slot: name)
  (slot: gp-value)
  (slot: printer))


(define-class treasure (object)) ;; TODO more
(define (new-treasure name) (make-treasure name 0 (lambda () #\T)))


(define-class equipable-object (object))

(define-class armor (equipable-object)
  (slot: ac))

(define-class body-armor (armor))
(define (new-body-armor name gp ac) ;; TODO have light, medium and heavy armor TODO have different printers for each type of armor ?
  (make-body-armor name gp (lambda () #\&) ac))
(define (new-leather-armor)
  (new-body-armor "leather armor"         10 2))
(define (new-studded-leather-armor)
  (new-body-armor "studded leather armor" 25 3))

(define-class shield (armor))
(define (new-shield name gp ac)
  (make-shield name gp (lambda () #\0) ac))
(define (new-light-shield) (new-shield "light shield" 3 1))

(define-class weapon (equipable-object)
  (slot: damage-fun) ; function that returns the damage
  (slot: damage-type)) ;; TODO maybe have subtypes for 1 and 2 handed (or weapon size), meelee and ranged, ...
(define (new-weapon name gp dmg-fun dmg-type)
  (make-weapon name gp (lambda () #\!) dmg-fun dmg-type))
;; TODO have small version of items ? (small weapons do less damage) goblins now have clubs instead of small morningstars
(define (new-club)        (new-weapon "club"        1  (dice 6)  'bludgeoning))
(define (new-greataxe)    (new-weapon "greataxe"    20 (dice 12) 'slashing))
(define (new-morningstar) (new-weapon "morningstar" 8  (dice 8)  'bludgeoning)) ;; TODO also piercing
(define (new-shortspear)  (new-weapon "shortspear"  1  (dice 6)  'piercing))
