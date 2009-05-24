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
(define-type-of-armor shield)
(define (new-shield name ac)
  (make-shield name (lambda () #\0) ac))

(define-type-of-equipable-object weapon
  damage-fun ; function that returns the damage
  damage-type) ;; TODO maybe have subtypes for 1 and 2 handed (or weapon size), meelee and ranged, ...
(define (new-weapon name dmg-fun dmg-type)
  (make-weapon name (lambda () #\!) dmg-fun dmg-type))