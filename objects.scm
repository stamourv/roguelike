(define-class object ()
  (slot: name)
  (slot: gp-value)
  (slot: printer))

(define-generic object-info)
(define-method (object-info o) (object-name o))


(define-class treasure (object)) ;; TODO more
(define (new-treasure name) (make-treasure name 0 (lambda () #\T)))


(define-class equipable-object (object))


(define-generic get-ac)
(define-method (get-ac o) 0)

(define-class armor (equipable-object)
  (slot: ac))
(define-method (get-ac      (o armor)) (armor-ac o))
(define-method (object-info (o armor))
  (string-append (object-name o)
		 " (ac: "
		 (number->string (armor-ac o))
		 ")"))

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


(define-generic get-damage-fun)
(define-method (get-damage-fun o) (dice 4)) ; unarmed strike

(define-class weapon (equipable-object)
  (slot: damage-dice) ; function that returns the damage
  (slot: damage-type)) ;; TODO maybe have subtypes for 1 and 2 handed (or weapon size), meelee and ranged, ...
(define (new-weapon name gp dmg-fun dmg-type)
  (make-weapon name gp (lambda () #\!) dmg-fun dmg-type))
(define-method (get-damage-fun (o weapon)) (apply dice (weapon-damage-dice o)))
(define-method (object-info (o weapon))
  (string-append (object-name o)
		 " (damage: "
		 (let loop ((l  (weapon-damage-dice o))
			    (s  "")
			    (+? #f))
		   (if (null? l)
		       s
		       (loop (cdr l)
			     (string-append s
					    (if +? " + " "")
					    ; we don't need the Xd of Xd1
					    (if (= (car l) 1) "" "1d") ;; TODO handle the case where we have '(6 6), print 2d6, not 1d6 + 1d6, to do that, sort the list of die (highest first), then count how many consecutive are identical. to have things like 2d6+2, use 2 1 sided die for the +2
					    (number->string (car l)))
			     #t)))
		 " "
		 (symbol->string (weapon-damage-type o))
		 ")"))

;; TODO have small version of items ? (small weapons do less damage) goblins now have clubs instead of small morningstars
(define (new-club)        (new-weapon "club"        1  '(6)  'bludgeoning))
(define (new-greataxe)    (new-weapon "greataxe"    20 '(12) 'slashing))
(define (new-morningstar) (new-weapon "morningstar" 8  '(8)  'bludgeoning)) ;; TODO also piercing
(define (new-shortspear)  (new-weapon "shortspear"  1  '(6)  'piercing))
