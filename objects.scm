(define-class object ()
  (slot: name)
  (slot: gp-value))

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

(define-class body-armor (armor)) ;; TODO have light, medium and heavy armor TODO have different display characters for each type of armor ?
(define-method (print (o body-armor)) #\&)
(define (new-leather-armor)
  (make-body-armor "leather armor"         10 2))
(define (new-studded-leather-armor)
  (make-body-armor "studded leather armor" 25 3))

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
		 (let loop ((l  (weapon-damage-dice o))
			    (s  "")
			    (+? #f))
		   (if (null? l)
		       s
		       (loop (cdr l)
			     (string-append s
					    (if +? " + " "")
					    ; we don't need the Xd of Xd1
					    (if (= (car l) 1) "" "1d") ;; TODO handle the case where we have '(6 6), print 2d6, not 1d6 + 1d6, to do that, sort the list of dice (highest first), then count how many consecutive are identical. to have things like 2d6+2, use 2 1 sided dice for the +2
					    (number->string (car l)))
			     #t)))
		 " "
		 (symbol->string (weapon-damage-type o))
		 ")"))
;; TODO have small version of items ? (small weapons do less damage) goblins now have clubs instead of small morningstars
(define (new-club)        (make-weapon "club"        1  '(6)  'bludgeoning))
(define (new-greataxe)    (make-weapon "greataxe"    20 '(12) 'slashing))
(define (new-morningstar) (make-weapon "morningstar" 8  '(8)  'bludgeoning)) ;; TODO also piercing
(define (new-shortspear)  (make-weapon "shortspear"  1  '(6)  'piercing))
