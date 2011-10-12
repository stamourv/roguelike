#lang racket

(require racket/require)
(require (multi-in "../utilities" ("random.rkt" "class.rkt" "descriptions.rkt")))
(provide (all-defined-out))

;; rarity is in [0,1] with 1 being most likely
(define-class <item> () name gp-value rarity)

(define-generic item-info)
(define-method (item-info o) (item-name o))

(define-generic removable?)
(define-method (removable? o) #t)

(define-class <equipable-item> (item))


(define-generic get-ac)
(define-method (get-ac o) 0)
(define-generic max-dex-bonus)
(define-method (max-dex-bonus o) #f)

(define-class <armor> (equipable-item) ac)
(define-method (get-ac      (o struct:armor)) (armor-ac o))
(define-method (item-info (o struct:armor))
  (format "~a (ac: ~a)" (item-name o) (armor-ac o)))

(define-class <body-armor> (armor) max-dex-bonus)
(define-method (max-dex-bonus (o struct:body-armor))
  (body-armor-max-dex-bonus o))
(add-show-method struct:body-armor 'item #\& "A piece of body armor.")

(define-class <shield> (armor))
(add-show-method struct:shield 'item #\0 "A shield.")


(define-generic get-damage-fun)
(define-method (get-damage-fun o) (dice 4)) ; unarmed strike

(define-class <weapon> (equipable-item)
  damage-dice ; function that returns the damage
  damage-type)
(add-show-method struct:weapon 'item #\! "A one-handed melee weapon.")
(define-method (get-damage-fun (o struct:weapon))
  (apply dice (weapon-damage-dice o)))
(define-method (item-info (o struct:weapon))
  (format "~a (damage: ~a ~a)"
          (item-name o) (show-dice (weapon-damage-dice o))
          (symbol->string (weapon-damage-type o))))

(define-class <two-handed-weapon> (weapon))
(add-show-method struct:two-handed-weapon 'item #\/
                 "A two-handed melee weapon.")
(define-class <off-hand-placeholder> (item))
(define (new-off-hand-placeholder)
  (make-off-hand-placeholder "<two-handed weapon>" 0 0))
(define-method (removable? (o struct:off-hand-placeholder)) #f)

(define-class <ranged-weapon> (two-handed-weapon))
(add-show-method struct:ranged-weapon 'item #\) "A ranged weapon.")

(define-class <natural-weapon> (weapon))
(define (new-natural-weapon damage-dice damage-type)
  (make-natural-weapon "<natural weapon>" 0 0 damage-dice damage-type))
(define-method (removable? (o struct:natural-weapon)) #f)


(define-class <potion> (item) thunk message)
(add-show-method struct:potion 'item #\; "A potion.")

;; important: make sure there is at least as many colors as potion types
(define all-colors
  (shuffle
   '("red" "blue" "green" "yellow" "white" "black" "pink" "teal" "purple"
     "brown" "amber" "grey" "silver" "beige" "cloudy" "shimmering" "gold"
     "milky")))
(define potion-colors '()) ; filled up as potion types are defined
(define (register-potion-type name)
  (set! potion-colors (cons (cons name (car all-colors)) potion-colors))
  (set! all-colors (cdr all-colors)))
;; potion types that have been identified
(define identified-potions '())

(define-method (item-info (o struct:potion))
  (let ((name (item-name o)))
    (cond ((member name identified-potions) name)
	  ;; unindentified, show the color
	  (else (format "~a potion" (cdr (assoc name potion-colors)))))))

(define-generic drink)
(define-method (drink o) (display "I can't drink that."))
(define-method (drink (o struct:potion))
  ((potion-thunk o))
  (display ((potion-message o)))
  (let ((name (item-name o)))
    (when (not (member name identified-potions))
      (set! identified-potions (cons name identified-potions)))))


(define-class <food> (item) thunk)
