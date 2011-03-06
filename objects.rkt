#lang racket

(require "utilities/utilities.rkt"
         "utilities/class.rkt"
         "utilities/display.rkt")
(provide (all-defined-out))

(define-class <object> () name gp-value)

(define-generic object-info)
(define-method (object-info o) (object-name o))

(define-generic removable?)
(define-method (removable? o) #t)

(define-class <equipable-object> (object))


(define-generic get-ac)
(define-method (get-ac o) 0)
(define-generic max-dex-bonus)
(define-method (max-dex-bonus o) #f)

(define-class <armor> (equipable-object) ac)
(define-method (get-ac      (o struct:armor)) (armor-ac o))
(define-method (object-info (o struct:armor))
  (format "~a (ac: ~a)" (object-name o) (armor-ac o)))

(define-class <body-armor> (armor) max-dex-bonus)
;; TODO have light, medium and heavy armor and have different display
;;  characters for each type of armor ?
(define-method (max-dex-bonus (o struct:body-armor))
  (body-armor-max-dex-bonus o))
(define-method (show (o struct:body-armor)) #\&)

(define-class <shield> (armor))
(define-method (show (o struct:shield)) #\0)


(define-generic get-damage-fun)
(define-method (get-damage-fun o) (dice 4)) ; unarmed strike

(define-class <weapon> (equipable-object)
  damage-dice ; function that returns the damage
  damage-type)
;; TODO maybe have subtypes for 1 and 2 handed (or weapon size), meelee
;; and ranged, ...
(define-method (show (o struct:weapon)) #\!)
(define-method (get-damage-fun (o struct:weapon))
  (apply dice (weapon-damage-dice o)))
(define-method (object-info (o struct:weapon))
  (format "~a (damage: ~a ~a)"
          (object-name o) (show-dice (weapon-damage-dice o))
          (symbol->string (weapon-damage-type o))))
;; TODO have small version of items ? (small weapons do less damage) goblins
;;  now have clubs instead of small morningstars

(define-class <two-handed-weapon> (weapon))
;; TODO have a different character to display two-handed weapons ?
;;  maybe / or \
(define-class <off-hand-placeholder> (object))
(define (new-off-hand-placeholder)
  (make-off-hand-placeholder "<two-handed weapon>" 0))
(define-method (removable? (o struct:off-hand-placeholder)) #f)

(define-class <ranged-weapon> (two-handed-weapon))
;; TODO what about slings, darts, shuriken, etc, whice are one handed
(define-method (show (o struct:ranged-weapon)) #\))


(define-class <potion> (object) thunk message)
(define-method (show (o struct:potion)) #\;)

;; important: make sure there is at least as many colors as potion types
(define potion-colors
  (let ((types
         ;; TODO keep this up to date, maybe have a macro that generates the
         ;;  potion type, and adds it to the list ?
	 '("light healing potion"
	   "bull's strength potion"
	   "cat's grace potion"
	   "bear's endurance potion"
	   "potion of barkskin"))
	(colors
	 '("red" "blue" "green" "yellow" "white" "black" "pink" "teal" "purple"
	   "brown" "amber" "grey" "silver" "beige" "cloudy" "shimmering" "gold"
	   "milky")))
    (map cons types (take (shuffle colors) (length types)))))
;; potion types that have been identified ;; TODO put this with the player ?
(define identified-potions '())

(define-method (object-info (o struct:potion))
  (let ((name (object-name o)))
    (cond ((member name identified-potions) name)
	  ;; unindentified, show the color
	  (else (format "~a potion" (cdr (assoc name potion-colors)))))))

(define-generic drink)
(define-method (drink o)          (display "I can't drink that."))
(define-method (drink (o struct:potion))
  ((potion-thunk o))
  (display (potion-message o))
  (let ((name (object-name o)))
    (when (not (member name identified-potions))
      (set! identified-potions (cons name identified-potions)))))


(define-class <food> (object) thunk)
;; TODO add some, should be cheaper than potions, but should heal less
