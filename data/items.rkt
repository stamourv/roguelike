#lang racket

(require "../utilities/random.rkt"
         "../utilities/display.rkt")
(require "../engine/common.rkt"
         "../engine/items.rkt"
         "../engine/character.rkt")
(provide (all-defined-out))


;; body-armor: name gp-value rarity AC max-dex-bonus
(define (new-leather-armor)
  (make-body-armor "leather armor"         10  0.8 2 6))
(define (new-studded-leather-armor)
  (make-body-armor "studded leather armor" 25  0.6 3 5))
(define (new-scale-mail)
  (make-body-armor "scale mail"            50  0.4 4 3))
(define (new-chain-mail)
  (make-body-armor "chain mail"            150 0.2 5 2))

;; shield: name gp-value rarity AC
(define (new-light-shield) (make-shield "light shield" 3  0.8 1)) ; wood
(define (new-heavy-shield) (make-shield "heavy shield" 20 0.5 2)) ; steel


;; weapon: name gp-value rarity damage-dice damage-type
(define (new-club)
  (make-weapon "club"        1  0.9 '(6)  'bludgeoning))
(define (new-shortspear)
  (make-weapon "shortspear"  1  0.3'(6)  'piercing))
;; TODO also piercing
(define (new-morningstar)
  (make-weapon "morningstar" 8  0.5 '(8)  'bludgeoning))
(define (new-short-sword)
  (make-weapon "short sword" 10 0.8 '(6)  'slashing))
(define (new-battleaxe)
  (make-weapon "battleaxe"   10 0.6 '(8)  'slashing))
(define (new-warhammer)
  (make-weapon "warhammer"   12 0.5 '(8)  'bludgeoning))
(define (new-long-sword)
  (make-weapon "long sword"  15 0.6 '(8)  'slashing))

(define (new-greataxe)
  (make-two-handed-weapon "greataxe"    20 0.3 '(12)  'slashing))
(define (new-great-sword)
  (make-two-handed-weapon "great sword" 50 0.3 '(6 6) 'slashing))

(define (new-shortbow) (make-ranged-weapon "shortbow" 30 0.75 '(6) 'piercing))
(define (new-longbow)  (make-ranged-weapon "longbow"  75 0.6  '(8) 'piercing))


;; potion: name gp-value effect-thunk message-thunk
(define (new-light-healing-potion)
  (make-potion "light healing potion" 50 0.8
               ;; TODO at this price, is oly seen on the 3rd level. might be
               ;;  nice to see on the second
	       (lambda ()
		 (set-character-hp! player
				    (min (+ (character-hp player) ((dice 8 1)))
                                         ;; TODO have this in a "heal" function
					 (character-max-hp player))))
	       (lambda ()
                 (if (= (character-hp player) (character-max-hp player))
                     "You feel nothing.\n"
                     ;; TODO also, don't identify the potion kind in that case
                     "You feel healthier.\n"))))
(define (new-bulls-strength-potion)
  (make-potion "bull's strength potion" 300 0.3
               ;; TODO at this price, will take a while to see
	       (lambda ()
		 (alter-attr player 'str 4 180)) ; 3 minutes/level @ level 3
	       (lambda ()
                 "You could lift boulders.\n")))
(define (new-cats-grace-potion)
  (make-potion "cat's grace potion" 300 0.3
	       (lambda ()
		 (alter-attr player 'dex 4 180))
	       (lambda ()
                 "You feel lighter.\n")))
(define (new-bears-endurance-potion)
  (make-potion "bear's endurance potion" 300 0.3
	       (lambda ()
		 (alter-attr player 'con 4 180)
		 (alter-attr player 'hp  (* (character-level player) 2) 180))
	       (lambda ()
                 "You could run a thousand miles.\n")))
;; TODO add others for int, wis, cha once they get useful
(define (new-barkskin-potion)
  (make-potion "potion of barkskin" 300 0.25
	       (lambda ()
		 (alter-attr player 'natural-ac 2 180))
	       (lambda ()
                 "Your skin becomes thick and rough.\n")))
;; TODO have some bad potions (with the same price as the good ones, to avoid
;;  running only into bad potions early on) to make it riskier


;; contains the probability of each kind of item, and the probability of each
;; item within each category
(define (gen i) (cons (item-rarity (i)) i))
(define treasure-table
  (normalize-probability-table
   `((0.43
      ;; weapons
      ,@(map gen (list new-club
                       new-shortspear
                       new-morningstar
                       new-short-sword
                       new-battleaxe
                       new-warhammer
                       new-long-sword
                       new-greataxe
                       new-great-sword
                       new-shortbow
                       new-longbow)))
     (0.25
      ;; shields
      ,@(map gen (list new-light-shield new-heavy-shield)))
     (0.22
      ;; body armor
      ,@(map gen (list new-leather-armor
                       new-studded-leather-armor
                       new-scale-mail
                       new-chain-mail)))
     (0.1
      ;; potions
      ,@(map gen (list new-light-healing-potion
                       new-bulls-strength-potion
                       new-cats-grace-potion
                       new-bears-endurance-potion
                       new-barkskin-potion))))))
