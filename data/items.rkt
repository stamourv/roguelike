#lang racket

(require racket/require)
(require (multi-in "../utilities" ("random.rkt" "display.rkt"))
         (multi-in "../engine"    ("common.rkt" "items.rkt" "character.rkt")))
(provide (all-defined-out) new-natural-weapon)

(define body-armor-table '())
(define shield-table     '())
(define weapon-table     '())
(define potion-table     '())

;; pseudo-dsl for creating items and register them for treasure generation
(define-syntax-rule (new-item-macro macro-name kind table)
  (define-syntax-rule
    (macro-name constructor name gp rarity args (... ...))
    (begin (define (constructor) (kind name gp rarity args (... ...)))
           (set! table (cons (cons rarity constructor) table)))))
(new-item-macro new-body-armor        make-body-armor        body-armor-table)
(new-item-macro new-shield            make-shield            shield-table)
(new-item-macro new-weapon            make-weapon            weapon-table)
(new-item-macro new-two-handed-weapon make-two-handed-weapon weapon-table)
(new-item-macro new-ranged-weapon     make-ranged-weapon     weapon-table)
(new-item-macro new-potion            make-potion            potion-table)


;; item definitions


;; body-armor: name gp-value rarity AC max-dex-bonus
(new-body-armor new-leather-armor
                "leather armor"         10  0.8 2 6)
(new-body-armor new-studded-leather-armor
                "studded leather armor" 25  0.6 3 5)
(new-body-armor new-scale-mail
                "scale mail"            50  0.4 4 3)
(new-body-armor new-chain-mail
                "chain mail"            150 0.2 5 2)

;; shield: name gp-value rarity AC
(new-shield new-light-shield "light shield" 3  0.8 1) ; wood
(new-shield new-heavy-shield "heavy shield" 20 0.5 2) ; steel


;; weapon: name gp-value rarity damage-dice damage-type
(new-weapon new-club        "club"        1  0.9 '(6)  'bludgeoning)
(new-weapon new-shortspear  "shortspear"  1  0.3 '(6)  'piercing)
(new-weapon new-morningstar "morningstar" 8  0.5 '(8)  'bludgeoning)
(new-weapon new-short-sword "short sword" 10 0.8 '(6)  'slashing)
(new-weapon new-battleaxe   "battleaxe"   10 0.6 '(8)  'slashing)
(new-weapon new-warhammer   "warhammer"   12 0.5 '(8)  'bludgeoning)
(new-weapon new-long-sword  "long sword"  15 0.6 '(8)  'slashing)

(new-two-handed-weapon new-greataxe    "greataxe"    20 0.3 '(12)  'slashing)
(new-two-handed-weapon new-great-sword "great sword" 50 0.3 '(6 6) 'slashing)

(new-ranged-weapon new-shortbow "shortbow" 30 0.75 '(6) 'piercing)
(new-ranged-weapon new-longbow  "longbow"  75 0.6  '(8) 'piercing)


;; potion: name gp-value effect-thunk message-thunk
(new-potion new-light-healing-potion
            "light healing potion" 50 0.8
            (lambda () (heal player ((dice 8 1))))
            (lambda () "You feel healthier.\n"))
(new-potion new-bulls-strength-potion
            "bull's strength potion" 300 0.3
            (lambda ()
              (alter-attr player 'str 4 180)) ; 3 minutes/level @ level 3
            (lambda () "You could lift boulders.\n"))
(new-potion new-cats-grace-potion
            "cat's grace potion" 300 0.3
            (lambda () (alter-attr player 'dex 4 180))
            (lambda () "You feel lighter.\n"))
(new-potion new-bears-endurance-potion
            "bear's endurance potion" 300 0.3
            (lambda ()
              (alter-attr player 'con 4 180)
              (alter-attr player 'hp  (* (character-level player) 2) 180))
            (lambda () "You could run a thousand miles.\n"))
(new-potion new-barkskin-potion
            "potion of barkskin" 300 0.25
            (lambda () (alter-attr player 'natural-ac 2 180))
            (lambda () "Your skin becomes thick and rough.\n"))

;; register potion types, to assign them a color
(for-each (lambda (p) (register-potion-type (item-name ((cdr p)))))
          potion-table)
