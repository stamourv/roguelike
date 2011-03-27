#lang racket

(require "../utilities/class.rkt"
         "../utilities/display.rkt")
(require "../engine/character.rkt"
         "../engine/monsters.rkt"
         "../engine/ai.rkt")
(require "items.rkt")

(provide (all-defined-out))

(define-monster goblin (monster) #\g "A goblin."
  (11 13 12 10 9 6) 1/3 (6)
  #:base-attack-bonus 1
  #:equipment (new-equipment
               #:main-hand (new-club)
               #:off-hand  (new-light-shield)
               #:torso     (new-leather-armor))
  (rush-behavior))
(define-monster goblin-archer (goblin)
  (new-sprite #\g #:fg 'magenta) "A goblin archer."
  (11 13 12 10 9 6) 1/2 (4)
  #:base-attack-bonus 1
  #:equipment (new-equipment
               #:main-hand (new-shortbow))
  (ranged-behavior))

(define-monster kobold (monster) #\k "A kobold."
  (9 13 10 10 9 8) 1/4 (4)
  #:base-attack-bonus 1
  #:equipment (new-equipment
               #:main-hand (new-shortspear)
               #:torso     (new-leather-armor))
  (rush-behavior))

(define-monster orc (monster) #\o "An orc."
  (17 11 12 8 7 6) 1/2 (8)
  #:base-attack-bonus 1
  #:equipment (new-equipment
               #:main-hand (new-greataxe)
               #:torso     (new-studded-leather-armor))
  (pursue-behavior))


(define-class <animal> (monster))

(define-monster bat (animal) #\b "A bat."
  (1 15 10 2 14 4) 1/10 (2)
  ;; will attack with unarmed strike (1d4 + str)
  (flee-behavior))

(define-monster rat (animal) #\r "A rat."
  (2 15 10 2 12 2) 1/8 (2)
  ;; also unarmed strike
  (rush-behavior))

(define-monster wolf (animal) #\w "A wolf."
  (13 15 15 2 12 6) 1 (8 8)
  #:base-attack-bonus 1
  #:equipment (new-equipment
               #:main-hand (new-natural-weapon '(6) 'piercing)) ; bite
  (pursue-behavior))


(define-class <undead> (monster))
