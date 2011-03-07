#lang racket

(require "../utilities/class.rkt"
         "../utilities/display.rkt")
(require "../character.rkt"
         "../monsters.rkt")
(require "items.rkt")

(provide (all-defined-out))

(define-class <goblin> (monster))
(define (new-goblin)
  (new-monster make-goblin
	       "goblin"
	       11 13 12 10 9 6
	       0 1/3 '(8)
	       1 1 1 6
	       (new-equipment
		#:main-hand (new-club)
		#:off-hand  (new-light-shield)
		#:torso     (new-leather-armor))
	       (rush-behavior)))
(define-method (show (m struct:goblin)) #\g)
(define-class <goblin-archer> (goblin))
(define (new-goblin-archer)
  (new-monster make-goblin-archer
	       "goblin archer"
	       11 13 12 10 9 6 ;; TODO abstract with goblin
	       0 1/2 '(8)
	       1 1 1 6
	       (new-equipment ;; TODO maybe also have a melee weapon
		#:main-hand (new-shortbow)) ; no armor to compensate for the bow
	       (ranged-behavior)))
(define-method (show (m struct:goblin-archer)) (new-sprite #\g #:fg 'magenta))

(define-class <kobold> (monster))
(define (new-kobold)
  (new-monster make-kobold
	       "kobold"
	       9 13 10 10 9 8
	       0 1/4 '(8)
	       1 1 1 6
	       (new-equipment
		#:main-hand (new-shortspear)
		#:torso     (new-leather-armor))
	       (rush-behavior)))
(define-method (show (m struct:kobold)) #\k)

(define-class <orc> (monster))
(define (new-orc)
  (new-monster make-orc
	       "orc"
	       17 11 12 8 7 6
	       0 1/2 '(8)
	       1 1 1 6
	       (new-equipment
		#:main-hand (new-greataxe)
		#:torso     (new-studded-leather-armor))
	       (pursue-behavior)))
(define-method (show (m struct:orc)) #\o)


(define-class <animal> (monster))

(define-class <bat> (animal))
(define (new-bat)
  (new-monster make-bat
	       "bat"
	       1 15 10 2 14 4
	       0 1/10 '(2)
	       0 0 1 6 ;; TODO make faster, and raise the challenge rating
	       (new-equipment) ; will attack with unarmed strike (1d4 - str)
	       (flee-behavior)))
(define-method (show (m struct:bat)) #\b)

(define-class <rat> (animal))
(define (new-rat)
  (new-monster make-rat
	       "rat"
	       2 15 10 2 12 2
	       0 1/8 '(2)
	       0 0 1 6
	       (new-equipment) ; also unarmed strike
               ;; TODO have a way to represent natural weapons
	       (rush-behavior)))
(define-method (show (m struct:rat)) #\r)


(define-class <undead> (monster)) ;; TODO add some
