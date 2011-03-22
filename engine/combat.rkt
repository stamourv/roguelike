#lang racket

(require (only-in racket/base [floor math-floor]))
(require "../utilities/random.rkt"
         "../utilities/class.rkt"
         "../utilities/grid.rkt")
(require "cell.rkt"
         "floor.rkt"
         "character.rkt"
         "player.rkt"
         "monsters.rkt"
         "common.rkt"
         "items.rkt"
         "scheduler.rkt"
         "visibility.rkt")
(provide (all-defined-out))

(define-method (attack (attacker struct:player-character) defender)
  (printf "~a attacks the ~a"
          (character-name attacker)
          (character-name defender))
  (check-if-hit attacker defender))
(define-method (ranged-attack (attacker struct:player-character) defender)
  (printf "~a shoots at the ~a"
          (character-name attacker)
          (character-name defender))
  (check-if-hit attacker defender get-ranged-attack-bonus)
  (attacks-of-opportunity attacker))

(define-method (attack (attacker struct:monster) defender)
  (printf "The ~a attacks ~a"
          (character-name attacker) (character-name defender))
  (check-if-hit attacker defender))
(define-method (ranged-attack (attacker struct:monster) defender)
  (printf "The ~a shoots at ~a"
          (character-name attacker) (character-name defender))
  (check-if-hit attacker defender get-ranged-attack-bonus)
  (attacks-of-opportunity attacker))

;; monsters don't attack other monsters
(define-method (attack (attacker struct:monster)
                       (defender struct:monster))
  #f)
(define-method (ranged-attack (attacker struct:monster)
                              (defender struct:monster))
  #f)

(define (get-damage c)
  (let ((weapon (equipment-main-hand (character-equipment c))))
    (+ ((get-damage-fun weapon))
       (math-floor (* (get-attribute-bonus 'str c)
                      (cond ((ranged-weapon?     weapon) 0)
                            ((two-handed-weapon? weapon) 3/2)
                            (else                        1)))))))


(define (check-if-hit attacker defender
		      (bonus-fun get-melee-attack-bonus))
  (let ((roll ((dice 20))))
    (if (>= (+ roll (bonus-fun attacker))
	    (get-armor-class defender))
	(damage attacker defender)
	(display " and misses.\n"))))


(define-generic damage)
(define-method (damage attacker defender)
  (let ((dmg (max (get-damage attacker) 1)))
    (printf " and deals ~a damage.\n" dmg)
    (set-character-hp! defender (- (character-hp defender) dmg))))
(define-method (damage (attacker struct:player-character)
                       (defender struct:monster))
  (let ((dmg (max (get-damage attacker) 1)))
    (printf " and deals ~a damage" dmg)
    (set-character-hp! defender (- (character-hp defender) dmg))
    (if (<= (character-hp defender) 0)
	(remove-monster defender)
	(display ".\n"))))

(define (attacks-of-opportunity char)
  (for-each (lambda (pos)
	      (cond ((grid-ref-check
		      (floor-map (character-floor char)) pos)
		     => (lambda (cell)
			  (let ((occ (cell-occupant cell)))
			    (when occ
                              (display "Attack of opportunity: ")
                              ;; give a turn, but don't reschedule
                              (turn occ #f)))))))
	    (four-directions (character-pos char))))


;; removes a monster, usually when killed
(define (remove-monster monster)
  (printf ", which kills the ~a.\n" (character-name monster))
  (let* ((floor (character-floor monster))
	 (cell  (grid-ref (floor-map floor) (character-pos monster))))
    ;; drop equipment with a certain probability
    (for-each-equipped (lambda (obj where)
			 (when (and obj (removable? obj) (random-boolean 0.3))
                           (add-item cell obj)))
		       (character-equipment monster))
    ;; remove the monster
    (set-cell-occupant! cell #f)
    (set-floor-monsters! floor (remove monster (floor-monsters floor)))
    (add-monster-experience monster)))


(define-method (available-targets (shooter struct:player-character))
  (filter (lambda (m)
            (and (eq? (grid-ref (player-view player)
                                (character-pos m))
                      'visible)
                 (clear-shot? (player-map player)
                              (character-pos player)
                              (character-pos m))))
          (floor-monsters (character-floor player))))
(define-method (available-targets (shooter struct:monster))
  (let ((pos (character-pos shooter))
        (map (floor-map (character-floor shooter))))
    (if (clear-shot? map pos (character-pos player))
        (list player)
        '())))


(define (drink-action item user)
  (drink item)
  (attacks-of-opportunity user))
