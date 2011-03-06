#lang racket

(require "class.rkt" "character.rkt" "player.rkt" "monsters.rkt"
         "objects.rkt" "utilities.rkt" "grid.rkt" "floor.rkt"
         "cell.rkt" "scheduler.rkt"
         (only-in racket/base [floor math-floor]))
(provide (all-defined-out))

(define-method (attack (attacker struct:player-character) defender)
  (printf "~a attacks the ~a"
          (character-name attacker)
          (character-name defender))
  ;; TODO instead of check-if-hit, use call-next-method?
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
  (check-if-hit attacker defender)) ;; TODO good for call-next-method too
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
  ;; TODO ranged weapons can currently be used in melee with no penalty, and
  ;;  use the strength bonus to hit
  (let ((roll ((dice 20))))
    (if (>= (+ roll (bonus-fun attacker))
	    (get-armor-class defender))
	(damage attacker defender)
	(display " and misses.\n"))))
;; TODO depending on by how much it missed, say different things


(define-generic damage)
(define-method (damage attacker defender)
  (let ((dmg (max (get-damage attacker) 1))) ;; TODO could deal 0 damage ?
    (printf " and deals ~a damage.\n" dmg)
    (set-character-hp! defender (- (character-hp defender) dmg))))
(define-method (damage (attacker struct:player-character)
                       (defender struct:monster))
  (let ((dmg (max (get-damage attacker) 1))) ;; TODO could deal 0 damage ?
    ;; TODO copied from character.scm, the fallback method, call-next-method?
    ;;  (but would mess up with the .\n at the end)
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
            ;; TODO for now, we just give them a turn, which means they could
            ;;  walk away instead of attacking
	    (four-directions (character-pos char))))


;; TODO not all that clean to have it here, but it's the only place where it
;;  would not lead to circular dependencies
;;  -> parameter?
;; removes a monster, usually when killed
(define (remove-monster monster)
  (printf ", which kills the ~a.\n" (character-name monster))
  (let* ((floor (character-floor monster))
	 (cell  (grid-ref (floor-map floor) (character-pos monster))))
    ;; drop equipment with a certain probability TODO TWEAK
    (for-each-equipped (lambda (obj where)
			 (when (and obj (removable? obj) (random-boolean 0.3))
                           (add-object cell obj)))
		       (character-equipment monster))
    ;; remove the monster
    (set-cell-occupant! cell #f)
    (set-floor-monsters! floor (remove monster (floor-monsters floor)))
    (add-monster-experience monster)))
