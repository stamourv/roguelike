#lang racket

(require "class.rkt" "utilities.rkt" "objects.rkt" "floor.rkt" "scheduler.rkt"
         "visibility.rkt" "cell.rkt" "grid.rkt"
         (only-in racket/base [floor math-floor]))
(provide (all-defined-out))

(define-class <character> ()
  (slot: name)
  
  (slot: pos)
  (slot: floor)

  (slot: str)
  (slot: dex)
  (slot: con)
  (slot: int)
  (slot: wis)
  (slot: cha)

  ;; table that contains, for each attribute, how many temporary effects are
  ;; affecting it. if this non-zero, the attribute will be displayed
  ;; differently
  (slot: altered-attrs)

  (slot: natural-ac) ;; TODO have natural weapons too, which are used when no weapon is equipped (change get-damage-fun)

  (slot: level)
  
  (slot: hit-dice)
  (slot: max-hp)
  (slot: hp)
  
  (slot: base-attack-bonus)
  (slot: current-attack-bonus) ; for multiple attacks TODO have a constructor that sets it at base, when monsters handle multiple attacks (which sets this), won't be needed (and mosnters can have it as #f)
  (slot: nb-attacks)
  
  (slot: speed) ; number of seconds needed to do a turn

  (slot: equipment))

(define (init-hp c (max? #f))
  (let* ((hd (character-hit-dice c))
	 (hp (max (+ (if max?
			 (apply + hd)
			 ((apply dice hd)))
		     (* (get-attribute-bonus 'con c)
			(length hd)))
		  1)))
    (set-character-max-hp! c hp)
    (set-character-hp!     c hp)))

(define (attribute-getter attr)
  (lambda (char)
    (case attr
      ((str)        (character-str char)) ;; TODO muck around with string->symbol instead ?
      ((dex)        (character-dex char))
      ((con)        (character-con char))
      ((int)        (character-int char))
      ((wis)        (character-wis char))
      ((cha)        (character-cha char))
      ((hp)         (character-hp  char))
      ((natural-ac) (character-natural-ac char)))))
(define (attribute-setter attr)
  (lambda (char val)
    (case attr
      ((str)        (set-character-str! char val))
      ((dex)        (set-character-dex! char val))
      ((con)        (set-character-con! char val))
      ((int)        (set-character-int! char val))
      ((wis)        (set-character-wis! char val))
      ((cha)        (set-character-cha! char val))
      ((hp)         (set-character-hp!  char val))
      ((natural-ac) (set-character-natural-ac! char val)))))

(define (get-attribute-bonus attr char)
  (quotient (- ((if (eq? attr 'dex)
		    (lambda (char)
		      (let ((dex (character-dex char))
			    (max (max-dex-bonus
				  (equipment-torso
				   (character-equipment char)))))
			(if max (min (+ 10 (* 2 max)) dex) dex)))
		    (attribute-getter attr))
                char)
               10)
            2))

;; to note that an attribute is altered by a temporary effect
(define (alter-attr    char attr n duration)
  (let ((alt    (character-altered-attrs char))
	(getter (attribute-getter attr))
	(setter (attribute-setter attr)))
    (hash-set! alt attr (+ (hash-ref alt attr 0) 1))
    (setter char (+ (getter char) n))
    (schedule
     (lambda ()
       (setter char (- (getter char) n))
       (hash-set! alt attr (- (hash-ref alt attr) 1)))
     (+ turn-no duration))))
(define (altered-attr? char attr)
  (> (hash-ref (character-altered-attrs char) attr 0) 0))

(define (get-melee-attack-bonus  c)
  (+ (character-current-attack-bonus c)
     (get-attribute-bonus 'str c)))
(define (get-ranged-attack-bonus c)
  (+ (character-current-attack-bonus c)
     (get-attribute-bonus 'dex c)))

(define-struct equipment
  (main-hand
   off-hand ; shield or 2nd weapon ;; TODO no second weapon for now
   torso) ;; TODO add more
  #:mutable #:transparent)
(define (new-equipment #:main-hand (main-hand #f)
                       #:off-hand (off-hand #f)
                       #:torso (torso #f))
  (make-equipment main-hand off-hand torso))
(define (equipment->list e)
  (list (cons (equipment-main-hand e) "main hand")
        (cons (equipment-off-hand  e) "off hand")
        (cons (equipment-torso     e) "torso")))
(define (for-each-equipped f e)
  (f (equipment-main-hand e) "main hand: ")
  (f (equipment-off-hand  e) "off hand:  ")
  (f (equipment-torso     e) "torso:     "))


(define (get-armor-class c)
  (let ((e (character-equipment c)))
    (+ 10
       (character-natural-ac c)
       (get-attribute-bonus 'dex c)
       (get-ac (equipment-torso    e))
       (get-ac (equipment-off-hand e))))) ;; TODO add more

(define (get-damage c)
  (let ((weapon (equipment-main-hand (character-equipment c))))
    (+ ((get-damage-fun weapon))
       (math-floor (* (get-attribute-bonus 'str c)
                      (cond ((ranged-weapon?     weapon) 0)
                            ((two-handed-weapon? weapon) 3/2)
                            (else                        1)))))))

(define-method (reschedule (char struct:character))
  (schedule (lambda () (turn char #t)) (+ turn-no (character-speed char))))

(define (move g occ new-pos)
  ;; moves the occupant of pos to new-pos, and returns #t if it actually moved
  (when (inside-grid? g new-pos)
    (let ((cell     (grid-ref g (character-pos occ)))
          (new-cell (grid-ref g new-pos)))
      (cond ((free-cell? new-cell)
             (set-cell-occupant! cell     #f)
             (set-cell-occupant! new-cell occ)
             (set-character-pos! occ new-pos)
             #t)
            ;; walkable, but occupied already, attack whoever is there
            ((walkable-cell? new-cell)
             (attack occ (cell-occupant (grid-ref g new-pos)))
             #f) ;; TODO return #f ? the character did not move, but its turn is likely lost (except in the case where a monster moves over a monster, and then tries to move around it, like in flee-behavior)
            (else #f)))))

(define-generic attack) ;; TODO these would be good candidates for call-next-method, but class seems to have trouble with it, or maybe it's my patched version, or maybe it's black hole
(define-generic ranged-attack)

(define (check-if-hit attacker defender
		      (bonus-fun get-melee-attack-bonus)) ;; TODO ranged weapons can currently be used in melee with no penalty, and use the strength bonus to hit
  (let ((roll ((dice 20))))
    (if (>= (+ roll (bonus-fun attacker))
	    (get-armor-class defender))
	(damage attacker defender)
	(display " and misses.\n")))) ;; TODO depending on by how much it missed, say different things

(define-generic damage)
(define-method (damage attacker defender)
  (let ((dmg (max (get-damage attacker) 1))) ;; TODO could deal 0 damage ?
    (display (string-append " and deals " (number->string dmg)
			    " damage.\n"))
    (set-character-hp! defender (- (character-hp defender) dmg))))

(define (attacks-of-opportunity char)
  (for-each (lambda (pos)
	      (cond ((grid-ref-check
		      (floor-map (character-floor char)) pos)
		     => (lambda (cell)
			  (let ((occ (cell-occupant cell)))
			    (when occ
                              (display "Attack of opportunity: ")
                              ;; give a turn, but don't reschedule
                              (turn occ #f))))))) ;; TODO for now, we just give them a turn, which means they could walk away instead of attacking
	    (four-directions (character-pos char))))

(define (clear-shot? grid a b) (line-of-sight? grid a b #t))