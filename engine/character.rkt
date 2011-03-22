#lang racket

(require "../utilities/random.rkt"
         "../utilities/class.rkt"
         "../utilities/grid.rkt")
(require "cell.rkt"
         "items.rkt"
         "scheduler.rkt")
(provide (all-defined-out))

(define-class <character> ()
  name
  pos floor
  str dex con int wis cha
  ;; table that contains, for each attribute, how many temporary effects are
  ;; affecting it. if this non-zero, the attribute will be displayed
  ;; differently
  altered-attrs
  natural-ac
  level
  hit-dice max-hp hp
  base-attack-bonus
  current-attack-bonus ; for multiple attacks
  nb-attacks
  speed ; number of seconds needed to do a turn
  equipment)

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
      ((str)        (character-str char))
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
     duration)))
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
   off-hand ; shield or 2nd weapon
   torso)
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
       (get-ac (equipment-off-hand e)))))

(define-method (reschedule (char struct:character))
  (schedule (lambda () (turn char #t)) (character-speed char)))

(define-generic attack)
(define-generic ranged-attack)
;; for ranged attacks. returns a list of characters
(define-generic available-targets)


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
             #f)
            (else #f)))))
