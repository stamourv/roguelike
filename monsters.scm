(import (path class))
(import utilities)
(import character)
(import scheduler)
(import objects)
(import cell)
(import grid)
(import dungeon)
(import visibility)
(import common)

(define-class monster (character)
  ;; function that takes the monster, the floor, and the position of the
  ;; player as parameters and makes the monster act
  (slot: behavior)) ;; TODO have different speeds (maybe even initiative?) to determine which monster moves first

;; to handle the repetitive part of generating the hp ;; TODO could be done with a constructor ?
(define (new-monster f . args)
  (let ((m (apply f args)))
    (init-hp m)
    m))

(define-method (turn (m monster) reschedule?)
  (if (and (> (character-hp m) 0)
	   (eq? (character-floor m) (character-floor player)))
      (begin ((behavior-fun (monster-behavior m))
	      m (character-pos player))
	     (if reschedule? (reschedule m))))) ;; TODO this would be a nice candidate for call-next-method, once it works

(define-method (attack (attacker monster) defender)
  (display (string-append "The "
			  (character-name attacker)
			  " attacks "
			  (character-name defender)))
  (check-if-hit attacker defender))
;; monsters don't attack other monsters
(define-method (attack (attacker monster) (defender monster)) #f)


(define-class goblin (monster))
(define (new-goblin)
  (new-monster make-goblin
	       "goblin" #f #f ;; TODO add ranged versions too
	       11 13 12 10 9 6 (make-table) 0
	       1/3 '(8) #f #f 1 6
	       (new-equipment
		main-hand: (new-club)
		off-hand:  (new-light-shield)
		torso:     (new-leather-armor))
	       (rush-behavior)))
(define-method (print (m goblin)) #\g)

(define-class kobold (monster))
(define (new-kobold)
  (new-monster make-kobold
	       "kobold" #f #f
	       9 13 10 10 9 8 (make-table) 0
	       1/4 '(8) #f #f 1 6
	       (new-equipment
		main-hand: (new-shortspear)
		torso:     (new-leather-armor))
	       (rush-behavior)))
(define-method (print (m kobold)) #\k)

(define-class orc (monster))
(define (new-orc)
  (new-monster make-orc
	       "orc" #f #f
	       17 11 12 8 7 6 (make-table) 0
	       1/2 '(8) #f #f 1 6
	       (new-equipment
		main-hand: (new-greataxe) ;; TODO handle two-handed weapons and placeholders for monsters ? since they can't change their equipment, and since they are used as two-handed weapons anyway (1.5 times the strength bonus), not really necessary
		torso:     (new-studded-leather-armor))
	       (pursue-behavior)))
(define-method (print (m orc)) #\o)


(define-class animal (monster))

(define-class bat (animal)) ;; TODO actually just as annoying and dangerous as rats, so up the challenge rating ?
(define (new-bat) ;; TODO these monsters are kind of pointless since they can barely do damage
  (new-monster make-bat
	       "bat" #f #f
	       1 15 10 2 14 4 (make-table) 0
	       1/10 '(2) #f #f 0 6 ;; TODO make faster, and raise the challenge rating
	       (new-equipment) ; will attack with unarmed strike (1d4 - str)
	       (rush-behavior)))
(define-method (print (m bat)) #\b)

(define-class rat (animal))
(define (new-rat)
  (new-monster make-rat
	       "rat" #f #f
	       2 15 10 2 12 2 (make-table) 0
	       1/8 '(2) #f #f 0 6
	       (new-equipment) ; also unarmed strike ;; TODO have a way to represent natural weapons
	       (rush-behavior)))
(define-method (print (m rat)) #\r)


(define-class undead (monster)) ;; TODO add some


;; AI
(define-type behavior
  fun
  nb-turns-idle)

(define (new-behavior fun)
  (let ((b (make-behavior #f 0)))
    (behavior-fun-set! b (fun b))
    b))

(define (rush-behavior) (new-behavior rush))
(define (rush b)
  (lambda (monster player-pos)
    (let ((pos (character-pos monster))
	  (map (floor-map (character-floor monster))))
      (if (line-of-sight? map pos player-pos)
	  (let ((next (find-path map pos player-pos)))
	    (if next (move-or-increment-idle map monster next)))))))

(define (pursue-behavior) (new-behavior pursue))
(define (pursue b)
  (let ((last-player-pos #f)) ; AI state
    (lambda (monster player-pos)
      (let* ((pos    (character-pos monster))
	     (map    (floor-map (character-floor monster)))
	     (target (cond ((line-of-sight? map pos player-pos)
			    (set! last-player-pos player-pos)
			    player-pos)
			   (else last-player-pos)))) ; we try to pursue
	(let ((next (and target (find-path map pos target))))
	  (if next (move-or-increment-idle map monster next)))))))

(define (move-or-increment-idle map monster dest)
  (let ((pos (character-pos monster)))
    (move map monster dest)
    (if (equal? pos (character-pos monster))
	;; we did not move, increment idle counter
	(let ((b (monster-behavior monster)))
	  (behavior-nb-turns-idle-set! b (+ (behavior-nb-turns-idle b) 1))))))

;; simple pathfinding using A*
(define (find-path g a b)
  ;; grid of pairs (cost . previous)
  (let* ((height  (grid-height g))
	 (width   (grid-width g))
	 (maximum (* width height)) ; arbitrarily high value
	 (costs (empty-grid
		 height width
		 cell-fun: (lambda (pos)
			     (cons (if (walkable-cell? (grid-ref g pos))
				       maximum
				       #f) ; we can't even get there
				   #f)))))
    (grid-set! costs a (cons 0 #f)) ; initialize
    (let loop ((queue (list a))) ; list of positions
      (if (null? queue)
	  ;; we have found a path, we return its first step
	  (let loop ((pos  b)
		     (prev #f))
	    (let ((parent (cdr (grid-ref costs pos))))
	      (if parent
		  (loop parent pos)
		  prev)))
	  (let* ((next (fold (lambda (best new) ; least expensive neighbor
			       (if (< (car (grid-ref costs new))
				      (car (grid-ref costs best)))
				   new
				   best))
			     (car queue)
			     queue))
		 (queue (remove next queue))
		 (neighbors
		  (filter
		   (lambda (pos)
		     (if (inside-grid? g pos)
			 (cond
			  ((car (grid-ref costs pos)) =>
			   (lambda (cost)
			     (let ((new-cost
				    (+ (car (grid-ref costs next))
				       ;; heuristic cost
				       (distance pos b)
				       ;; if we would pass through another
				       ;; monster, take into account the number
				       ;; of turns it has been stuck there, to
				       ;; avoid congestion
				       (let ((occ (cell-occupant
						   (grid-ref g pos))))
					 (if (and occ (monster? occ))
					     (* (behavior-nb-turns-idle
						 (monster-behavior occ))
						5)
					     0)))))
			       (if (< new-cost cost)
				   (begin
				     (grid-set! costs pos (cons new-cost next))
				     #t)
				   #f))))
			  (else #f))
			 #f))
		   (four-directions next))))
	    (loop (append neighbors queue)))))))
