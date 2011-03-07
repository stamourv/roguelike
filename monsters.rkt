#lang racket

(require "utilities/utilities.rkt"
         "utilities/class.rkt"
         "utilities/cell.rkt"
         "utilities/grid.rkt"
         "utilities/floor-utils.rkt"
         "utilities/display.rkt")
(require "data/items.rkt")
(require "character.rkt"
         "scheduler.rkt"
         "items.rkt"
         "visibility.rkt"
         "common.rkt")
(provide (all-defined-out))

(define-class <monster> (character)
  ;; function that takes the monster, the floor, and the position of the
  ;; player as parameters and makes the monster act
  behavior)
;; TODO have different speeds (maybe even initiative?) to determine which
;;  monster moves first

;; to handle the repetitive part of generating the hp
;; TODO could be done with a constructor ?
(define (new-monster f . args)
  (let ((m (apply f `(,@(take! args 1) ; name
		      #f #f            ; pos, floor
		      ,@(take! args 6) ; str, dex, con, int, wis, cha
		      ,(make-hash)     ; altered-attrs
		      ,@(take! args 3) ; natural-ac, level, hit-dice
		      #f #f            ; hp, max-hp
		      ,@args))))       ; rest
    (init-hp m)
    m))

(define-method (turn (m struct:monster) reschedule?)
  (when (and (> (character-hp m) 0)
             (eq? (character-floor m) (character-floor player)))
    ((behavior-fun (monster-behavior m))
     m (character-pos player))
    (when reschedule? (reschedule m)))) ;; TODO call-next-method

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



;; AI

(define-struct behavior (fun nb-turns-idle) #:mutable #:transparent)

(define (new-behavior fun)
  (let ((b (make-behavior #f 0)))
    (set-behavior-fun! b (fun b))
    b))

(define (rush-behavior)
  (new-behavior
   (lambda (b)
     (lambda (monster player-pos) ;; TODO have a macro for all that ?
       (let ((pos (character-pos monster))
	     (map (floor-map (character-floor monster))))
	 (when (line-of-sight? map pos player-pos)
           (let ((next (find-path map pos player-pos)))
             (when next (move-or-increment-idle map monster next)))))))))

(define (pursue-behavior)
  (new-behavior
   (lambda (b)
     (let ((last-player-pos #f)) ; AI state
       (lambda (monster player-pos)
	 (let* ((pos    (character-pos monster))
		(map    (floor-map (character-floor monster)))
		(target (cond ((line-of-sight? map pos player-pos)
			       (set! last-player-pos player-pos)
			       player-pos)
			      (else last-player-pos)))) ; we try to pursue
	   (let ((next (and target (find-path map pos target))))
	     (when next (move-or-increment-idle map monster next)))))))))

(define (flee-behavior)
  (new-behavior
   (lambda (b)
     (lambda (monster player-pos)
       (let ((pos (character-pos monster))
	     (map (floor-map (character-floor monster))))
	 (cond ((member player-pos (four-directions pos))
		;; we are next to the player, attack
                ;; TODO have another that flees even if next to the player, and
                ;;  one that flees when wounded
		=> (lambda (pl)
		     (move-or-increment-idle map monster player-pos)))
	       ((line-of-sight? map pos player-pos)
		;; flee
		(let* ((x     (point-x pos))
		       (y     (point-y pos))
		       (dx    (- (point-x player-pos) x))
		       (dy    (- (point-y player-pos) y))
		       (adx   (abs dx))
		       (ady   (abs dy))
		       (vert  (lambda ()
				(move-or-increment-idle
				 map monster
                                 (new-point (- x (/ dx (max adx 1))) y))))
		       (horiz (lambda ()
				(move-or-increment-idle
				 map monster
                                 (new-point x (- y (/ dy (max ady 1))))))))
		  (if (> adx ady)
		      ;; try to move vertically first
		      (when (not (vert))  (horiz))
		      ;; try to move horizontally first
		      (when (not (horiz)) (vert)))))))))))

(define (ranged-behavior)
  (new-behavior
   (lambda (b)
     (lambda (monster player-pos)
       (when (not (ranged-weapon? (equipment-main-hand
                                   (character-equipment monster))))
         (error "monster " monster " has no ranged weapon"))
       (let ((pos (character-pos monster)) ;; TODO have that in a macro
	     (map (floor-map (character-floor monster))))
         ;; TODO not very interesting for the moment
	 (if (clear-shot? map pos player-pos)
	     (ranged-attack monster (cell-occupant (grid-ref map player-pos)))
	     #f)))))) ;; stay there


(define (move-or-increment-idle map monster dest)
  (let ((pos (character-pos monster)))
    (move map monster dest)
    (when (equal? pos (character-pos monster))
      ;; we did not move, increment idle counter
      (let ((b (monster-behavior monster)))
        (set-behavior-nb-turns-idle! b (+ (behavior-nb-turns-idle b) 1))))))

;; simple pathfinding using A*
(define (find-path g a b)
  ;; grid of pairs (cost . previous)
  (let* ((height  (grid-height g))
	 (width   (grid-width g))
	 (maximum (* width height)) ; arbitrarily high value
	 (costs (empty-grid
		 height width
		 #:cell-fun (lambda (pos)
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
	  (let* ((next (foldl (lambda (new best) ; least expensive neighbor
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
