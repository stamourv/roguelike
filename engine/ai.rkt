#lang racket

(require "../utilities/grid.rkt"
         "../utilities/display.rkt")
(require "cell.rkt"
         "floor.rkt"
         "character.rkt"
         "monsters.rkt"
         "items.rkt"
         "visibility.rkt"
         "pathfinding.rkt")
(provide (all-defined-out))

;; if we would pass through another monster, take into account the number of
;; turns it has been stuck there, to avoid congestion
(define (anti-congestion-heuristic g pos)
  (let ((occ (cell-occupant (grid-ref g pos))))
    (if (and occ (monster? occ))
        (* (behavior-nb-turns-idle
            (monster-behavior occ))
           5)
        0)))

(define (rush-behavior)
  (new-behavior
   (lambda (b)
     (lambda (monster player-pos)
       (let ((pos (character-pos monster))
	     (map (floor-map (character-floor monster))))
	 (when (line-of-sight? map pos player-pos)
           (let ((next (find-path
                        map pos player-pos
                        #:extra-heuristic anti-congestion-heuristic)))
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
	   (let ((next (and target
                            (find-path
                             map pos target
                             #:extra-heuristic anti-congestion-heuristic))))
	     (when next (move-or-increment-idle map monster next)))))))))

(define (flee-behavior)
  (new-behavior
   (lambda (b)
     (lambda (monster player-pos)
       (let ((pos (character-pos monster))
	     (map (floor-map (character-floor monster))))
	 (cond ((member player-pos (four-directions pos))
		;; we are next to the player, attack
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
       (let ([targets (available-targets monster)])
         (when (not (null? targets))
           (ranged-attack monster (car targets))))))))


(define (move-or-increment-idle map monster dest)
  (let ((pos (character-pos monster)))
    (move map monster dest)
    (when (equal? pos (character-pos monster))
      ;; we did not move, increment idle counter
      (let ((b (monster-behavior monster)))
        (set-behavior-nb-turns-idle! b (+ (behavior-nb-turns-idle b) 1))))))
