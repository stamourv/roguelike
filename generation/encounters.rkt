#lang racket

(require "utilities.rkt")
(require "../utilities/random.rkt"
         "../utilities/floor.rkt"
         "../utilities/cell.rkt"
         "../utilities/grid.rkt")
(require "../engine/character.rkt"
         "../engine/encounters.rkt")
(require "../data/encounters.rkt")
(provide place-encounters show-encounters)


(define (possible-encounters no)
  (let* ((encounter-level-cap (/ no 2.5))
         (encounter-level-bottom
          (max (/ no 4)
               (foldl min
                      encounter-level-cap
                      (map encounter-type-points
                           encounter-types)))))
    (filter (lambda (e)
              (let ((pts (encounter-type-points e)))
                (and (>= pts encounter-level-bottom)
                     (<= pts encounter-level-cap))))
            encounter-types)))

(define (generate-encounters no)
  (let* ([possible-encounter-types (possible-encounters no)]
         [actual-bottom            (foldl min
                                          no ; generous upper bound
                                          (map encounter-type-points
                                               possible-encounter-types))]
         [possible-types-table     (normalize-probability-table
                                    (map (lambda (t)
                                           (cons (encounter-type-rarity t) t))
                                         possible-encounter-types))])
    (if (null? possible-encounter-types)
        (generate-encounters (sub1 no))
        (let loop ([pts        (* no 5)]
                   [encounters '()])
          (if (and (>= pts actual-bottom))
              (let* ([type             (random-choice possible-types-table)]
                     [encounter-points (encounter-type-points type)])
                (if (<= encounter-points pts)
                    (loop (- pts encounter-points)
                          (cons (new-encounter type) encounters))
                    (loop pts encounters))) ; try something else
              encounters)))))

;; encounters scale with the player's level, not the dungeon level
(define (place-encounters floor level)
  (let ((encounters (generate-encounters level)))
    (set-floor-monsters! floor (apply append (map encounter-monsters
						  encounters)))
    (let loop ((encounters encounters)
	       (free-rooms (floor-rooms floor)))
      (when (not (or (null? encounters)
                     (null? free-rooms)))
        (let* ((room      (random-element free-rooms))
               (encounter (car encounters)))
          (if (and (not (room-encounter room)) ; not already an encounter there
                   (>= (length (room-cells room)) ; would fit
                       (length (encounter-monsters encounter)))
                   ((encounter-type-can-be-placed? ; other restrictions
                     (encounter-encounter-type encounter))room))
              (let loop2 ((monsters (encounter-monsters encounter))
                          (space    (room-cells room)))
                (if (not (null? monsters))
                    (let ((cell (random-element space))
                          (mon  (car monsters)))
                      (set-character-floor! mon floor)
                      (set-cell-occupant! (grid-ref (floor-map floor) cell)
                                          mon)
                      (set-character-pos! mon cell)
                      (loop2 (cdr monsters)
                             (remove cell space)))
                    (loop (cdr encounters) (remove room free-rooms))))
              (loop encounters (remove room free-rooms))))))))

;; for debugging purposes
(define (show-encounters no)
  (map (lambda (enc) (map (lambda (m) (character-name m))
			  (encounter-monsters enc)))
       (generate-encounters no)))
