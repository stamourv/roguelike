#lang racket

(require unstable/function)
(require "utilities.rkt")
(require "../utilities/random.rkt"
         "../utilities/floor.rkt"
         "../utilities/cell.rkt"
         "../utilities/grid.rkt")
(require "../engine/character.rkt")
(require "../data/monsters.rkt")
(provide place-encounters)

(define-struct encounter-type
  (points
   monsters ; list of functions that create monsters
   can-be-placed?) ; takes a room as parameter
  #:mutable #:transparent)
;; TODO have more, maybe have chests, treasure, dungeon features (campfire),
;;  maybe a name for the encounter, to display when we enter the room ?

(define-struct encounter (encounter-type monsters) #:transparent)
;; TODO have more, espescially other kinds of objects that would need to be
;;  placed, such as chests or campfires
;; TODO also have some treasure with the encounter, see DM guide for
;;  proportions and amount by encounter level
(define (new-encounter encounter-type) ; actually creates the monsters
  (make-encounter encounter-type
		  (map call (encounter-type-monsters encounter-type))))
(define (encounter-points e)
  (apply + (map character-level (encounter-monsters e))))

(define (new-encounter-type
         monsters
         (restriction (lambda (room)
                        (>= (length (room-cells room))
                            (length monsters)))))
  (let* ((encounter-type  (make-encounter-type
                           #f
                           monsters
                           restriction))
         (dummy-encounter (new-encounter encounter-type)))
    (set-encounter-type-points! encounter-type
                                (encounter-points dummy-encounter))
    encounter-type))

(define encounter-types
  ;; TODO have weights, since some would be more common, make it so it can
  ;;  use random-choice
  (map new-encounter-type
       ;; TODO have a "language" to define encounters types, maybe make the
       ;;  probability a function of the level-no ?
       `((,new-bat ,new-bat ,new-bat)
         (,new-bat ,new-bat ,new-bat ,new-bat)
         (,new-rat ,new-rat)
         (,new-rat ,new-rat ,new-rat)
         (,new-rat ,new-rat ,new-rat ,new-rat)
         (,new-kobold ,new-kobold ,new-kobold)
         (,new-kobold ,new-kobold ,new-kobold ,new-kobold)
         (,new-goblin ,new-goblin)
         (,new-goblin ,new-goblin ,new-goblin)
	 (,new-goblin ,new-goblin-archer)
	 (,new-goblin ,new-goblin-archer ,new-goblin-archer)
	 (,new-goblin ,new-goblin ,new-goblin-archer)
         (,new-orc ,new-orc)
         (,new-orc ,new-goblin ,new-goblin))))


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
  (let* ((possible-encounter-types (possible-encounters no))
         (actual-bottom            (foldl min
                                          no ; generous upper bound
                                          (map encounter-type-points
                                               possible-encounter-types))))
    (when (null? possible-encounter-types)
      (error "no possible encounters for this level"))
    (let loop ((pts        (* no 5))
               (encounters '()))
      (if (and (>= pts actual-bottom))
          (let* ((type             (random-element possible-encounter-types))
                 (encounter-points (encounter-type-points type)))
            (if (<= encounter-points pts)
		(loop (- pts encounter-points)
		      (cons (new-encounter type) encounters))
                (loop pts encounters))) ; try something else
	  encounters))))

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
          (if (and (not (room-encounter room)) ; already an encounter there
                   ((encounter-type-can-be-placed?
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
