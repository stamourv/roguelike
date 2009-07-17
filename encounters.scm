(import utilities)
(import cell)
(import grid)
(import character)
(import monsters)
(import dungeon)

(define-type encounter-type
  points
  monsters ; list of functions that create monsters
  can-be-placed?) ; takes a room as parameter
;; TODO have more, maybe have chests, treasure, dungeon features (campfire), maybe a name for the encounter, to display when we enter the room ?

(define-type encounter
  monsters) ;; TODO have more, espescially other kinds of objects that would need to be placed, such as chests or campfires
;; TODO also have some treasure with the encounter, see DM guide for proportions and amount by encounter level
(define (new-encounter encounter-type) ; actually creates the monsters
  (make-encounter (map call (encounter-type-monsters encounter-type))))
(define (encounter-points e)
  (fold + 0 (map character-level (encounter-monsters e))))

(define (new-encounter-type
         monsters
         #!optional (restriction #f))
  (if (not restriction) ;; FOO put as default value once black hole fully supports key parameters
      (set! restriction (lambda (room)
			  (>= (length (room-cells room))
			      (length monsters)))))
  (let* ((encounter-type  (make-encounter-type
                           #f
                           monsters
                           restriction))
         (dummy-encounter (new-encounter encounter-type)))
    (encounter-type-points-set! encounter-type
                                (encounter-points dummy-encounter))
    encounter-type))

(define encounter-types ;; TODO have weights, since some would be more common, make it so it can use random-choice
  (map new-encounter-type ;; TODO have a "language" to define encounters types, maybe make the probability a function of the level-no ?
       `((,new-bat ,new-bat ,new-bat)
         (,new-bat ,new-bat ,new-bat ,new-bat)
         (,new-rat ,new-rat)
         (,new-rat ,new-rat ,new-rat)
         (,new-rat ,new-rat ,new-rat ,new-rat)
         (,new-kobold ,new-kobold ,new-kobold)
         (,new-kobold ,new-kobold ,new-kobold ,new-kobold)
         (,new-goblin ,new-goblin)
         (,new-goblin ,new-goblin ,new-goblin)
         (,new-orc ,new-orc)
         (,new-orc ,new-goblin ,new-goblin))))


(define (possible-encounters no)
  (let* ((encounter-level-cap (/ no 2.5))
         (encounter-level-bottom
          (max (/ no 4)
               (fold min
                     encounter-level-cap
                     (map encounter-type-points
                          encounter-types)))))
    (filter (lambda (e)
              (let ((pts (encounter-type-points e)))
                (and (>= pts encounter-level-bottom)
                     (<= pts encounter-level-cap))))
            encounter-types)))

(define (generate-encounters floor)
  (let* ((no                       (+ (floor-no floor) 1))
         (possible-encounter-types (possible-encounters no))
         (actual-bottom            (fold min
                                         no ; generous upper bound
                                         (map encounter-type-points
                                              possible-encounter-types))))
    (if (null? possible-encounter-types)
        (error "no possible encounters for this level"))
    (let loop ((pts            (* no 5))
               (free-rooms     (floor-rooms floor))
               (floor-monsters '()))
      (if (and (>= pts actual-bottom)
               (not (null? free-rooms)))
          (let* ((type             (random-element possible-encounter-types))
                 (encounter-points (encounter-type-points type))
                 (room             (random-element free-rooms)))
            (if (and (<= encounter-points pts)
                     (not (room-encounter room)) ; already an encounter there
                     ((encounter-type-can-be-placed? type) room))
                (let loop2 ((monsters     (encounter-monsters
                                           (new-encounter type)))
                            (all-monsters '())
                            (space        (room-cells room)))
                  (if (not (null? monsters))
                      (let ((cell (random-element space))
                            (mon  (car monsters)))
                        (character-floor-set! mon floor)
                        (cell-occupant-set! (grid-ref (floor-map floor) cell)
                                            mon)
                        (character-pos-set! mon cell)
                        (loop2 (cdr monsters)
                               (cons mon all-monsters)
                               (remove cell space)))
                      (loop (- pts encounter-points)
                            (remove room free-rooms)
                            (append floor-monsters all-monsters))))
                (loop pts free-rooms floor-monsters))) ; try something else
          (floor-monsters-set! floor floor-monsters)))))
