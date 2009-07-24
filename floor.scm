(import grid)
(import utilities)

(define-type floor ;; TODO also have a dungeon type ?
  no
  map
  rooms ;; TODO have a set ?
  stairs-up
  stairs-down
  walkable-cells ;; TODO remove from the list if we add impassable features, also remove from the cell lists in room objects
  monsters)

(define-type room
  type
  cells ; TODO the 3 of these are sets, use hash tables for sets if it becomes slow
  walls
  connected-to
  encounter)
(define (get-room point rooms)
  (find (lambda (room) (member point (room-cells room))) rooms))
(define (connected? a b)
  ;; since it's commutative, no need to check both sides
  ;; TODO need to check for a and b, since this sometimes receives #f, probably due to a bug somewhere, investigate
  (and a b (memq a (room-connected-to b))))
(define (connect! a b)
  (if (and a b) ;; TODO same here
      (begin (room-connected-to-set! a (cons b (room-connected-to a)))
	     (room-connected-to-set! b (cons a (room-connected-to b))))))

(define (random-free-position floor)
  (let ((rooms (floor-rooms floor)))
    (random-element
     (filter (lambda (cell)
	       ;; not in a corridor, and not in front of a door
	       (and (not (eq? 'corridor (room-type (get-room cell rooms))))
		    (not (next-to-a-door? (floor-map floor) cell))))
	     (floor-walkable-cells floor)))))
