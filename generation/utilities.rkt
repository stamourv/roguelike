#lang racket

(require "../engine/cell.rkt"
         "../engine/floor.rkt")
(require "../utilities/grid.rkt"
         "../utilities/random.rkt")
(provide (all-defined-out))

(define-struct room
  (type
   cells
   walls
   preferred-expansion-points ; like the opposite end of a corridor
   connected-to
   encounter)
  #:mutable #:transparent)
(define (get-room point rooms)
  (findf (lambda (room) (member point (room-cells room))) rooms))
(define (connected? a b)
  ;; since it's commutative, no need to check both sides
  (and a b (memq a (room-connected-to b))))
(define (connect! a b)
  (when (and a b)
    (set-room-connected-to! a (cons b (room-connected-to a)))
    (set-room-connected-to! b (cons a (room-connected-to b)))))


(define (next-to-a-door? g pos)
  (ormap (lambda (new) (door? (grid-ref-check g new)))
         (four-directions pos)))

(define (random-free-position floor)
  (let ((rooms (floor-rooms floor)))
    (random-element
     (filter (lambda (cell)
	       ;; not in a corridor, and not in front of a door
	       (and (not (eq? 'corridor (room-type (get-room cell rooms))))
		    (not (next-to-a-door? (floor-map floor) cell))))
	     (floor-walkable-cells floor)))))
