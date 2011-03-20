#lang racket

(require "../engine/cell.rkt"
         "../engine/floor.rkt")
(require "../utilities/grid.rkt"
         "../utilities/random.rkt")
(provide (all-defined-out))

(define-struct room
  (type
   ;; TODO the 3 of these are sets, use hash tables for sets if it becomes slow
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
  ;; TODO need to check for a and b, since this sometimes receives #f
  ;;  probably due to a bug somewhere, investigate
  (and a b (memq a (room-connected-to b))))
(define (connect! a b)
  (when (and a b) ;; TODO same here
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
