#lang racket

(require "cell.rkt" "grid.rkt" "random.rkt")
(provide (all-defined-out))

(define-struct floor ;; TODO also have a dungeon type ?
  (map
   rooms ;; TODO have a set ?
   stairs-up
   stairs-down
   ;; TODO remove from the list if we add impassable features, also remove
   ;;  from the cell lists in room objects
   walkable-cells
   monsters)
  #:mutable #:transparent)

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


;; given a wall, returns the cells that are either perpendicular or
;; parrallel to the direction of the wall
(define-values (wall-perpendicular wall-parrallel)
  (let ()
    (define ((mk per?) g pos)
      (define (wall-there? p) (wall? (grid-ref-check g p)))
      ((cond [(andmap wall-there? (up-down    pos))
              (if per? left-right up-down)]
             [(andmap wall-there? (left-right pos))
              (if per? up-down left-right)]
             [else (lambda (x) '())]) ; not an appropriate wall
       pos))
    (values (mk #t) (mk #f))))


;; wall smoothing, for aesthetic reasons
(define (smooth-walls level)
  (grid-for-each
   (lambda (pos) (smooth-single-wall pos level))
   level))
(define (smooth-single-wall pos level)
  (let ((cell (grid-ref-check level pos)))
    (when (wall? cell)
      (match-let
       ([(list up down left right up-left down-left up-right down-right)
         (map (lambda (p) (grid-ref-check level p))
              (eight-directions pos))])
       (define (wall-or-door? c)
         (and (or (wall? c) (door? c))))
       ;; these don't count as walls for determining the shape of
       ;; neighboring walls
       (define (not-counting-as-wall? c)
         (or (not c)
             (void-cell? c)
             (walkable-cell? c)))
       (grid-set!
        level pos
        ((cond ((and (wall-or-door? up)   (wall-or-door? down)
                     (wall-or-door? left) (wall-or-door? right))
                new-four-corner-wall)
               ((and (wall-or-door? down)
                     (wall-or-door? left)
                     (wall-or-door? right)
                     (not-counting-as-wall? up))
                new-north-tee-wall)
               ((and (wall-or-door? up)
                     (wall-or-door? left)
                     (wall-or-door? right)
                     (not-counting-as-wall? down))
                new-south-tee-wall)
               ((and (wall-or-door? up)
                     (wall-or-door? down)
                     (wall-or-door? right)
                     (not-counting-as-wall? left))
                new-west-tee-wall)
               ((and (wall-or-door? up)
                     (wall-or-door? down)
                     (wall-or-door? left)
                     (not-counting-as-wall? right))
                new-east-tee-wall)
               ((and (wall-or-door?  down)
                     (wall-or-door?  right))
                new-north-west-wall)
               ((and (wall-or-door?  down)
                     (wall-or-door?  left))
                new-north-east-wall)
               ((and (wall-or-door?  up)
                     (wall-or-door?  right))
                new-south-west-wall)
               ((and (wall-or-door?  up)
                     (wall-or-door?  left))
                new-south-east-wall)
               ((and (wall-or-door? up)
                     (wall-or-door? down))
                new-vertical-wall)
               ((and (wall-or-door? left)
                     (wall-or-door? right))
                new-horizontal-wall)
               (else
                new-pillar))))))))
