#lang racket

(require unstable/function)
(require "character.rkt")

(provide (all-defined-out))

(define-struct encounter-type
  (points
   monsters ; list of functions that create monsters
   can-be-placed?) ; takes a room as parameter
  #:mutable #:transparent)

(define-struct encounter (encounter-type monsters) #:transparent)
(define (new-encounter encounter-type) ; actually creates the monsters
  (make-encounter encounter-type
		  (map call (encounter-type-monsters encounter-type))))
(define (encounter-points e)
  (apply + (map character-level (encounter-monsters e))))

(define (new-encounter-type
         monsters
         [restriction (lambda (x) #t)])
  (let* ([encounter-type  (make-encounter-type
                           #f
                           monsters
                           restriction)]
         [dummy-encounter (new-encounter encounter-type)])
    (set-encounter-type-points! encounter-type
                                (encounter-points dummy-encounter))
    encounter-type))
