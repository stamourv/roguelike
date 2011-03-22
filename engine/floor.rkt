#lang racket

(require "cell.rkt")
(require "../utilities/grid.rkt"
         "../utilities/random.rkt")
(provide (all-defined-out))

(define-struct floor
  (map
   rooms
   stairs-up
   stairs-down
   walkable-cells
   monsters)
  #:mutable #:transparent)
