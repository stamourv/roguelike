#lang racket

(require "cell.rkt")
(require "../utilities/grid.rkt"
         "../utilities/random.rkt")
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
