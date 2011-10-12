#lang racket

(require racket/require)
(require "cell.rkt"
         (multi-in "../utilities" ("grid.rkt" "random.rkt")))
(provide (all-defined-out))

(define-struct floor
  (map
   rooms
   stairs-up
   stairs-down
   walkable-cells
   monsters)
  #:mutable #:transparent)
