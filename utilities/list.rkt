#lang racket

(provide (all-defined-out))

(define (repeat n l) (apply append (make-list n l)))

(define (remove-at-index l i)
  (append (take l i) (drop l (add1 i))))
