#lang racket

(provide (all-defined-out))

(define n-levels 3) ;; TODO change

(define player #f) ; needed for level-generation
(define (set-player! p)
  (set! player p))


(define god-mode? (box #f)) ; for debugging
