#lang racket

(provide (all-defined-out))

(define n-levels 3) ;; TODO change

(define player #f) ; needed for level-generation
(define (set-player! p)
  (set! player p))

;; list of pairs (name . score), sorted by descending order of score
(define hall-of-fame
  (box (with-input-from-file "hall-of-fame" read)))

(define god-mode? (box #f)) ; for debugging
