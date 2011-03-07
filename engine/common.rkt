#lang racket

(provide (all-defined-out))

(define player #f)
(define (set-player! p)
  (set! player p))


(define god-mode? (box #f)) ; for debugging
