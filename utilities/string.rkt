#lang racket

(provide (all-defined-out))

(define (upcase-word s)
  (format "~a~a"
          (char-upcase (string-ref s 0))
          (substring s 1)))
