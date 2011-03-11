#lang racket

(provide (all-defined-out))

(define (repeat n l) (apply append (make-list n l)))

(define-syntax-rule (take! l n)
  (let loop ((tmp l)
             (i   n)
             (res '()))
    (if (< i 1)
        (begin (set! l tmp) (reverse res))
        (loop (cdr tmp)
               (- i 1)
               (cons (car tmp) res)))))


(define (remove-at-index l i)
  (append (take l i) (drop l (add1 i))))
