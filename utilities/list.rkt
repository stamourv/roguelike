#lang racket

(provide (all-defined-out))

(define (repeat n l) (apply append (make-list n l)))

(define (remove-at-index l i)
  (append (take l i) (drop l (add1 i))))

(define (cartesian-product l1 l2)
  (apply append
         (map (lambda (x) (map (lambda (y) (list x y))
                               l2))
              l1)))

(define (group-by-identical l comp #:key [key values])
  (let loop ([l   (sort l comp #:key key)]
             [acc '()]
             [res '()])
    (if (null? l)
        (reverse (cons (reverse acc) res))
        (if (or (null? acc)
                (equal? (key (car l)) (key (car acc))))
            (loop (cdr l)
                  (cons (car l) acc)
                  res)
            (loop (cdr l)
                  (list (car l))
                  (cons (reverse acc) res))))))
