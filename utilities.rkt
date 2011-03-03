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

;; disjoint sets : http://en.wikipedia.org/wiki/Disjoint-set_data_structure
(define-struct set (parent rank) #:mutable #:transparent)
(define (new-set) (make-set (gensym) 0))
(define (set-find x)
  (if (symbol? (set-parent x)) ; we hit the gensym, we're at the root
      x
      (begin (set-set-parent! x (set-find (set-parent x)))
	     (set-parent x))))
(define (set-union x y)
  (let* ((x-root (set-find x))
	 (x-rank (set-rank x-root))
	 (y-root (set-find y))
	 (y-rank (set-rank y-root)))
    (cond ((> x-rank y-rank) (set-set-parent! y-root x-root) x)
	  ((> y-rank x-rank) (set-set-parent! x-root y-root) y)
	  (else              (set-set-parent! y-root x-root)
			     (set-set-rank!   x-root (+ (set-rank x-root) 1))
			     x))))
(define (set-equal? x y)
  (equal? (set-find x) (set-find y)))


(define (remove-at-index l i)
  (append (take l i) (drop l (add1 i))))


(define (random-element l)
  (list-ref l (random (length l))))
(define (random-choice l) ; list of pairs (prob . x)
  (let loop ((r (random))
	     (l l))
    (cond ((null? l)      #f) ; shouldn't happen
	  ((< r (caar l)) (cdar l))
	  (else           (loop (- r (caar l)) (cdr l))))))
(define (random-boolean (p 0.5)) (< (random) p))
(define (random-between x y) ; between x and y inclusively
  (+ x (random (+ 1 (- y x)))))

;; returns a thunk that simulated the requested dice roll
(define (dice . kinds)
  (lambda ()
    (apply + (map (lambda (new) (+ (random new) 1)) kinds))))


(define (group-by-identical l)
  (let loop ((l   (sort l <))
	     (acc '())
	     (res '()))
    (if (null? l)
	(cons acc res)
	(if (or (null? acc)
		(equal? (car l) (car acc)))
	    (loop (cdr l)
		  (cons (car l) acc)
		  res)
	    (loop (cdr l)
		  (list (car l))
		  (cons acc res))))))

(define (show-dice l)
  (let loop ((l  (group-by-identical l))
	     (s  "")
	     (+? #f))
    (if (null? l)
	s
	(loop (cdr l)
              (format "~a~a~a~a"
                      s (if +? " + " "") (length (car l))
                      (if (= (caar l) 1) ; we don't need the d1 of Xd1
                          ""
                          (format "d~a" (caar l))))
	      #t))))
