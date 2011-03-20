#lang racket

(provide (all-defined-out))

(define (random-element l)
  (list-ref l (random (length l))))
(define (random-choice l) ; list of pairs (prob . x)
  (let loop ((r (random))
	     (l l))
    (cond ((null? l)      (error "invalid probability distribution"))
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
