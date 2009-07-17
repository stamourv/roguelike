(import class)

(define (iota n)
  (let loop ((n (- n 1)) (l '()))
    (if (< n 0) l (loop (- n 1) (cons n l)))))

(define (fold f base lst)
  (if (null? lst)
      base
      (fold f (f base (car lst)) (cdr lst))))

(define (filter p l)
  (cond ((null? l)   '())
	((p (car l)) (cons (car l) (filter p (cdr l))))
	(else        (filter p (cdr l)))))

(define (repeat n l)
  (let loop ((n n) (res '()))
    (if (= n 0)
	res
	(loop (- n 1) (append res l)))))

(define (find p l)
  (cond ((null? l)   #f)
	((p (car l)) (car l))
	(else        (find p (cdr l)))))

(define (take l n)
  (if (< n 1)
      '()
      (cons (car l) (take (cdr l) (- n 1)))))

(define (call x) (x))

(define (identity x)  x)
(define (call     x) (x))

;; disjoint sets : http://en.wikipedia.org/wiki/Disjoint-set_data_structure
(define-type set
  parent
  rank)
(define (new-set) (make-set (gensym) 0))
(define (set-find x)
  (if (symbol? (set-parent x)) ; we hit the gensym, we're at the root
      x
      (begin (set-parent-set! x (set-find (set-parent x)))
	     (set-parent x))))
(define (set-union x y)
  (let* ((x-root (set-find x))
	 (x-rank (set-rank x-root))
	 (y-root (set-find y))
	 (y-rank (set-rank y-root)))
    (cond ((> x-rank y-rank) (set-parent-set! y-root x-root) x)
	  ((> y-rank x-rank) (set-parent-set! x-root y-root) y)
	  (else              (set-parent-set! y-root x-root)
			     (set-rank-set!   x-root (+ (set-rank x-root) 1))
			     x))))
(define (set-equal? x y)
  (equal? (set-find x) (set-find y)))


(define (remove-at-index l i)
  (let loop ((i i)
	     (l l)
	     (r '()))
    (if (= i 0)
	(append (reverse r) (cdr l))
	(loop (- i 1)
	      (cdr l)
	      (cons (car l) r)))))

(define (remove x lst)
  (cond ((null? lst)
         '())
        ((eq? x (car lst))
         (cdr lst))
        (else
         (cons (car lst)
               (remove x (cdr lst))))))

(define (randomize-list l)
  (let loop ((n (length l))
	     (l l)
	     (r '()))
    (if (= n 0)
	r
	(let ((i (random-integer n)))
	  (loop (- n 1)
		(remove-at-index l i)
		(cons (list-ref l i) r))))))

(define (random-element l)
  (list-ref l (random-integer (length l))))
(define (random-choice l) ; list of pairs (prob . x)
  (let loop ((r (random-real))
	     (l l))
    (cond ((null? l)      #f) ; shouldn't happen
	  ((< r (caar l)) (cdar l))
	  (else           (loop (- r (caar l)) (cdr l))))))
(define (random-between x y) ; between x and y inclusively
  (+ x (random-integer (+ 1 (- y x)))))

(define (split-string s delimiter) ; delimiter is a char
  (let loop ((s   (string->list s))
	     (acc '())
	     (res '()))
    (cond ((null? s)
	   (reverse (map (lambda (x) (list->string (reverse x)))
			 (if (null? acc) res (cons acc res)))))
	  ((eq? (car s) delimiter)
	   (loop (cdr s)
		 '()
		 (cons acc res)))
	  (else
	   (loop (cdr s)
		 (cons (car s) acc)
		 res)))))

;; returns a thunk that simulated the requested dice roll
(define (dice . kinds)
  (lambda ()
    (fold (lambda (acc new) (+ acc (random-integer new) 1)) 0 kinds)))

;; written by Marc Feeley
(define (sort-list l <?)

  (define (mergesort l)

    (define (merge l1 l2)
      (cond ((null? l1) l2)
            ((null? l2) l1)
            (else
             (let ((e1 (car l1)) (e2 (car l2)))
               (if (<? e1 e2)
                 (cons e1 (merge (cdr l1) l2))
                 (cons e2 (merge l1 (cdr l2))))))))

    (define (split l)
      (if (or (null? l) (null? (cdr l)))
        l
        (cons (car l) (split (cddr l)))))

    (if (or (null? l) (null? (cdr l)))
      l
      (let* ((l1 (mergesort (split l)))
             (l2 (mergesort (split (cdr l)))))
        (merge l1 l2))))

  (mergesort l))

(define (group-by-identical l)
  (let loop ((l   (sort-list l <))
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
	      (string-append
	       s
	       (if +? " + " "")
	       (number->string (length (car l)))
	       (if (= (caar l) 1) ; we don't need the d1 of Xd1
		   ""
		   (string-append "d" (number->string (caar l)))))
	      #t))))
