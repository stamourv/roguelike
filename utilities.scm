(define (iota n)
  (let loop ((n (- n 1)) (l '()))
    (if (< n 0) l (loop (- n 1) (cons n l)))))


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

(define (terminal-command command)
  (display (string-append (list->string (list (integer->char #x1b))) command)))
(define (terminal-print text #!key (bg 'black) (fg 'white))
  (terminal-command (string-append "[" (case bg
					 ((black) "40") ((red)     "41")
					 ((green) "42") ((yellow)  "43")
					 ((blue)  "44") ((magenta) "45")
					 ((cyan)  "46") ((white)   "47"))
				   ";" (case fg
					 ((black) "30") ((red)     "31")
					 ((green) "32") ((yellow)  "33")
					 ((blue)  "34") ((magenta) "35")
					 ((cyan)  "36") ((white)   "37"))
				   "m"))
  (display text) ;; TODO dim doesn't seem to work, try the other effects (bold, underline, etc)
  (terminal-command "[0m")) ; reset
