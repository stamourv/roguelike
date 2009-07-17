(import class)

(define-generic print) ; used for objects, cells, etc. ;; TODO receive bg and fg color ? or visibility ? as optional arguments, if methods can have them ?
(define-method (print o) #\space)

(define n-levels 3) ;; TODO change
(define player #f) ; needed for level-generation

;; list of pairs (name . score), sorted by descending order of score
(define hall-of-fame
  (let ((hall (read-all (open-file (list path:   "hall-of-fame"
					 create: 'maybe)))))
    (if (null? hall)
	'()
	(car hall))))
