#lang racket

(require (only-in srfi/1 iota))
(require "utilities.rkt")
(require "../utilities/random.rkt"
         "../utilities/grid.rkt")
(require "../engine/cell.rkt"
         "../engine/floor.rkt"
         "../engine/items.rkt")
(require "../data/items.rkt")
(provide place-treasure show-treasure)

;; knobs

(define treasure-class-probs
  `((0.43 ,@weapon-table)
    (0.25 ,@shield-table)
    (0.22 ,@body-armor-table)
    (0.1  ,@potion-table)))

(define (individual-gp-cap    level) (* 10 (expt level 2)))
(define (individual-gp-bottom level) (* 2  (expt level 2)))
(define (total-gp             level) (* 30 (expt level 1.5)))

(define min-nb-chests 4)
(define max-nb-chests 8)


;; contains the probability of each kind of item, and the probability of each
;; item within each category
(define treasure-table (normalize-probability-table treasure-class-probs))

(define (possible-treasure no)
  (let* ((treasure-cap    (individual-gp-cap no))
	 (treasure-bottom (max (individual-gp-bottom no)
			       (foldl
				min
				treasure-cap
				(map (lambda (i) (item-gp-value ((cdr i))))
				     (apply append
					    (map cdr treasure-table)))))))
    (map (lambda (cat)
	   ;; keep only the items that respect the conditions
	   (let* ((new-items
		   (filter (lambda (i)
			     (let ((value (item-gp-value ((cdr i)))))
			       (and (>= value treasure-bottom)
				    (<= value treasure-cap))))
			   (cdr cat)))
		  (factor (apply + (map car new-items))))
	     ;; recalculate the probabilities
	     ;; note: the probability of each category remains unchanged
	     (cons (car cat) (normalize-probability-table new-items))))
	 treasure-table)))

(define (generate-treasure no)
  (let* ((treasure-points (total-gp no))
	 (possible        (possible-treasure no))
	 (actual-bottom (foldl min ; lowest value of the possible treasure
                               treasure-points ; generous upper bound
                               (map (lambda (i) (item-gp-value ((cdr i))))
                                    (apply append
                                           (map cdr possible))))))
    (if (andmap (lambda (new) (null? (cdr new))) possible)
        (generate-treasure (sub1 no))
        (let loop ([pts   treasure-points]
                   [items '()])
          (if (>= pts actual-bottom)
              (let* ([cat   (random-choice possible)]
                     [item  (and (not (null? cat)) ((random-choice cat)))]
                     [value (and item (item-gp-value item))])
                (if (and value (<= value pts))
                    (loop (- pts value) (cons item items))
                    (loop pts items))) ; try something else
              items)))))

(define (place-treasure floor no)
  ;; the number of chests is level number independent
  (let ((chests (map (lambda (x) (new-chest '())) ; will be filled later
		     (iota (random-between min-nb-chests max-nb-chests)))))
    ;; place the chests randomly on the map
    (let loop ((chests chests))
      (when (not (null? chests))
        (let ((pos (random-free-position floor)))
          (cond [(next-to-a-door? (floor-map floor) pos)
                 (loop chests)] ; try again
                [else
                 (grid-set! (floor-map floor) pos (car chests))
                 (set-floor-walkable-cells!
                  floor (remove pos (floor-walkable-cells floor)))
                 (loop (cdr chests))]))))
    ;; fill the chests
    (for-each (lambda (item) (add-item (random-element chests) item))
	      (generate-treasure no))))

;; for debugging purposes
(define (show-treasure no) (map item-name (generate-treasure no)))
