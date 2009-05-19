(define-type cell
  printer    ; thunk that returns a character ;; TODO maybe not a thunk, but a function that receives bg and fg color ?
  extender: define-type-of-cell)

(define-type-of-cell walkable-cell
  object ;; TODO have a list instead
  occupant ; player, monster, ...
  extender: define-type-of-walkable-cell)
(define (new-walkable-cell)
  (let ((cell (make-walkable-cell #f #f #f)))
    (cell-printer-set! cell (lambda ()
			      (cond ((get-occupant cell)
				     => (lambda (o) ((occupant-printer o))))
				    ((get-object cell)
				     => (lambda (o) ((object-printer o))))
				    (else #\space))))
    cell))
(define (get-object cell)
  (if (walkable-cell? cell)
      (walkable-cell-object cell)
      #f)) ;; TODO change with multiple objects
(define (add-object cell object)
  (if (walkable-cell? cell)
      (walkable-cell-object-set! cell object)
      #f)) ;; TODO change it when we have multiple objects
(define (remove-object cell object)
  (if (walkable-cell? cell)
      (walkable-cell-object-set! cell #f)
      #f)) ;; TODO change with multiple objects
(define (get-occupant cell)
  (if (walkable-cell? cell)
      (walkable-cell-occupant cell)
      #f))
(define (occupant-set! cell occupant)
  (if (walkable-cell? cell)
      (walkable-cell-occupant-set! cell occupant)
      #f))

(define-type object
  name
  printer
  extender: define-type-of-object)
(define-type-of-object treasure) ;; TODO more
(define (new-treasure) (make-treasure (random-element object-names)
				      (lambda () #\T)))
(define-type occupant
  name
  printer
  extender: define-type-of-occupant)

;; TODO as it is, any object can hide stairs
(define-type-of-walkable-cell stairs-cell
  extender: define-type-of-stairs-cell)
(define-type-of-stairs-cell stairs-up-cell)
(define-type-of-stairs-cell stairs-down-cell)
(define (new-stairs-up-cell)   (make-stairs-up-cell   (lambda () #\^) #f #f))
(define (new-stairs-down-cell) (make-stairs-down-cell (lambda () #\v) #f #f))


(define-type-of-cell wall-cell
  extender: define-type-of-wall-cell)
(define-type-of-wall-cell vertical-wall-cell)
(define-type-of-wall-cell horizontal-wall-cell)
(define-type-of-wall-cell corner-wall-cell)
(define-type-of-wall-cell solid-wall-cell)
(define (new-wall-cell) (make-wall-cell (lambda () #\#)))
(define (new-vertical-wall-cell)   (make-vertical-wall-cell   (lambda () #\|)))
(define (new-horizontal-wall-cell) (make-horizontal-wall-cell (lambda () #\-)))
(define (new-corner-wall-cell)     (make-corner-wall-cell     (lambda () #\+)))
(define (new-solid-wall-cell)      (make-solid-wall-cell      (lambda () #\#)))

(define-type-of-cell door-cell ;; TODO drop the door from the name ? same for wall ?
  open?
  open-fun ; these 2 take the opener as parameter (to help for locked doors)
  close-fun
  extender: define-type-of-door-cell)
(define (new-door-cell)
  (let ((door (make-door-cell #f #f #f #f)))
    (cell-printer-set! door (lambda () (if (door-cell-open? door) #\_ #\$))) ;; TODO other symbols ? silly for horizontal doors. if wall ever end up all being #, use - and |, or maybe for now use $ and _ for vertical doors and _ and something else for horizontal TODO see on the web what other people use
    (door-cell-open-fun-set!  door (lambda (o) (doorcell-open?-set! door #t)))
    (door-cell-close-fun-set! door (lambda (o) (doorcell-open?-set! door #f)))
    door))

(define (opaque-cell? cell) (or (wall-cell? cell) (door-cell? cell))) ;; TODO add as other opaque cell types are added
