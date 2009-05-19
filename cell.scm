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
(define-type-of-walkable-cell stairs
  extender: define-type-of-stairs)
(define-type-of-stairs stairs-up)
(define-type-of-stairs stairs-down)
(define (new-stairs-up)   (make-stairs-up   (lambda () #\^) #f #f))
(define (new-stairs-down) (make-stairs-down (lambda () #\v) #f #f))


(define-type-of-cell wall
  extender: define-type-of-wall)
(define-type-of-wall vertical-wall)
(define-type-of-wall horizontal-wall)
(define-type-of-wall corner-wall)
(define-type-of-wall solid-wall)
(define (new-wall) (make-wall (lambda () #\#)))
(define (new-vertical-wall)   (make-vertical-wall   (lambda () #\|)))
(define (new-horizontal-wall) (make-horizontal-wall (lambda () #\-)))
(define (new-corner-wall)     (make-corner-wall     (lambda () #\+)))
(define (new-solid-wall)      (make-solid-wall      (lambda () #\#)))

(define-type-of-cell door
  open?
  open-fun ; these 2 take the opener as parameter (to help for locked doors)
  close-fun
  extender: define-type-of-door)
(define (new-door)
  (let ((door (make-door #f #f #f #f)))
    (cell-printer-set! door (lambda () (if (door-open? door) #\_ #\$))) ;; TODO other symbols ? silly for horizontal doors. if wall ever end up all being #, use - and |, or maybe for now use $ and _ for vertical doors and _ and something else for horizontal TODO see on the web what other people use
    (door-open-fun-set!  door (lambda (o) (door-open?-set! door #t)))
    (door-close-fun-set! door (lambda (o) (door-open?-set! door #f)))
    door))

(define (opaque? cell) (or (wall? cell) (door? cell))) ;; TODO add as other opaque cell types are added
