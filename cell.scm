(define-type cell
  printer    ; thunk that returns a character ;; TODO maybe not a thunk, but a function that receives bg and fg color ?
  extender: define-type-of-cell)

(define-type-of-cell walkable-cell
  object ;; TODO have a list instead
  occupant ; player, monster, ...
  extender: define-type-of-walkable-cell)
(define (walkable-cell-print cell char)
  (lambda () (cond ((get-occupant cell)
		    => (lambda (o) ((occupant-printer o))))
		   ((get-object cell)
		    => (lambda (o) ((object-printer o))))
		   (else char))))
(define (new-walkable-cell)
  (let ((cell (make-walkable-cell #f #f #f)))
    (cell-printer-set! cell (walkable-cell-print cell #\space))
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

;; TODO as it is, the stairs hides everything, even the player
(define-type-of-walkable-cell stairs
  extender: define-type-of-stairs)
(define-type-of-stairs stairs-up)
(define-type-of-stairs stairs-down)
(define (new-stairs f char)
  (let ((stairs (f #f #f #f)))
    (cell-printer-set! stairs (walkable-cell-print stairs char))
    stairs))
(define (new-stairs-up)   (new-stairs make-stairs-up   #\^))
(define (new-stairs-down) (new-stairs make-stairs-down #\v))


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

;; TODO other symbols ? silly for horizontal doors. if wall ever end up all being #, use - and |, or maybe for now use $ and _ for vertical doors and _ and something else for horizontal TODO see on the web what other people use
(define-type-of-wall door
  ;; open-fun takes the opened as parameter (to help for locked doors and co)
  ;; and returns whether the door can be opened
  open-fun
  extender: define-type-of-door)
(define (new-door)
  (let ((door (make-door (lambda () #\$) #f)))
    (door-open-fun-set! door (lambda (o) #t))
    door))
(define-type-of-walkable-cell open-door
  close-fun ; analogous to open-fun
  when-closed) ; the original closed door
(define (new-open-door orig)
  (let ((door (make-open-door #f #f #f #f orig)))
    (cell-printer-set! door (walkable-cell-print door #\_))
    (open-door-close-fun-set! door (lambda (o) #t))
    door))
(define (open-door  grid pos opener)
  (let ((door (grid-get grid pos)))
    (if ((door-open-fun door) opener)
	(begin (grid-set! grid pos (new-open-door door))
	       (display "Door opened.\n")
	       #t)
	#f)))
(define (close-door grid pos closer)
  (let ((door (grid-get grid pos)))
    (if ((open-door-close-fun door) closer)
	(begin (grid-set! grid pos (open-door-when-closed door))
	       (display "Door closed.\n")
	       #t)
	#f)))

(define (opaque? cell) (or (wall? cell))) ;; TODO add as other opaque cell types are added
