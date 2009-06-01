(define-type cell
  printer    ; thunk that returns a character ;; TODO maybe not a thunk, but a function that receives bg and fg color ?
  extender: define-type-of-cell)

(define-type-of-cell walkable-cell
  objects
  occupant ; player, monster, ...
  extender: define-type-of-walkable-cell)
(define (walkable-cell-print cell char)
  (lambda () (cond ((get-occupant cell)
		    => (lambda (o) ((character-printer o))))
		   ((not (null? (get-objects cell)))
		    ((object-printer (car (get-objects cell)))))
		   (else char))))
(define (new-walkable-cell)
  (let ((cell (make-walkable-cell #f '() #f)))
    (cell-printer-set! cell (walkable-cell-print cell #\space))
    cell))
(define (get-objects cell)
  (if (walkable-cell? cell)
      (walkable-cell-objects cell)
      #f))
(define (add-object cell object)
  (if (walkable-cell? cell)
      (walkable-cell-objects-set! cell (cons object (get-objects cell)))
      #f))
(define (remove-object cell object)
  (if (walkable-cell? cell)
      (walkable-cell-objects-set! cell (remove object (get-objects cell)))
      #f))
(define (get-occupant cell)
  (if (walkable-cell? cell)
      (walkable-cell-occupant cell)
      #f))
(define (occupant-set! cell occupant)
  (if (walkable-cell? cell)
      (walkable-cell-occupant-set! cell occupant)
      #f))


;; TODO as it is, the stairs hides everything, even the player
(define-type-of-walkable-cell stairs
  extender: define-type-of-stairs)
(define-type-of-stairs stairs-up)
(define-type-of-stairs stairs-down)
(define (new-stairs f char)
  (let ((stairs (f #f '() #f)))
    (cell-printer-set! stairs (walkable-cell-print stairs char))
    stairs))
(define (new-stairs-up)   (new-stairs make-stairs-up   #\<))
(define (new-stairs-down) (new-stairs make-stairs-down #\>))


(define-type-of-cell wall
  extender: define-type-of-wall)
(define-type-of-wall vertical-wall)
(define-type-of-wall horizontal-wall)
(define-type-of-wall corner-wall)
(define-type-of-wall solid-wall)
(define-type-of-wall pillar)
(define (new-wall) (make-wall (lambda () #\#)))
(define (new-vertical-wall)   (make-vertical-wall   (lambda () #\|)))
(define (new-horizontal-wall) (make-horizontal-wall (lambda () #\-)))
(define (new-corner-wall)     (make-corner-wall     (lambda () #\+)))
(define (new-solid-wall)      (make-solid-wall      (lambda () #\+)))
(define (new-pillar)          (make-pillar          (lambda () #\+)))

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
  (let ((door (make-open-door #f '() #f #f orig)))
    (cell-printer-set! door (walkable-cell-print door #\_))
    (open-door-close-fun-set! door (lambda (c) #t))
    door))
(define (open-door  grid pos opener)
  (let ((door (grid-get grid pos)))
    (if ((door-open-fun door) opener)
	(begin (grid-set! grid pos (new-open-door door))
	       (display "Door opened.\n")
	       #t)
	#f)))
(define (close-door grid pos closer) ;; TODO closing the door would remove any items underneath. maybe have a link to the opened door, which has the objects ?
  (let ((door (grid-get grid pos)))
    (if ((open-door-close-fun door) closer)
	(begin (grid-set! grid pos (open-door-when-closed door))
	       (display "Door closed.\n")
	       #t)
	#f)))

(define-type-of-wall chest ;; TODO abstract common parts with doors ?
  open-fun
  contents)
(define (new-chest contents)
  (let ((chest (make-chest (lambda () #\#) #f contents)))
    (chest-open-fun-set! chest (lambda (o) #t))
    chest))
(define-type-of-walkable-cell open-chest
  close-fun)
(define (new-open-chest contents)
  (let ((chest (make-open-chest #f contents #f #f))) ;; TODO for now, chests can't be closed
    (cell-printer-set! chest (walkable-cell-print chest #\=))
    (open-chest-close-fun-set! chest (lambda (c) #t))
    chest))
(define (open-chest  grid pos opener)
  (let ((chest (grid-get grid pos)))
    (if ((chest-open-fun chest) opener)
	(begin (grid-set! grid pos (new-open-chest (chest-contents chest)))
	       (display "Chest opened.\n")
	       #t)
	#f)))

(define (free-cell? cell)
  (and (walkable-cell? cell)
       (not (get-occupant cell)) ;; TODO could be interesting to see over monsters (espescially for other monsters), but disabling this gives weird results
       ))
