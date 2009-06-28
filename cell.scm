(define-class cell ()
  (slot: printer)   ; thunk that returns a character ;; TODO maybe not a thunk, but a function that receives bg and fg color ? or visibility ?
  (slot: objects)
  (slot: occupant)) ; player, monster, ...

(define (new-cell f c)
  (f (lambda () c) '() #f))

(define (add-object cell object)
  (cell-objects-set! cell (cons object (cell-objects cell))))
(define (remove-object cell object)
  (cell-objects-set! cell (remove object (cell-objects cell))))

(define-generic walkable-cell?)
(define-method (walkable-cell? c) #f)
(define (free-cell? cell)
  (and (walkable-cell? cell)
       (not (cell-occupant cell))))

(define-generic opaque-cell?)
(define-method (opaque-cell? c) #f)


(define-class empty-cell (cell))
(define (walkable-cell-print cell char)
  (lambda () (cond ((cell-occupant cell)
		    => (lambda (o) ((character-printer o))))
		   ((not (null? (cell-objects cell)))
		    ((object-printer (car (cell-objects cell)))))
		   (else char))))
(define (new-empty-cell) ;; TODO use constructors
  (let ((cell (make-empty-cell #f '() #f)))
    (cell-printer-set! cell (walkable-cell-print cell #\space))
    cell))
(define-method (walkable-cell? (c empty-cell)) #t)


(define-class stairs (empty-cell))
(define-class stairs-up   (stairs))
(define-class stairs-down (stairs))
(define (new-stairs f char)
  (let ((stairs (f #f '() #f)))
    (cell-printer-set! stairs (walkable-cell-print stairs char))
    stairs))
(define (new-stairs-up)   (new-stairs make-stairs-up   #\<))
(define (new-stairs-down) (new-stairs make-stairs-down #\>))


(define-class wall (cell))
(define-method (opaque-cell? (c wall)) #t)
(define-class vertical-wall   (wall))
(define-class horizontal-wall (wall))
(define-class corner-wall     (wall))
(define-class solid-wall      (wall))
(define-class pillar          (wall))
(define (new-wall)            (new-cell make-wall            #\#))
(define (new-vertical-wall)   (new-cell make-vertical-wall   #\|))
(define (new-horizontal-wall) (new-cell make-horizontal-wall #\-))
(define (new-corner-wall)     (new-cell make-corner-wall     #\+))
(define (new-solid-wall)      (new-cell make-solid-wall      #\+))
(define (new-pillar)          (new-cell make-pillar          #\+))


(define-generic open)
(define-method (open  grid x opener) (display "I can't open that.\n"))
(define-generic close)
(define-method (close grid x closer) (display "I can't close that.\n"))


;; TODO other symbols ? silly for horizontal doors. if wall ever end up all being #, use - and |, or maybe for now use $ and _ for vertical doors and _ and something else for horizontal TODO see on the web what other people use
(define-class door (cell)
  (slot: open?))
(define closed-door-printer (lambda () #\$))
(define (new-door)
  (let ((door (make-door #f '() #f #f)))
    (cell-printer-set!
     door (lambda () (if (door-open? door)
			 ((walkable-cell-print door #\_))
			 #\$)))
    door))
(define-method (open grid (door door) opener)
  (if (door-open? door)
      (display "This door is already open.\n")
      (begin (door-open?-set! door #t)
	     (display "Door opened.\n"))))
(define-method (close grid (door door) closer)
  (if (not (door-open? door))
      (display "This door is already closed.\n")
      (begin (door-open?-set! door #f)
	     (display "Door closed.\n"))))
(define-method (walkable-cell? (c door))      (door-open? c))
(define-method (opaque-cell?   (c door)) (not (door-open? c)))


(define-class chest (cell)
  (slot: open?))
(define (new-chest contents)
  (let ((chest (make-chest #f contents #f #f)))
    (cell-printer-set!
     chest (lambda () (if (chest-open? chest)
			  ((walkable-cell-print chest #\=))
			  #\#)))
    chest))
(define-method (open grid (chest chest) opener)
  (if (chest-open? chest)
      (display "This chest is already open.\n")
      (begin (chest-open?-set! chest #t)
	     (display "Chest opened.\n"))))
(define-method  (walkable-cell? (c chest)) (chest-open? c))
