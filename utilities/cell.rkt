#lang racket

(require "class.rkt" "display.rkt" "utilities.rkt")
(provide (all-defined-out))

(define-class <cell> ()
  items
  occupant) ; player, monster, ...

(define (add-item cell item)
  (set-cell-items! cell (cons item (cell-items cell))))
(define (remove-item cell item)
  (set-cell-items! cell (remove item (cell-items cell))))

(define-generic walkable-cell?)
(define-method (walkable-cell? c) #f)
(define (free-cell? cell)
  (and (walkable-cell? cell)
       (not (cell-occupant cell))))

(define-generic opaque-cell?)
(define-method (opaque-cell? c occupants-opaque?)
  (and occupants-opaque? (cell-occupant c)))


(define (walkable-cell-show cell char)
  (cond ((cell-occupant cell)
	 => show)
	((not (null? (cell-items cell)))
	 (show (car (cell-items cell))))
	(else char)))

;; what the level initially starts as
;; doesn't have any properties
(define-class <void-cell> (cell))
(define (new-void-cell) (make-void-cell '() #f))
(define-method (show (c struct:void-cell)) "*") ; for debugging purposes

(define-class <empty-cell> (cell))
(define (new-empty-cell) (make-empty-cell '() #f))
(define-method (show (c struct:empty-cell)) (walkable-cell-show c #\space))
(define-method (walkable-cell? (c struct:empty-cell)) #t)


(define-class <stairs> (empty-cell))
(define-class <stairs-up>   (stairs))
(define (new-stairs-up)   (make-stairs-up   '() #f))
(define-method (show (c struct:stairs-up))   (walkable-cell-show c #\<))
(define-class <stairs-down> (stairs))
(define (new-stairs-down) (make-stairs-down '() #f))
(define-method (show (c struct:stairs-down)) (walkable-cell-show c #\>))


(define-class <wall> (cell))
(define-method (show (c struct:wall)) #\+)
(define-method (opaque-cell? (c struct:wall) occupants-opaque?) #t)

(define double-walls? #f)

;; TODO abstract w/ macro
(define-class <vertical-wall>   (wall))
(define (new-vertical-wall)   (make-vertical-wall   '() #f))
(define-method (show (c struct:vertical-wall))
  (if double-walls? #\u2551 #\u2502))
(define-class <horizontal-wall> (wall))
(define (new-horizontal-wall) (make-horizontal-wall '() #f))
(define-method (show (c struct:horizontal-wall))
  (if double-walls? #\u2550 #\u2500))

(define-class <four-corner-wall> (wall))
(define (new-four-corner-wall) (make-four-corner-wall '() #f))
(define-method (show (c struct:four-corner-wall))
  (if double-walls? #\u256c #\u253c))

(define-class <corner-wall> (wall))
(define-class <north-east-wall> (corner-wall))
(define (new-north-east-wall) (make-north-east-wall '() #f))
(define-method (show (c struct:north-east-wall))
  (if double-walls? #\u2557 #\u2510))
(define-class <north-west-wall> (corner-wall))
(define (new-north-west-wall) (make-north-west-wall '() #f))
(define-method (show (c struct:north-west-wall))
  (if double-walls? #\u2554 #\u250c))
(define-class <south-east-wall> (corner-wall))
(define (new-south-east-wall) (make-south-east-wall '() #f))
(define-method (show (c struct:south-east-wall))
  (if double-walls? #\u255d #\u2518))
(define-class <south-west-wall> (corner-wall))
(define (new-south-west-wall) (make-south-west-wall '() #f))
(define-method (show (c struct:south-west-wall))
  (if double-walls? #\u255a #\u2514))

(define-class <tee-wall> (wall))
(define-class <north-tee-wall> (tee-wall))
(define (new-north-tee-wall) (make-north-tee-wall '() #f))
(define-method (show (c struct:north-tee-wall))
  (if double-walls? #\u2566 #\u252c))
(define-class <south-tee-wall> (tee-wall))
(define (new-south-tee-wall) (make-south-tee-wall '() #f))
(define-method (show (c struct:south-tee-wall))
  (if double-walls? #\u2569 #\u2534))
(define-class <east-tee-wall>  (tee-wall))
(define (new-east-tee-wall)  (make-east-tee-wall  '() #f))
(define-method (show (c struct:east-tee-wall))
  (if double-walls? #\u2563 #\u2524))
(define-class <west-tee-wall>  (tee-wall))
(define (new-west-tee-wall)  (make-west-tee-wall  '() #f))
(define-method (show (c struct:west-tee-wall))
  (if double-walls? #\u2560 #\u251c))

(define-class <solid-wall> (wall))
(define (new-solid-wall) (make-solid-wall '() #f))
(define-class <pillar>     (wall))
(define (new-pillar)     (make-pillar     '() #f))


(define-generic open)
(define-method (open  grid x opener) (display "I can't open that.\n"))
(define-generic close)
(define-method (close grid x closer) (display "I can't close that.\n"))


(define-class <door> (cell) open?)
(define (new-door) (make-door '() #f #f))
(define-method (show (c struct:door))
  (if (door-open? c)
      (walkable-cell-show c #\_)
      #\$))
(define-method (open grid (door struct:door) opener)
  (if (door-open? door)
      (display "This door is already open.\n")
      (begin (set-door-open?! door #t)
	     (display "Door opened.\n"))))
(define-method (close grid (door struct:door) closer)
  (if (not (door-open? door))
      (display "This door is already closed.\n")
      (begin (set-door-open?! door #f)
	     (display "Door closed.\n"))))
(define-method (walkable-cell? (c struct:door)) (door-open? c))
(define-method (opaque-cell?   (c struct:door) occupants-opaque?)
  (or (not (door-open? c))
      (and occupants-opaque? (cell-occupant c)))) ;; TODO call-next-method?


(define-class <chest> (cell) open?)
(define (new-chest contents) (make-chest contents #f #f))
(define-method (show (c struct:chest))
  (if (chest-open? c)
      (walkable-cell-show c #\=)
      #\#))
(define-method (open grid (chest struct:chest) opener)
  (if (chest-open? chest)
      (display "This chest is already open.\n")
      (begin (set-chest-open?! chest #t)
	     (display "Chest opened.\n"))))
(define-method (walkable-cell? (c struct:chest)) (chest-open? c))

;; used to display special information, for instance when taking aim
(define-class <display-cell> (cell) char)
(define (new-display-cell c) (make-display-cell #f #f c))
(define-method (show (c struct:display-cell)) (display-cell-char c))
