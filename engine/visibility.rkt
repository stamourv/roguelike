#lang racket

(require "../utilities/grid.rkt"
         "../utilities/display.rkt")
(require "cell.rkt")
(provide (all-defined-out))

(define (init-visibility g)
  (empty-grid (grid-height g) (grid-width g)
	      #:cell-fun (lambda (pos) 'unknown)))

;; draw a line between a and b, see if we hit any opaque objects
(define (line-of-sight? g a b [monsters-opaque? #f])
  (let ([los? #t])
    (trace-line (lambda (pos)
                  (let ([cell (grid-ref g pos)])
                    (when (and (not (equal? pos a)) ; not ourselves
                               (not (equal? pos b)) ; not the target
                               (opaque-cell? cell monsters-opaque?)) ; obstacle
                      (set! los? #f))))
                a b)
    los?))

(define (clear-shot? grid a b) (line-of-sight? grid a b #t))


(define (update-visibility! view floor-map pos)
  ;; set the fog of war
  (grid-for-each (lambda (pos)
                   (when (eq? (grid-ref view pos) 'visible)
                     (grid-set! view pos 'visited)))
                 view)

  ;; field of vision using shadow casting (spiral path FOV)
  ;; see roguebasin.roguelikedevelopment.org/index.php?title=Spiral_Path_FOV
  (let* ((g     floor-map)
         (x     (point-x    pos))
         (y     (point-y    pos)))
    (let loop ((queue (list pos)))
      (define (pass-light pos new)
        ;; enqueue cells depending on the orientation of new from pos
        (match-let
         ([pos-x (point-x pos)] [pos-y (point-y pos)]
          [new-x (point-x new)] [new-y (point-y new)]
          [(list north south west east) (four-directions new)])
         (cond ((< new-x pos-x) ; somewhere north
                (cond ((= new-y pos-y) (list east north west)) ; due north
                      ((< new-y pos-y) (list north west))      ; north-west
                      ((> new-y pos-y) (list east north))))    ; north-east
               ((> new-x pos-x) ; somewhere south
                (cond ((= new-y pos-y) (list west south east)) ; due south
                      ((< new-y pos-y) (list west south))      ; south-west
                      ((> new-y pos-y) (list south east))))    ; south-east
               ((< new-y pos-y) (list north west south))       ; due west
               ((> new-y pos-y) (list south east north))       ; due east
                 (else ; we are at the starting point
                  (list east north west south)))))
	(when (not (null? queue))
          (let ((new (car queue)))
            (when (and (inside-grid? view new)
                       (not (eq? (grid-ref view new)
                                 'visible)) ; already seen
                       (<= (distance pos new) 7) ; within range
                       ;; do we have line of sight ? helps restrict the
                       ;; visibility down to a reasonable level
                       ;; note: line of sight is not necessary to see walls,
                       ;; this gives better results
                       (or (opaque-cell? (grid-ref g new) #f)
                           (line-of-sight? g pos new)))
              (grid-set! view new 'visible) ; mark as lit
              (when (not (opaque-cell? (grid-ref g new) #f))
                (loop (append (cdr queue)
                              (pass-light pos new)))))
            (loop (cdr queue)))))

      ;; one last pass to solve the problem case of walls that are hard to
      ;; see, which gives ugly results
      ;; to solve the problem, any wall next to a visible square is visible
      (grid-for-each
       (lambda (pos)
	 (when (and (opaque-cell? (grid-ref g pos) #f)
                    (eq? (grid-ref view pos) 'unknown)
                    (ormap
                     (lambda (new)
                       (and (not (opaque-cell? (grid-ref-check g new) #f))
                            (eq? (grid-ref-check view new) 'visible)))
                     (eight-directions pos)))
           (grid-set! view pos 'visited)))
       view)))


(define (visibility-show view map)
  (lambda (pos cell)
    ;; visibility for walls that consider only seen walls
    ;; if we have a 4-corner wall or a T wall, show it differently
    ;; depending on whether the neighbouring walls are known or not
    (define (visited? x (edge-ok? #t))
      (let ((v (grid-ref-check view x)))
	(and (or (eq? v 'visited) (eq? v 'visible) (and edge-ok? (not v)))
	     (not (wall? (grid-ref-check map x))))))
    (match-let
     ([(list up down left right up-left down-left up-right down-right)
       (eight-directions pos)])
     (let [(cell  (if (or (four-corner-wall? cell) (tee-wall? cell))
		      ((cond ((four-corner-wall? cell)
			      (cond ((or (>= (+ (if (visited? up-left)    1 0)
                                                (if (visited? up-right)   1 0)
                                                (if (visited? down-left)  1 0)
                                                (if (visited? down-right) 1 0))
                                             3) ; at least 3 corners are seen
                                         ;; or two opposed corners
                                         (and (visited? up-left)
                                              (visited? down-right))
                                         (and (visited? down-left)
                                              (visited? up-right))
                                         (andmap visited? ; or the 4 walls
                                                 (list up down left right)))
				     make-four-corner-wall)
				    ((and (visited? down-left)
					  (visited? down-right))
				     make-north-tee-wall)
				    ((and (visited? up-left)
					  (visited? up-right))
				     make-south-tee-wall)
				    ((and (visited? up-right)
					  (visited? down-right))
				     make-west-tee-wall)
				    ((and (visited? up-left)
					  (visited? down-left))
				     make-east-tee-wall)
				    ((visited? up-left)
				     make-south-east-wall)
				    ((visited? up-right)
				     make-south-west-wall)
				    ((visited? down-left)
				     make-north-east-wall)
				    ((visited? down-right)
				     make-north-west-wall)
				    (else
				     make-four-corner-wall)))
			     ((north-tee-wall? cell)
			      (cond ((>= (+ (if (visited? up #f)      1 0)
					    (if (visited? down-left)  1 0)
					    (if (visited? down-right) 1 0))
					 2)
				     make-north-tee-wall)
				    ((visited? down-left)
				     make-north-east-wall)
				    ((visited? down-right)
				     make-north-west-wall)
				    ((or (visited? up)
					 (visited? up-left)
					 (visited? up-right))
				     make-horizontal-wall)
				    (else
				     make-north-tee-wall)))
			     ((south-tee-wall? cell)
			      (cond ((>= (+ (if (visited? down #f)  1 0)
					    (if (visited? up-left)  1 0)
					    (if (visited? up-right) 1 0))
					 2)
				     make-south-tee-wall)
				    ((visited? up-left)
				     make-south-east-wall)
				    ((visited? up-right)
				     make-south-west-wall)
				    ((or (visited? down)
					 (visited? down-left)
					 (visited? down-right))
				     make-horizontal-wall)
				    (else
				     make-south-tee-wall)))
			     ((east-tee-wall? cell)
			      (cond ((>= (+ (if (visited? right #f)  1 0)
					    (if (visited? up-left)   1 0)
					    (if (visited? down-left) 1 0))
					 2)
				     make-east-tee-wall)
				    ((visited? up-left)
				     make-south-east-wall)
				    ((visited? down-left)
				     make-north-east-wall)
				    ((or (visited? right)
					 (visited? up-right)
					 (visited? down-right))
				     make-vertical-wall)
				    (else
				     make-east-tee-wall)))
			     ((west-tee-wall? cell)
			      (cond ((>= (+ (if (visited? left #f)    1 0)
					    (if (visited? up-right)   1 0)
					    (if (visited? down-right) 1 0))
					 2)
				     make-west-tee-wall)
				    ((visited? up-right)
				     make-south-west-wall)
				    ((visited? down-right)
				     make-north-west-wall)
				    ((or (visited? left)
					 (visited? up-left)
					 (visited? down-left))
				     make-vertical-wall)
				    (else
				     make-west-tee-wall))))
		       (cell-items cell) (cell-occupant cell))
		      cell))]
       (let ((c (show cell)))
         (case (grid-ref view pos)
           ((visible)
            (if (opaque-cell? cell #f)
                (print-sprite (darken-sprite  c))
                (print-sprite (lighten-sprite c))))
           ((visited)
            ;; (terminal-print c bg: 'black fg: 'white)
            ;; these are the default colors of the terminal, and not having to
            ;; print the control characters speeds up the game
            ;; we don't show enemies if they would be in the fog of war
            (cond ((cell-occupant cell) =>
                   (lambda (occ)
                     (set-cell-occupant! cell #f)
                     (print-sprite (show cell))
                     (set-cell-occupant! cell occ)))
                  (else (print-sprite c)))) ; no enemy to hide
           ((unknown)
            (display " "))))))))
