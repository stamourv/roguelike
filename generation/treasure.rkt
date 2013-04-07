#lang racket

(require racket/require)
(require "utilities.rkt"
         (multi-in "../utilities" ("random.rkt" "grid.rkt"))
         (multi-in "../engine"    ("cell.rkt" "floor.rkt" "items.rkt"))
         "../data/items.rkt")
(provide place-treasure show-treasure)

;; knobs

(define treasure-class-probs
  `([0.43 ,@weapon-table]
    [0.25 ,@shield-table]
    [0.22 ,@body-armor-table]
    [0.1  ,@potion-table]))

(define (individual-gp-cap    level) (* 10 (expt level 2)))
(define (individual-gp-bottom level) (* 2  (expt level 2)))
(define (total-gp             level) (* 30 (expt level 1.5)))

(define min-nb-chests 4)
(define max-nb-chests 8)

(define endgame-level 1) ; level at which the endgame may be triggered ;; TODO higher
(define endgame-prob  1) ; chance the endgame is triggers on a given floor ;; TODO lower


;; contains the probability of each kind of item, and the probability of each
;; item within each category
(define treasure-table (normalize-probability-table treasure-class-probs))

(define (get-bottom lowest table)
  (for*/fold ([lowest lowest])
      ([cat (in-list table)]
       [i   (in-list (rest cat))])
    (min lowest (item-gp-value ((cdr i))))))

(define (possible-treasure no)
  (define treasure-cap    (individual-gp-cap no))
  (define treasure-bottom
    (max (individual-gp-bottom no)
         (get-bottom treasure-cap treasure-table)))
  (for/list ([cat (in-list treasure-table)])
    ;; keep only the items that respect the conditions
    (define new-items
      (for*/list ([i     (in-list (rest cat))]
                  [value (in-value (item-gp-value ((cdr i))))]
                  #:when (and (>= value treasure-bottom)
                              (<= value treasure-cap)))
        i))
    (define factor (apply + (map car new-items)))
    ;; recalculate the probabilities
    ;; note: the probability of each category remains unchanged
    (cons (car cat) (normalize-probability-table new-items))))

(define (generate-treasure no)
  (define treasure-points (total-gp no))
  (define possible        (possible-treasure no))
  (define actual-bottom   (get-bottom treasure-points possible))
  (if (andmap (lambda (new) (null? (cdr new))) possible)
      ;; no possible treasure, try cheaper treasure
      (generate-treasure (sub1 no))
      (let loop ([pts   treasure-points]
                 [items '()])
        (cond [(>= pts actual-bottom)
               (define cat   (random-choice possible))
               (define item  (and (not (null? cat)) ((random-choice cat))))
               (define value (and item (item-gp-value item)))
               (if (and value (<= value pts))
                   (loop (- pts value) (cons item items))
                   (loop pts items))]
              [else ; try something else
               items]))))

(define (place-treasure floor no level)
  ;; the number of chests is level number independent
  (define chests
    (for/list ([_ (range (random-between min-nb-chests max-nb-chests))])
      (new-chest '()))) ; will be filled later
  ;; place the chests randomly on the map
  (let loop ((chests chests))
    (when (not (null? chests))
      (define pos (random-free-position floor))
      (cond [(next-to-a-door? (floor-map floor) pos)
             (loop chests)] ; try again
            [else
             (grid-set! (floor-map floor) pos (car chests))
             (set-floor-walkable-cells!
              floor (remove pos (floor-walkable-cells floor)))
             (loop (cdr chests))])))
  ;; fill the chests
  (for ([item (in-list (generate-treasure no))])
    (add-item (random-element chests) item))
  ;; if the player is high-level enough, maybe generate endgame item
  (when (and (>= level endgame-level) (random-boolean endgame-prob))
    (add-item (random-element chests) endgame-amulet)))

;; for debugging purposes
(define (show-treasure no) (map item-name (generate-treasure no)))
