#lang racket

(require "../utilities/class.rkt"
         "../utilities/grid.rkt"
         "../utilities/terminal.rkt"
         "../utilities/descriptions.rkt")
(require "cell.rkt"
         "floor.rkt"
         "scheduler.rkt"
         "character.rkt"
         "items.rkt"
         "visibility.rkt"
         "common.rkt")
(require "../data/items.rkt")
(require "../generation/generation.rkt")
(provide (all-defined-out))

(define-class <player-character> (character)
  floor-no
  floors-before ; pairs (map . view)
  current-floor
  floors-after
  experience
  inventory) ; list of items
(define (new-player name)
  (set-player!
   (make-player-character
    name #f #f
    16 14 14 10 10 10 (make-hash) 0
    1 '(10) ; hit dice
    #f #f
    1 #f 1  ; base and current attack bonus, nb attacks
    6       ; speed, 6 seconds for a turn
    (new-equipment #:main-hand (new-club))
    ;; player-character attributes:
    1 '() #f '()
    0 (list)))
  (init-hp player #t) ; the player gets maxed out hp
  (place-player player (new-player-floor (player-character-floor-no player))))
(add-show-method struct:player-character 'player #\@ "You.")

(define-struct player-floor
  (floor ; views are a grid of either visible, visited or unknown
   view)
  #:transparent)
(define (new-player-floor no)
  ;; if we're not generating the first floor, generate the stairs up from the
  ;; new floor where the stairs down of the previous floor are
  (let ((floor (generate-floor no (character-level player)
                               (and player (character-pos player)))))
    (make-player-floor floor (init-visibility (floor-map floor)))))


(define (player-map   player)
  (floor-map (character-floor player)))
(define (player-view  player)
  (player-floor-view (player-character-current-floor player)))

(define (place-player
	 player player-floor
	 #:start-pos (start-pos (floor-stairs-up
                                 (player-floor-floor player-floor))))
  (let* ((floor (player-floor-floor player-floor))
	 (map   (floor-map floor)))
    (set-cell-occupant!                  (grid-ref map start-pos) player)
    (set-character-pos!                  player start-pos)
    (set-player-character-current-floor! player player-floor)
    (set-character-floor!                player floor)
    (reset-turn-no)
    (reset-turn-id)
    (reset-turn-queue)
    ;; no need to reschedule the player, since he will get rescheduled at the
    ;; end of his turn
    (for-each reschedule (floor-monsters floor))))


(define (update-visibility)
  ;; set the fog of war
  (let ((view (player-view player))
	(pos  (character-pos player)))
    (grid-for-each (lambda (pos)
		     (when (eq? (grid-ref view pos) 'visible)
                       (grid-set! view pos 'visited)))
		   view)

    ;; field of vision using shadow casting (spiral path FOV)
    ;; see roguebasin.roguelikedevelopment.org/index.php?title=Spiral_Path_FOV
    (let* ((g     (player-map player))
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
       view))))


(define (add-monster-experience m)
  (let* ((challenge     (character-level m))
         (xp-same-level (* challenge 300))
         (delta-level   (- challenge (character-level player))))
    (add-experience (if (= delta-level 0)
                        xp-same-level
                        (max 0
                             (ceiling (* xp-same-level
                                         (+ 1 (* 1/3 delta-level)))))))))
(define (add-experience xp)
  (let ((total (+ (player-character-experience player) xp))
	(level (character-level player)))
    (set-player-character-experience! player total)
    (when (>= total (* 1000 (/ (* level (+ level 1)) 2)))
      ;; 1000, 3000, 6000, ...
      (level-up))))

(define (level-up)
  (display "Level up!\n")
  (let* ((old-level (character-level player))
	 (new-level (+ old-level 1)))
    (set-character-level! player new-level)
    (let ((hd (character-hit-dice player)))
      (set-character-hit-dice! player (cons (car hd) hd))
      (init-hp player #t))
    (set-character-base-attack-bonus!
     player (+ (character-base-attack-bonus player) 1))
    (when (= (modulo old-level 5) 0) ; 6, 11, ...
      ;; add a new attack
      (set-character-nb-attacks! player (+ (character-nb-attacks player) 1)))))


;; inventory actions
(define (inventory-pick-up item)
  (remove-item (grid-ref (player-map player) (character-pos player))
               item)
  (set-player-character-inventory!
   player (cons item (player-character-inventory player))))
(define (inventory-drop item)
  (set-player-character-inventory!
   player (remove item (player-character-inventory player)))
  (add-item (grid-ref (player-map player) (character-pos player))
            item))
(define (inventory-equip item)
  (let* ([place (cond ((weapon?     item) 'main-hand)
                      ((shield?     item) 'off-hand)
                      ((body-armor? item) 'torso))]
         [e     (character-equipment player)]
         [old   ((case place
                   ((main-hand) equipment-main-hand)
                   ((off-hand)  equipment-off-hand)
                   ((torso)     equipment-torso))
                 e)])
    (define (back-in-inventory o)
      (printf "Put ~a back in inventory.\n" (item-name o))
      (set-player-character-inventory!
       player (cons o (player-character-inventory player))))
    (set-player-character-inventory!
     player (remove item (player-character-inventory player)))
    ((case place
       ((main-hand) set-equipment-main-hand!)
       ((off-hand)  set-equipment-off-hand!)
       ((torso)     set-equipment-torso!))
     e item)
    (cond ((and old (not (off-hand-placeholder? old)))
           (back-in-inventory old))
          ((two-handed-weapon? old)
           (set-equipment-off-hand! e #f)) ; remove the placeholder
          ((off-hand-placeholder? old)
           ;; we have to remove the two-handed weapon itself
           (back-in-inventory (equipment-main-hand e))
           (set-equipment-main-hand! e #f)))
    (when (two-handed-weapon? item)
      (let ((old-off (equipment-off-hand e)))
        (when (and old-off (not (off-hand-placeholder? old-off)))
          (back-in-inventory old-off))
        (set-equipment-off-hand! e (new-off-hand-placeholder))))))
(define (inventory-take-off item)
  (let ([e (character-equipment player)])
    (cond ((weapon?     item)
           (set-equipment-main-hand! e #f)
           (when (two-handed-weapon? item)
             ;; remove the placeholder
             (set-equipment-off-hand! e #f)))
          ((shield?     item)
           (set-equipment-off-hand!  e #f))
          ((body-armor? item)
           (set-equipment-torso!     e #f)))
    (set-player-character-inventory!
     player (cons item (player-character-inventory player)))))
(define (inventory-remove item)
  (set-player-character-inventory!
   player (remove item (player-character-inventory player))))


(define (go-up-stairs)
  (let ([current (player-character-current-floor player)]
        [before  (player-character-floors-before player)]
        [after   (player-character-floors-after  player)])
    (cond [(not (null? before))
           (let ([new (car before)])
             (place-player
              player new
              #:start-pos (floor-stairs-down
                           (player-floor-floor new)))
             (set-player-character-floor-no!
              player (sub1 (player-character-floor-no player)))
             (set-player-character-floors-after!
              player (cons current after))
             (set-player-character-floors-before!
              player (cdr before)))]
          [else (display "This would lead to the surface.\n")])))
(define (go-down-stairs)
  (let ([current (player-character-current-floor player)]
        [before  (player-character-floors-before player)]
        [after   (player-character-floors-after  player)])
    (set-player-character-floor-no!
     player (add1 (player-character-floor-no player)))
    (set-player-character-floors-before! player (cons current before))
    (if (null? after)
        (place-player player
                      (new-player-floor
                       (player-character-floor-no player)))
        (begin (place-player player (car after))
               (set-player-character-floors-after!
                player (cdr after))))))
