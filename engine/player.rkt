#lang racket

(require racket/require)
(require (multi-in "../utilities" ("class.rkt" "grid.rkt" "terminal.rkt"
                                   "descriptions.rkt"))
         "cell.rkt"
         "floor.rkt"
         "scheduler.rkt"
         "character.rkt"
         "items.rkt"
         "visibility.rkt"
         "common.rkt"
         "../data/items.rkt"
         "../generation/generation.rkt")
(provide (all-defined-out))

(define-class <player-character> (character)
  floor-no
  floors-before ; listof player-floor
  current-floor ; player-floor
  floors-after  ; listof player-floor
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
    (flush-turn-queue)
    ;; no need to reschedule the player, since he will get rescheduled at the
    ;; end of his turn
    (for-each reschedule (floor-monsters floor))))


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


(define (rest)
  (cond [(not (null? (floor-monsters (character-floor player))))
         (displayln "There are still monsters nearby.")]
        [else
         (set-character-hp! player (character-max-hp player))
         (displayln "You have rested for 8 hours.")]))
