#lang racket

(require "utilities/utilities.rkt"
         "utilities/class.rkt"
         "utilities/cell.rkt"
         "utilities/grid.rkt"
         "utilities/floor-utils.rkt"
         "utilities/terminal.rkt"
         "utilities/display.rkt")
(require "scheduler.rkt"
         "character.rkt"
         "objects.rkt"
         "items.rkt"
         "visibility.rkt"
         "common.rkt")
(require "generation/generation.rkt")
(provide (all-defined-out))

(define-class <player-character> (character)
  floor-no
  floors-before ; pairs (map . view)
  current-floor
  floors-after
  experience
  inventory) ; list of objects
(define (new-player name) ;; TODO constructor ?
  (set-player!
   (make-player-character
    name #f #f
    ;; TODO have a way to select (and also display, maybe press r
    ;;  for roster, c for character)
    16 14 14 10 10 10 (make-hash) 0
    1 '(10) ; hit dice
    #f #f
    1 #f 1  ; base and current attack bonus, nb attacks
    6       ; speed, 6 seconds for a turn
    (new-equipment #:main-hand (new-club))
    ;; player-character attributes:
    1 '() #f '()
    0 '()))
  (init-hp player #t) ; the player gets maxed out hp
  (place-player player (new-player-floor (player-character-floor-no player))))
(define-method (show (p struct:player-character)) #\@)

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
  ;; TODO maybe show visible parts in dark yellow instead of white background?
  ;;  to simulate a lantern
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
                       ;; TODO have range in a variable, maybe a player trait
                       ;;  (elves see farther?)
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

(define (level-up) ;; TODO have attribute upgrades, etc
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
