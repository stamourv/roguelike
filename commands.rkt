#lang racket

(require (only-in srfi/1 iota))
(require "utilities/terminal.rkt"
         "utilities/grid.rkt"
         "utilities/cell.rkt")
(require "engine/character.rkt"
         "engine/player.rkt"
         "engine/combat.rkt"
         "engine/items.rkt"
         "engine/common.rkt"
         "engine/visibility.rkt")
(require "ui/utilities.rkt"
         "ui/display.rkt")
(provide (all-defined-out))

(define (pick-up)
  (let* ((cell    (grid-ref (player-map player) (character-pos player)))
         (items (cell-items cell)))
    (choice items inventory-pick-up
            "There is nothing to pick up." "Pick up what?" "Picked up ")))
(define (cmd-drop)
  (let ([items (player-character-inventory player)])
    (choice items inventory-drop
            "You have nothing to drop." "Drop what?" "Dropped ")))
(define (equip)
  (let ([items (filter (lambda (x) (equipable-item? x))
                       (player-character-inventory player))])
    (choice items inventory-equip
            "You have nothing to equip." "Equip what?" "Equipped ")))
(define (take-off)
  (let* ([e     (character-equipment player)]
         [items (filter (lambda (obj) (and obj (removable? obj)))
                        (map car (equipment->list e)))])
    (choice items inventory-take-off
            "You have nothing to take off." "Take off what?" "Took off ")))
(define (cmd-drink)
  (let* ([e     (character-equipment player)]
         [items (filter potion? ; currently, we can only drink potions
                        (player-character-inventory player))]
         [o     #f])
    (choice items (lambda (item) (set! o item) (inventory-remove item))
            "You have nothing to drink." "Drink what?" "Drank ")
    ;; necessary to display the messages in the right order
    (when o (drink-action o player))))


(define (climb-stairs)
  (let ((cell (grid-ref (player-map player) (character-pos player))))
    (cond [(stairs-up? cell)   (go-up-stairs)]
          [(stairs-down? cell) (go-down-stairs)]
          [else (display "There are no stairs here.\n")])))

(define (cmd-open)  (direction-command "Open"  open))
(define (cmd-close) (direction-command "Close" close))


(define (shoot)
  (let* ((grid    (player-map player))
         (weapon  (equipment-main-hand (character-equipment player)))
         (targets (available-targets player))
         (n       (length targets)))
    (cond
     ((not (ranged-weapon? weapon))
      (display "I can't shoot with that.\n"))
     ((null? targets)
      (display "There is nothing to shoot.\n"))
     (else
      (let ((grid (let ((grid (grid-copy grid)))
                    (for-each
                     (lambda (m n) ;; TODO like choice, won't scale over 10
                       (grid-set!
                        grid
                        (character-pos m)
                        (new-display-cell
                         (string-ref (number->string (+ n 1)) 0))))
                     targets
                     (iota n))
                    grid)))
        ;; show which monsters are which numbers
        (cursor-home) ;; TODO taken from show-state
        (clear-to-bottom)
        (cursor-notification-head)
        (for-each (lambda (m n)
                    (printf-notification "~a: ~a\n"
                                         (+ n 1) (character-name m)))
                  targets
                  (iota n))
        (newline)
        (printf-notification "q: Cancel\n")
        ;; show the map with the target numbers
        (cursor-home)
        (printf "Floor ~a\n" (player-character-floor-no player))
        (show-grid grid
                   #:print-fun (visibility-show (player-view player)
                                                (player-map  player)))
        ;; choose a target
        (let ((nb (read-number n)))
          (when nb (ranged-attack player (list-ref targets nb)))))))))


;; for debugging purposes
(define (kill) ; insta-kill something
  (direction-command "Kill"
                     (lambda (grid cell player)
                       (cond ((cell-occupant cell)
                              => (lambda (occ)
                                   (printf "Killed the ~a\n"
                                           (character-name occ))
                                   (remove-monster occ)))
                             (else (display
                                    "There is nothing to kill there.\n"))))))
(define (reveal-map)
  (let ((view (player-view player)))
    (grid-for-each (lambda (p) (grid-set! view p 'visited)) view)))
(define (god-mode)
  (reveal-map)
  (set-box! god-mode? #t))



;; help features
;; currently incomplete
(define (show-help)
  ;; TODO maybe generate commands with a macro and have help text that can be
  ;;  displayed by help, everything would be automatic
  'TODO)

(define (info grid pos)
  ;; TODO show a message about the location, occupant first (unless player),
  ;;  items then, finally terrain
  (let ((cell (grid-ref grid pos)))
    (cond ((let ((occ (cell-occupant cell)))
             (and occ (not (player-character? occ)) occ))
           => (lambda (occ) (display (character-name occ))))
          ;;        ((car (cell-items cell))
          ;;         => (lambda (obj) (display (item-name obj))))
          ;; TODO broken. + add monsters
          (else
           (display "Nothing to see here.")))))
;; TODO describe the terrain, have a description for each type, ideally
;;  define with the type

(define (look grid pos)
  ;; TODO have a moveable cursor, and when enter is pressed, display the info
  ;;  of the location, pos is starting position of the cursor, if final cursor
  ;;  position is outside visibility, say I can't see there
  ;; TODO use the choose-direction command to control the cursor
  (cursor-on)
  ;; TODO maybe just be able to look at immediate squares, and just use
  ;;  direction-command, but we might want to identify something before we
  ;;  get closer (a monster, for example)
  (set-cursor-on-grid grid pos)
  (read-char)
  ;; TODO implement the rest, and it seems that pressing l then an arrow
  ;;  shows some weird text in the background about terminal options
  (cursor-off))
