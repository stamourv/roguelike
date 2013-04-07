#lang racket

(require racket/require)
(require (multi-in "../utilities" ("terminal.rkt" "display.rkt" "grid.rkt"))
         (multi-in "../engine"    ("cell.rkt" "character.rkt" "player.rkt"
                                   "combat.rkt" "items.rkt" "common.rkt"
                                   "visibility.rkt"))
         "utilities.rkt"
         "display.rkt")

(provide (all-defined-out))

(define (inventory)
  (cursor-home)
  (clear-to-bottom)
  (display "Equipment:\n")
  (for-each-equipped
   (lambda (obj where)
     (printf "~a~a\n" where (if obj (item-info obj) "")))
   (character-equipment player))
  (printf "\nAC: ~a\n" (get-armor-class player))
  (display "\nInventory:\n")
  (for-each (lambda (o) (printf "~a\n" (item-info o)))
            (player-character-inventory player))
  (let loop ((c #f))
    (case c
      ((#\e) (equip))
      ((#\r) (take-off))
      ((#\d) (cmd-drop))
      ((#\q) #f)
      (else (display "\ne: Equip\nr: Take off\nd: Drop\nq: Cancel\n")
            (loop (read-char)))))
  (clear-to-bottom))

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
  (define orig-grid (player-map player))
  (define weapon    (get-weapon player))
  (define targets   (available-targets player))
  (define n         (length targets))
  (cond
   [(not (ranged-weapon? weapon))
    (display "I can't shoot with that.\n")]
   [(null? targets)
    (display "There is nothing to shoot.\n")]
   [else
    ;; show targets
    (define grid (grid-copy orig-grid))
    (for ([m (in-list targets)]
          [i (in-naturals 1)])
      (grid-set! grid
                 (character-pos m)
                 (new-display-cell (string-ref (number->string i) 0))))
    ;; show which monsters are which numbers
    (cursor-home)
    (clear-to-bottom)
    (cursor-notification-head)
    (for ([m (in-list targets)]
          [i (in-naturals 1)])
      (printf-notification "~a: ~a\n" i (character-name m)))
    (newline)
    (printf-notification "q: Cancel\n")
    ;; show the map with the target numbers
    (print-floor-banner)
    (show-grid grid
               #:print-fun (visibility-show (player-view player)
                                            (player-map  player)))
    ;; choose a target
    (define nb (read-number n))
    (when nb (ranged-attack player (list-ref targets nb)))]))


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
  (set-box! god-mode? #t))
