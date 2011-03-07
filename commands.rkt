#lang racket

(require (only-in srfi/1 iota))
(require "utilities/terminal.rkt"
         "utilities/grid.rkt"
         "utilities/cell.rkt"
         "utilities/floor-utils.rkt")
(require "character.rkt"
         "player.rkt"
         "combat.rkt"
         "objects.rkt"
         "common.rkt"
         "visibility.rkt"
         "ui-utils.rkt")
(provide (all-defined-out))

(define (inventory)
  (cursor-home)
  (clear-to-bottom)
  (display "Equipment:\n")
  (for-each-equipped
   (lambda (obj where)
     (printf "~a~a\n" where (if obj (object-info obj) "")))
   (character-equipment player))
  (printf "\nAC: ~a\n" (get-armor-class player))
  (display "\nInventory:\n")
  (for-each (lambda (o) (printf "~a\n" (object-info o)))
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
(define (pick-up pos) ;; TODO pos can be useful if we can pick up at a distance
  (let* ((cell    (grid-ref (player-map player) pos))
         (objects (cell-objects cell)))
    (choice objects
            (lambda (object)
              (remove-object cell object)
              (set-player-character-inventory!
               player (cons object (player-character-inventory player))))
            "There is nothing to pick up." "Pick up what?" "Picked up ")))
(define (cmd-drop)
  (let ((cell    (grid-ref (player-map player) (character-pos player)))
        (objects (player-character-inventory player)))
    (choice objects
            (lambda (object)
              (set-player-character-inventory! player (remove object objects))
              (add-object cell object))
            "You have nothing to drop." "Drop what?" "Dropped ")))
(define (equip)
  (let ((e       (character-equipment player))
        (objects (filter (lambda (x) (equipable-object? x))
                         (player-character-inventory player))))
    (choice objects
            (lambda (object)
              ;; TODO macro?
              (let* ((place (cond ((weapon?     object) 'main-hand)
                                  ((shield?     object) 'off-hand)
                                  ((body-armor? object) 'torso)))
                     (old   ((case place
                               ((main-hand) equipment-main-hand)
                               ((off-hand)  equipment-off-hand)
                               ((torso)     equipment-torso))
                             e)))
                (define (back-in-inventory o)
                  (printf "Put ~a back in inventory.\n" (object-name o))
                  (set-player-character-inventory!
                   player (cons o (player-character-inventory player))))
                (set-player-character-inventory!
                 player (remove object objects))
                ((case place
                   ((main-hand) set-equipment-main-hand!)
                   ((off-hand)  set-equipment-off-hand!)
                   ((torso)     set-equipment-torso!))
                 e object)
                ;; TODO generalize with all non-removable items
                (cond ((and old (not (off-hand-placeholder? old)))
                       (back-in-inventory old))
                      ((two-handed-weapon? old)
                       (set-equipment-off-hand! e #f)) ; remove the placeholder
                      ((off-hand-placeholder? old)
                       ;; we have to remove the two-handed weapon itself
                       (back-in-inventory (equipment-main-hand e))
                       (set-equipment-main-hand! e #f)))
                (when (two-handed-weapon? object)
                  (let ((old-off (equipment-off-hand e)))
                    (when (and old-off (not (off-hand-placeholder? old-off)))
                      (back-in-inventory old-off))
                    (set-equipment-off-hand! e (new-off-hand-placeholder))))))
            "You have nothing to equip." "Equip what?" "Equipped ")))
(define (take-off)
  (let* ((e       (character-equipment player))
         (objects (filter (lambda (obj) (and obj (removable? obj)))
                          (map car (equipment->list e)))))
    (choice objects
            (lambda (object)
              (cond ((weapon?     object)
                     (set-equipment-main-hand! e #f)
                     (when (two-handed-weapon? object)
                       ;; remove the placeholder
                       (set-equipment-off-hand! e #f)))
                    ((shield?     object)
                     (set-equipment-off-hand!  e #f))
                    ((body-armor? object)
                     (set-equipment-torso!     e #f)))
              (set-player-character-inventory!
               player (cons object (player-character-inventory player))))
            "You have nothing to take off." "Take off what?" "Took off ")))
(define (cmd-drink)
  (let* ((e       (character-equipment player))
         (objects (player-character-inventory player))
         (o       #f))
    (choice objects
            (lambda (object)
              (set! o object)
              (set-player-character-inventory! player (remove object objects)))
            "You have nothing to drink." "Drink what?" "Drank ")
    ;; necessary to display the messages in the right order
    (when o (drink o) (attacks-of-opportunity player))))


(define (climb-stairs)
  (let ((cell (grid-ref (player-map player) (character-pos player))))
    (let ((current      (player-character-current-floor player))
          (before       (player-character-floors-before player))
          (after        (player-character-floors-after  player)))
      (cond ((stairs-up? cell)
             (cond ((not (null? before))
                    (let ((new (car before)))
                      (place-player
                       player new
                       #:start-pos (floor-stairs-down
                                    (player-floor-floor new)))
                      (set-player-character-floor-no!
                       player (sub1 (player-character-floor-no player)))
                      (set-player-character-floors-after!
                       player (cons current after))
                      (set-player-character-floors-before!
                       player (cdr before))))
                   (else (display "This would lead to the surface.\n"))))
            ((stairs-down? cell)
             (set-player-character-floor-no!
              player (add1 (player-character-floor-no player)))
             (set-player-character-floors-before! player (cons current before))
             (if (null? after)
                 (place-player player
                               (new-player-floor
                                (player-character-floor-no player)))
                 (begin (place-player player (car after))
                        (set-player-character-floors-after!
                         player (cdr after)))))
            (else (display "There are no stairs here.\n"))))))

(define (cmd-open)  (direction-command "Open"  open))
(define (cmd-close) (direction-command "Close" close))


(define (shoot)
  (let* ((grid    (player-map player))
         (weapon  (equipment-main-hand (character-equipment player)))
         (targets (filter (lambda (m)
                            (and (eq? (grid-ref (player-view player)
                                                (character-pos m))
                                      'visible)
                                 (clear-shot? grid
                                              (character-pos player)
                                              (character-pos m))))
                          (floor-monsters (character-floor player))))
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


(define (kill) ; insta-kill something, for debugging purposes
  (direction-command "Kill"
                     (lambda (grid cell player)
                       (cond ((cell-occupant cell)
                              => (lambda (occ)
                                   (printf "Killed the ~a\n"
                                           (character-name occ))
                                   (remove-monster occ)))
                             (else (display
                                    "There is nothing to kill there.\n"))))))


;; for debugging
(define (reveal-map)
  (let ((view (player-view player)))
    (grid-for-each (lambda (p) (grid-set! view p 'visited))
                   view)))
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
  ;;  objects then, finally terrain
  (let ((cell (grid-ref grid pos)))
    (cond ((let ((occ (cell-occupant cell)))
             (and occ (not (player-character? occ)) occ))
           => (lambda (occ) (display (character-name occ))))
          ;;        ((car (cell-objects cell))
          ;;         => (lambda (obj) (display (object-name obj))))
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
