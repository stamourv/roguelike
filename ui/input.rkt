#lang racket

(require "../utilities/terminal.rkt"
         "../utilities/grid.rkt")
(require "../engine/character.rkt"
         "../engine/common.rkt"
         "../engine/floor.rkt")
(require "utilities.rkt"
         "commands.rkt"
         "help.rkt"
         "display.rkt")
(provide (all-defined-out))


;; inventory
(new-command #\p pick-up   'inventory "Pick up an item from the ground.")
(new-command #\d cmd-drop  'inventory "Drop an item from the inventory.")
(new-command #\i inventory 'inventory "Display inventory.")
(new-command #\e equip     'inventory "Equip an item from inventory.")
(new-command #\r take-off  'inventory "Take off an equipped item.")

(new-command #\o cmd-open     'exploration "Open a door or a chest.")
(new-command #\c cmd-close    'exploration "Close a door or a chest.")
(new-command #\t climb-stairs 'exploration "Climb stairs.")

(new-command #\D cmd-drink 'combat "Drink a potion from inventory.")
(new-command #\s shoot     'combat "Shoot a target using a ranged weapon.")

;; help
(new-command #\? describe-commands 'help "List all commands.")
(new-command #\/ describe     'help "Describe what a character represents.")
(new-command #\& describe-all 'help "List all characters known to the game.")
(new-command #\' info         'help "Get information about the current tile.")
(new-command #\" look         'help "Information about a given tile.")

;; debugging
(new-command #\k kill       'debugging "Insta-kill.")
(new-command #\R reveal-map 'debugging "Reaveal map.")
(new-command #\G god-mode   'debugging "God mode.")
(new-command #\: console    'debugging "Console.")

(new-command #\space (lambda () display "Nothing happen\n")
             'misc "Wait.")
(new-command #\q quit 'misc "Quit.")

;; for display
(reverse-command-table)


(define (read-command)
  (let* ((pos   (copy-point (character-pos player)))
         (grid  (floor-map (character-floor player)))
         (x     (point-x pos))
         (y     (point-y pos))
         (char  (read-char)))
    (clear-to-bottom)
    ;; escape. bizarrely unrecognizable with case...
    (if (= (char->integer char) 27)
        ;; movement
        (begin (case (which-direction?)
                 ;; TODO abstract with other arrow reading code
                 ((up)    (set-point-x! pos (- x 1)))
                 ((down)  (set-point-x! pos (+ x 1)))
                 ((right) (set-point-y! pos (+ y 1)))
                 ((left)  (set-point-y! pos (- y 1))))
               ;; tries to move to the new position
               ;; if it fails, stay where we were
               (move grid player pos)
               'move)
        ((car (dict-ref command-table char (list invalid-command)))))))
