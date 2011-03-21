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


(define command-table
  (list
   ;; inventory
   (cons #\p pick-up)
   (cons #\d cmd-drop)
   (cons #\i inventory)
   (cons #\e equip)
   (cons #\r take-off)
   (cons #\D cmd-drink)
   
   (cons #\o cmd-open)
   (cons #\c cmd-close)
   (cons #\t climb-stairs)
   
   (cons #\s shoot)
   
   ;; help
   (cons #\? describe)
   (cons #\/ describe-all)
   (cons #\' info)
   (cons #\" look)
   
   ;; debugging
   (cons #\k kill)
   (cons #\R reveal-map)
   (cons #\G god-mode)
   (cons #\: console)
   
   (cons #\space (lambda () display "Nothing happen\n"))
   (cons #\q quit)))

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
        ((dict-ref command-table char invalid-command)))))
