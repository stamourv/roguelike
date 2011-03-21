#lang racket

(require "../utilities/terminal.rkt"
         "../utilities/list.rkt"
         "../utilities/string.rkt"
         "../utilities/grid.rkt"
         "../utilities/display.rkt"
         "../utilities/descriptions.rkt")
(require "../engine/common.rkt"
         "../engine/cell.rkt"
         "../engine/player.rkt"
         "../engine/character.rkt")

(provide (all-defined-out))

;; help features
(define (describe-commands)
  ;; TODO show all commands, and have a version that waits for one command
  ;;  and says what it is
  ;; TODO explicitly say that arrows are for moving, since they won't be in
  ;;  the list
  'TODO)

(define (describe-one what)
  (let ([type+desc (dict-ref descriptions-table what
                             "I don't know what that is.")])
    (if (pair? type+desc)
        (format "~a" (cdr type+desc))
        type+desc)))

;; reads a character, and prints out the description
(define (describe)
  (displayln "Type the character you want me to describe.")
  (echo-on)
  (let ([what (read-char)])
    (newline)
    (displayln (describe-one what)))
  (echo-off))

(define (describe-all)
  (let ([grouped (group-by-identical
                  descriptions-table string<?
                  ;; sort by category
                  #:key (lambda (x) (symbol->string (cadr x))))])
    (for ([g grouped])
         ;; capitalize category
         (let ([cat (symbol->string (cadar g))])
           (printf "~as:\n" (upcase-word cat)))
         (for ([x g])
              (print-sprite (car x))
              (printf ": ~a\n"  (cddr x)))
         (newline)))
  (displayln "\nPress any key.")
  (read-char)
  (for ([i (in-range 30)]) (newline)))

(define (info)
  (let* ([grid (player-map player)]
         [pos  (character-pos player)]
         [cell (grid-ref grid pos)]
         [occ  (cell-occupant cell)])
    ;; temporarily remove occupant, so see beyond the player itself
    (set-cell-occupant! cell #f)
    (displayln (describe-one (show cell)))
    (set-cell-occupant! cell occ)))

(define (look)
  ;; TODO have a moveable cursor, and when enter is pressed, display the info
  ;;  of the location, pos is starting position of the cursor, if final cursor
  ;;  position is outside visibility, say I can't see there
  ;; TODO use the choose-direction command to control the cursor
  (cursor-on)
  ;; TODO maybe just be able to look at immediate squares, and just use
  ;;  direction-command, but we might want to identify something before we
  ;;  get closer (a monster, for example)
  ;;   (set-cursor-on-grid grid pos)
  (read-char)
  ;; TODO implement the rest, and it seems that pressing l then an arrow
  ;;  shows some weird text in the background about terminal options
  (cursor-off))
