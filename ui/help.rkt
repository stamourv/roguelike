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
(require "utilities.rkt")

(provide (all-defined-out))

(define (describe-command)
  (displayln "Type the keybinding you want me to describe.")
  (echo-on)
  (let* ([what (read-char)]
         [cmd  (dict-ref command-table what "This keybinding is unused.")])
    (newline)
    (displayln (if (pair? cmd) (caddr cmd) cmd)))
  (echo-off))

(define (describe-commands)
  (displayln "Movement:\nArrow keys")
  (let loop ([commands command-table]
             [category (caddr (car command-table))]
             [line-no  0])
    (if (eq? category 'debugging) ; don't show debug commands
        (loop (cdr commands) (caddr (cadr commands)) line-no)
        (let ([cat (symbol->string category)])
          (newline)
          (let loop2 ([cs   commands]
                      [para ;; capitalize category
                       (format "~a:\n" (upcase-word cat))])
            (when (not (null? cs))
              (let* ([head (car cs)]
                     [cat  (caddr head)])
                (if (eq? cat category) ; same category
                    (loop2 (cdr cs)
                           (string-append para
                                          (format "~a: ~a\n"
                                                  (car head) (cadddr head))))
                    ;; print paragraph and go on
                    (loop cs (caddr head)
                          (print-paragraph para line-no)))))))))
  (wait-after-long-output))

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
    (let loop ([grouped grouped]
               [line-no 0])
      (when (not (null? grouped))
        (newline)
        (let loop2 ([g    (car grouped)]
                    [para ;; capitalize category
                     (let ([cat (symbol->string (cadr (caar grouped)))])
                       (format "~as:\n" (upcase-word cat)))])
          (if (not (null? g))
              (loop2 (cdr g)
                     (string-append para
                                    (with-output-to-string
                                      (lambda ()
                                        (let ([x (car g)])
                                          (print-sprite (car x))
                                          (printf ": ~a\n" (cddr x)))))))
              (loop (cdr grouped)
                    (print-paragraph para line-no)))))))
  (wait-after-long-output))

(define (info)
  (let* ([grid (player-map player)]
         [pos  (character-pos player)]
         [cell (grid-ref grid pos)]
         [occ  (cell-occupant cell)])
    ;; temporarily remove occupant, so see beyond the player itself
    (set-cell-occupant! cell #f)
    (displayln (describe-one (show cell)))
    (set-cell-occupant! cell occ)))

;; not currently working
(define (look)
  ;; have a moveable cursor, and when enter is pressed, display the info of
  ;; the location, pos is starting position of the cursor, if final cursor
  ;; position is outside visibility, say I can't see there
  ;; use the choose-direction command to control the cursor
  (cursor-on)
  ;; maybe just be able to look at immediate squares, and just use
  ;; direction-command, but we might want to identify something before we
  ;; get closer (a monster, for example)
  ;; (set-cursor-on-grid grid pos)
  (read-char)
  ;; implement the rest, and it seems that pressing l then an arrow shows
  ;; some weird text in the background about terminal options
  (cursor-off))
