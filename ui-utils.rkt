#lang racket

(require (only-in srfi/1 iota))
(require "utilities/grid.rkt"
         "utilities/terminal.rkt")
(require "character.rkt"
         "player.rkt"
         "common.rkt"
         "objects.rkt"
         "visibility.rkt")
(provide (all-defined-out))

(define (invalid-command) (display "Invalid command.\n"))

(define (choose-direction)
  (if (= (char->integer (read-char)) 27) ; escape
      (case (which-direction?)
        ((up)    up)
        ((down)  down)
        ((left)  left)
        ((right) right))
      (begin (invalid-command)
             #f)))

(define (which-direction?)
  (when (not (eq? (read-char) #\[))
    (invalid-command))
  (case (read-char)
    ((#\A) 'up)
    ((#\B) 'down)
    ((#\C) 'right)
    ((#\D) 'left)
    (else  (invalid-command))))

(define (read-number n) ; read a number up to n, or q to cancel
  (let loop ((nb (read-char)))
    (cond ((eq? nb #\q)
           #f) ; cancel
          ((not (and (char>=? nb #\1)
                     (<= (- (char->integer nb) (char->integer #\0)) n)))
           (loop (read-char)))
          (else
           (- (char->integer nb)
              (char->integer #\0) 1)))))

(define (choice objects f null-message question feedback)
  (if (null? objects)
      (printf "~a\n" null-message)
      (begin   (cursor-home)
               (clear-to-bottom)
               (printf "~a\nq: Cancel\n" question)
               (for-each (lambda (o i)
                           (printf "~a: ~a\n" (+ i 1) (object-info o)))
                         objects
                         (iota (length objects)))
               (let ((nb (read-number (length objects))))
                 (when nb
                   (let ((object (list-ref objects nb)))
                     (show-state)
                     (f object)
                     (printf "~a~a.\n" feedback (object-info object))))))))

(define (direction-command name f)
  (clear-to-bottom)
  (printf "~a in which direction? " name)
  (flush-output) ;; TODO may be necessary elsewhere
  (let ((dir (choose-direction))) ; evaluates to a function, or #f
    (when dir
      (let* ((grid (player-map player))
             (cell (grid-ref grid (dir (character-pos player)))))
        (f grid cell player)))))

;; console from which arbitrary expressions can be evaluated
(define (console)
  (restore-tty)
  (display ": ")
  (display (eval (read))) ;; TODO not sure this is going to work...
  (read-char)
  (intercept-tty))


(define (show-state)
  (cursor-notification-head)
  (printf-notification "~a\n" (character-name player))
  (printf-notification "level ~a\n" (character-level player))
  (printf-notification "~a xp pts\n" (player-character-experience player))
  (printf-notification "")
  (when (altered-attr? player 'hp)
    (terminal-colors 'white 'black)) ;; TODO abstract that
  (display (character-hp player))
  (terminal-reset)
  (printf "/~a hp\n" (character-max-hp player))
  
  (printf-notification "AC: ")
  (when (altered-attr? player 'natural-ac)
    (terminal-colors 'white 'black))
  (display (get-armor-class player))
  (terminal-reset)
  (newline)
  
  (printf-notification "str: ")
  (when (altered-attr? player 'str)
    (terminal-colors 'white 'black))
  (display (character-str player))
  (terminal-reset)
  (display "	int: ")
  (when (altered-attr? player 'int)
    (terminal-colors 'white 'black))
  (display (character-int player))
  (terminal-reset)
  (newline)
  
  (printf-notification "dex: ")
  (when (altered-attr? player 'dex)
    (terminal-colors 'white 'black))
  (display (character-dex player))
  (terminal-reset)
  (display "	wis: ")
  (when (altered-attr? player 'wis)
    (terminal-colors 'white 'black))
  (display (character-wis player))
  (terminal-reset)
  (newline)
  
  (printf-notification "con: ")
  (when (altered-attr? player 'con)
    (terminal-colors 'white 'black))
  (display (character-con player))
  (terminal-reset)
  (display "	cha: ")
  (when (altered-attr? player 'cha)
    (terminal-colors 'white 'black))
  (display (character-cha player))
  (terminal-reset)
  (newline)
  
  (cursor-home)
  (clear-line)
  (printf "Floor ~a\n" (player-character-floor-no player))
  (show-grid (player-map player)
	     #:print-fun (visibility-show (player-view player)
                                          (player-map  player))))
