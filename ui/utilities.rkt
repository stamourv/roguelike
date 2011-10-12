#lang racket

(require (only-in srfi/1 iota) racket/require)
(require (multi-in "../utilities" ("grid.rkt" "terminal.rkt"))
         (multi-in "../engine"    ("character.rkt" "player.rkt" "common.rkt"
                                   "items.rkt"))
         "display.rkt")
(provide (all-defined-out))

(define command-table '())
(define (new-command char thunk category desc)
  (set! command-table (cons (list char thunk category desc) command-table)))
(define (reverse-command-table)
  (set! command-table (reverse command-table)))

(define (invalid-command) (display "Invalid command.\n"))

;; print a large block of text
;; if it doesn't fit, ask to continue, then display
;; returns new line number
(define (print-paragraph s line-at)
  (let ([n-lines (length (regexp-split "\n" s))])
    (cond [(> (+ line-at n-lines) 22) ; to leave space for scroll message
           (display "There's more.")
           (wait-after-long-output)
           (display s)
           n-lines] ; we had to wrap around
          [else
           (display s)
           (+ line-at n-lines)])))

(define (wait-after-long-output)
  (newline)
  (displayln "Press any key to continue.")
  (read-char)
  (for ([i (in-range 30)]) (newline)))

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

(define (choice items f null-message question feedback)
  (if (null? items)
      (printf "~a\n" null-message)
      (begin   (cursor-home)
               (clear-to-bottom)
               (printf "~a\nq: Cancel\n" question)
               (for-each (lambda (o i)
                           (printf "~a: ~a\n" (+ i 1) (item-info o)))
                         items
                         (iota (length items)))
               (let ((nb (read-number (length items))))
                 (when nb
                   (let ((item (list-ref items nb)))
                     (print-state)
                     (f item)
                     (printf "~a~a.\n" feedback (item-info item))))))))

(define (direction-command name f)
  (clear-to-bottom)
  (printf "~a in which direction? " name)
  (flush-output)
  (let ((dir (choose-direction))) ; evaluates to a function, or #f
    (when dir
      (let* ((grid (player-map player))
             (cell (grid-ref grid (dir (character-pos player)))))
        (f grid cell player)))))

;; console from which arbitrary expressions can be evaluated
(define (console)
  (restore-tty)
  (display ": ")
  ;; untouched from the Gambit version. unlikely to work
  (display (eval (read)))
  (read-char)
  (intercept-tty))
