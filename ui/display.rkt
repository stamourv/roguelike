#lang racket

(require "../utilities/display.rkt"
         "../utilities/terminal.rkt")
(require "../engine/character.rkt"
         "../engine/player.rkt"
         "../engine/items.rkt"
         "../engine/common.rkt"
         "../engine/visibility.rkt")
(provide show-state quit)

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

(define (quit #:force [force? #f])
  (when (not force?) (displayln "Do you really want to quit?"))
  (echo-on)
  (cond
   [(and (not force?) (not (eq? (read-char) #\y)))
    (displayln "\nAlright then.")
    (echo-off)]
   [else
    (display "\n\nHall of fame:\n\n") ;; TODO have in a function
    (let* ((name         (string->symbol (character-name player)))
           (xp           (player-character-experience player))
           (level        (character-level player))
           (floor-no     (player-character-floor-no player))
           (current-game (list name xp level floor-no))
           (filename     "hall-of-fame"))
      (define (update-hall-of-fame)
        ;; list of pairs (name . score), sorted by descending order of score
        (let* ((l   (sort (cons current-game
                                (if (file-exists? filename)
                                    (with-input-from-file filename read)
                                    '()))
                          ;; TODO if same score, sort with the other factors
                          > #:key cadr))
               (new (take l (min (length l) 10)))); we keep the 10 best
          (display new (open-output-file filename #:exists 'replace))
          new))
      (let loop ((hall (update-hall-of-fame))
                 (highlight? #t))
        (when (not (null? hall))
          (let ((head (car hall)))
            (terminal-print
             (format "~a:\t~a\tlevel ~a\tfloor ~a\n"
                     (car head) (cadr head) (caddr head) (cadddr head))
             #:bg (if (and highlight? (equal? (car hall) current-game))
                      'white
                      'black)
             #:fg (if (and highlight? (equal? (car hall) current-game))
                      'black
                      'white))
            (loop (cdr hall)
                  (and highlight? (not (equal? (car hall) current-game))))))))
    (newline)
    (restore-tty)
    (exit)]))
