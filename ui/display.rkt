#lang racket

(require "../utilities/display.rkt"
         "../utilities/terminal.rkt")
(require "../engine/character.rkt"
         "../engine/player.rkt"
         "../engine/items.rkt"
         "../engine/common.rkt"
         "../engine/visibility.rkt")
(provide print-state quit print-floor-banner)

(define (print-state)
  (print-sidebar)
  (print-floor-banner)
  (show-grid (player-map player)
	     #:print-fun (visibility-show (player-view player)
                                          (player-map  player))))

(define (print-floor-banner)
  (cursor-home)
  (clear-line)
  (printf "Floor ~a\n" (player-character-floor-no player)))

(define (print-sidebar)
  (cursor-notification-head)
  (printf-notification "~a\n" (character-name player))
  (printf-notification "level ~a\n" (character-level player))
  (printf-notification "~a xp pts\n" (player-character-experience player))
  (printf-notification "") ; for alignment

  (print-altered 'hp)
  (printf "/~a hp\n" (character-max-hp player))
  
  (printf-notification "AC: ")
  (when (altered-attr? player 'natural-ac)
    (terminal-colors 'white 'black))
  (display (get-armor-class player))
  (terminal-reset)
  (newline))

(define (print-altered a)
  (when (altered-attr? player a)
    (terminal-colors 'white 'black))
  (display ((attribute-getter a) player))
  (terminal-reset))

(define (print-attributes)
  (for-each
   (lambda (l)
     (define (display-attr a)
       (printf "~a: " a)
       (print-altered a))
     (printf-notification "") ; for alignment
     (display-attr (first l))
     (display "\t")
     (display-attr (second l))
     (newline))
   '((str int) (dex wis) (con cha))))


(define (quit #:force [force? #f])
  (when (not force?) (displayln "Do you really want to quit?"))
  (echo-on)
  (cond
   [(and (not force?) (not (eq? (read-char) #\y)))
    (displayln "\nAlright then.")
    (echo-off)]
   [else
    (display "\n\nHall of fame:\n\n")
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
