#lang racket

(require racket/require)
(require (multi-in "../utilities" ("display.rkt" "terminal.rkt"))
         (multi-in "../engine"    ("character.rkt" "player.rkt" "scheduler.rkt"
                                   "common.rkt" "visibility.rkt" "items.rkt")))
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
