(include "class.scm") ; CLOS-like object system ;; TODO sucks that I can't put it in ~~/lib
(include "utilities.scm") ;; TODO have a makefile, and separate compilation
(include "grid.scm")
(include "cell.scm")
(include "character.scm")
(include "player.scm")
(include "behavior.scm")
(include "objects.scm")
(include "monsters.scm")
(include "treasure.scm")
(include "dungeon.scm")
(include "visibility.scm")
(include "terminal.scm")
(include "input.scm")
(include "help.scm")

(define debug #f) ;; TODO find a better way

(random-source-randomize! default-random-source)
(tty-mode-set! (current-input-port) #t #t #t #f 0)
(shell-command "setterm -cursor off")
;; clear the screen. ugly, but the clear terminal code does not seem to be
;; supported by gambit
(for-each (lambda (dummy) (display "\n")) (iota 50)) ;; TODO use window size

;; list of pairs (name . score), sorted by descending order of score
(define hall-of-fame (car (read-all (open-file (list path:   "hall-of-fame"
						     create: 'maybe)))))
(define (update-hall-of-fame name score)
  (let* ((l   (sort-list (cons (cons name score) hall-of-fame)
			 (lambda (x y) (> (cdr x) (cdr y)))))
	 (new (if (> (length l) 10) ; we keep the 10 best
		  (remove-at-index l 10)
		  l)))
    (set! hall-of-fame new)
    (with-output-to-file "hall-of-fame"
      (lambda () (display new)))
    new))

(define (game player victory-fun)
  (let loop ()
    (cond ((victory-fun (player-floor player) player)
	   (display "You win!\n") ;; TODO have something more dramatic
	   (quit player))
	  ((<= (character-hp player) 0)
	   (display "You die.\n")
	   (quit player))
	  (else (let ((floor (player-floor player)))
		  (update-visibility player)
		  (show-state player)
		  (read-command player) ; side-effects the player
		  (for-each (lambda (m)
			      ((behavior-fun (monster-behavior m))
			       m floor (character-pos player)))
			    (floor-monsters floor))
		  (loop))))))

(define n-levels #f) ;; TODO having a global for that is ugly, but it can't really fit in the player structure. if we end up having a dungeon type, put it there
(define (dungeon n name)
  (set! n-levels n)
  (game (new-player name) (lambda (level player) #f)))

(define (quit player)
  (display "\nHall of fame:\n\n")
  (let loop ((hall       (update-hall-of-fame
			  (string->symbol (player-name player))
			  (player-experience player)))
	     (highlight? #t))
    (if (not (null? hall))
	(let ((current-game (cons (string->symbol (player-name player))
				  (player-experience player))))
	    (terminal-print
	     (string-append (symbol->string (caar hall))
			    ":\t" ;; TODO alignement will be messed up anyways
			    (number->string (cdar hall))
			    "\n")
	     bg: (if (and highlight? (equal? (car hall) current-game))
		     'white
		     'black)
	     fg: (if (and highlight? (equal? (car hall) current-game))
		     'black
		     'white))
	    (loop (cdr hall)
		  (and highlight? (not (equal? (car hall) current-game)))))))
  (display "\n")
  ;; restore tty
  (tty-mode-set! (current-input-port) #t #t #f #f 0)
  (shell-command "setterm -cursor on")
;;   (profile-stop!) ;; TODO PROFILING
;;   (write-profile-report "profiling")
  (exit))

;; (load "~/src/scheme/statprof/statprof.scm") ;; TODO PROFILING
;; (profile-start!)

(if (not debug) (dungeon 3 (getenv "LOGNAME")))
