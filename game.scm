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


(define (game player victory-fun)
  (let loop ()
    (if (victory-fun (player-floor player) player)
	(begin
	  (display "You win!")
	  (quit)) ;; TODO have something more dramatic
	(let ((floor (player-floor player)))
	  (update-visibility player)
	  (show-state player)
	  (read-command player) ; side-effects the player
	  (for-each (lambda (m)
		      ((behavior-fun (monster-behavior m))
		       m floor (character-pos player)))
		    (floor-monsters floor))
	  (loop)))))

(define n-levels #f) ;; TODO having a global for that is ugly, but it can't really fit in the player structure. if we end up having a dungeon type, put it there
(define (dungeon n name)
  (set! n-levels n)
  (game (new-player name) (lambda (level player) #f)))

(define (quit)
  ;; restore tty
  (tty-mode-set! (current-input-port) #t #t #f #f 0)
  (shell-command "setterm -cursor on")
;;   (profile-stop!) ;; TODO PROFILING
;;   (write-profile-report "profiling")
  (exit))

;; (load "~/src/scheme/statprof/statprof.scm") ;; TODO PROFILING
;; (profile-start!)

;; (if (not debug) (maze 8 8 (random-element character-names)))
(if (not debug) (dungeon 3 "Bob"))

(if (not debug) (quit))
