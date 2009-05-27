(include "utilities.scm")
(include "grid.scm")
(include "cell.scm")
(include "character.scm")
(include "player.scm")
(include "monsters.scm")
(include "objects.scm")
(include "maze.scm")
(include "dungeon.scm")
(include "visibility.scm")
(include "terminal.scm")
(include "input.scm")
(include "names.scm")
(include "help.scm")

(define debug #f) ;; TODO find a better way

(random-source-randomize! default-random-source)
(tty-mode-set! (current-input-port) #t #t #t #f 0)
(shell-command "setterm -cursor off")
;; clear the screen. ugly, but the clear terminal code does not seem to be
;; supported by gambit
(for-each (lambda (dummy) (display "\n")) (iota 50)) ;; TODO use window size


(define test-dungeon
  (string->grid
   (string-append "   |     | |   \n"
		  "   |     |     \n"
		  "   |     | |   \n"
		  "-+ +-- --+ +- -\n"
		  " | |     | |   \n"
		  " |       | |- -\n"
		  " | |     | |   \n"
		  "   |       | | \n")))

(define (game player victory-fun) ;; TODO get a level function, to generate new levels, or just use generate-level ?
  (let loop ()
    (if (victory-fun (player-floor player) player)
	(begin
	  (display "You win!")
	  (quit)) ;; TODO have something more dramatic
	(let ((floor (player-floor player)))
	  (update-visibility player)
	  (show-state player)
	  (read-command player) ; side-effects the player
	  (for-each (lambda (monster) (move (floor-map floor) monster (random-element (four-directions (character-pos monster)))))
		    (floor-monsters floor)) ;; TODO FOO replace with an AI
	  (loop)))))

;; TODO obsolete, doesn't work with the new architecture
;; (define (maze h w name)
;;   ;; simple maze game, start at the top left, and get to the bottom right
;;   (let ((level (generate-maze h w)))
;;     ;; add a treasure
;;     (add-object (grid-get level (new-point (- (grid-height level) 1)
;; 					   (- (grid-width level)  1)))
;; 		(new-treasure))
;;     (game (new-player name level (new-point 0 0))
;; 	  (lambda (level player) #f))))

(define n-levels #f) ;; TODO having a global for taht is ugly, but it can't really fit in the player structure. if we end up having a dungeon type, put it there
(define (dungeon n name)
  (set! n-levels n)
  (game (new-player name) (lambda (level player) #f))) ;; TODO use n-levels, have a dungeon type for that

(define (quit)
  ;; restore tty
  (tty-mode-set! (current-input-port) #t #t #f #f 0)
  (shell-command "setterm -cursor on")
  (profile-stop!) ;; TODO PROFILING
  (write-profile-report "profiling")
  (exit))

(load "~/src/scheme/statprof/statprof.scm") ;; TODO PROFILING
(profile-start!)

;; (if (not debug) (maze 8 8 (random-element character-names)))
(if (not debug) (dungeon 3 (random-element character-names)))

(if (not debug) (quit))
