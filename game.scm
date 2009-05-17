(include "utilities.scm")
(include "grid.scm")
(include "maze.scm")
(include "names.scm")

(random-source-randomize! default-random-source)
(tty-mode-set! (current-input-port) #t #t #t #f 0)
;; (##tty-mode-set! port input-allow-special input-echo input-raw output-raw speed)

(define (maze h w player-name maze-name treasure-name)
  ;; simple maze game, start at the top left, and get to the bottom right
  (let* ((maze  (generate-maze h w))
	 (view (empty-grid (grid-height maze) (grid-width maze)
			    cell-fun: (lambda (x y) 'unknown))))
    ;; treasure at the bottom corner
    (grid-set! maze (- (grid-height maze) 1) (- (grid-width maze) 1)
	       (new-treasure-cell))
    (let loop ((posx 0) (posy 0))
      (define (move x y)
	(if (and (inside-grid?   maze x y)
		 (walkable-cell? (grid-get maze x y)))
	    (begin (grid-set! maze posx posy (new-walkable-cell))
		   (set! posx x)
		   (set! posy y))))
      (grid-set! maze posx posy (new-player-cell (make-player player-name)))
      ;; update our map with what's around us
      (for-each (lambda (x)
		  (for-each (lambda (y)
			      (if (eq? (grid-get view x y) 'visible)
				  (grid-set! view x y 'visited)))
			    (iota (grid-width view))))
		(iota (grid-height view))) ;; TODO have a function, update-fog-of-war for that, or even update-visibility, which also does what's right after
      (for-each (lambda (x y) (if (inside-grid? maze x y)
				  (grid-set! view x y 'visible)))
		(list (- posx 1) (- posx 1) (- posx 1)
		      posx       posx       posx
		      (+ posx 1) (+ posx 1) (+ posx 1))
		(list (- posy 1) posy       (+ posy 1)
		      (- posy 1) posy       (+ posy 1)
		      (- posy 1) posy       (+ posy 1)))
;;       (display (string-append (list->string (list (integer->char #x1b))) "[D")) ;; TODO scroll the screen (erase screen doesn't work) TODO not supported by gambit
      (show-grid maze view)
      (let ((newposx posx)
	    (newposy posy))
	(case (read-char)
	  ((#\w) (set! newposx (- posx 1))) ;; TODO put the command handling in a function
	  ((#\a) (set! newposy (- posy 1)))
	  ((#\s) (set! newposx (+ posx 1)))
	  ((#\d) (set! newposy (+ posy 1))))
	(move newposx newposy)
	(if (and (inside-grid? maze newposx newposy)
		 (treasure-cell? (grid-get maze newposx newposy)))
	    (display (string-append player-name " has recovered "
				    treasure-name " from " maze-name "\n"))
	    (loop posx posy))))))

(maze 5 5
      (random-element character-names) 
      (random-element dungeon-names)
      (random-element object-names))

;; restore tty, needed if launched from the REPL
(tty-mode-set! (current-input-port) #t #t #f #f 0)
