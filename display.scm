(import (path class))
(import terminal)

(define-generic show) ; used for objects, cells, etc.
(define-method (show o) #\space)

(define-generic print)
(define-method (print x) (display x))

(define-class sprite ()
  (slot: char)
  (slot: bg)
  (slot: fg)
  (slot: bold?)
  (slot: underline?))
(define (new-sprite char #!key (bg 'black) (fg 'white)
		               (bold? #f) (underline? #f))
  (make-sprite char bg fg bold? underline?))
(define-method (print (x sprite))
  (terminal-print (sprite-char x)
		  bg:         (sprite-bg         x)
		  fg:         (sprite-fg         x)
		  bold?:      (sprite-bold?      x)
		  underline?: (sprite-underline? x)))

(define-generic darken-sprite) ; changes bg to black
(define-method (darken-sprite (s sprite))
  (new-sprite (sprite-char s)
	      bg:         'black
	      fg:         (let ((fg (sprite-fg s)))
			    (if (eq? fg 'black) 'white fg))
	      bold?:      (sprite-bold?      s)
	      underline?: (sprite-underline? s)))
(define-method (darken-sprite s) (new-sprite s))

(define-generic lighten-sprite)
(define-method (lighten-sprite (s sprite))
  (new-sprite (sprite-char s)
	      bg:         'white
	      fg:         (let ((fg (sprite-fg s)))
			    (if (eq? fg 'white) 'black fg))
	      bold?:      (sprite-bold?      s)
	      underline?: (sprite-underline? s)))
(define-method (lighten-sprite s) (new-sprite s bg: 'white fg: 'black))
