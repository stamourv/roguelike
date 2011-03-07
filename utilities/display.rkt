#lang racket

(require "class.rkt" "terminal.rkt")
(provide (all-defined-out))

(define-generic show) ; used for items, cells, etc.
(define-method (show o) #\space)

(define-generic print-sprite)
(define-method (print-sprite x) (display x))

(define-class <sprite> () char bg fg bold? underline?)
(define (new-sprite char #:bg (bg 'black) #:fg (fg 'white)
		         #:bold? (bold? #f) #:underline? (underline? #f))
  (make-sprite char bg fg bold? underline?))
(define-method (print-sprite (x struct:sprite))
  (terminal-print (sprite-char x)
		  #:bg         (sprite-bg         x)
		  #:fg         (sprite-fg         x)
		  #:bold?      (sprite-bold?      x)
		  #:underline? (sprite-underline? x)))

(define-generic darken-sprite) ; changes bg to black
(define-method (darken-sprite (s struct:sprite))
  (new-sprite (sprite-char s)
	      #:bg         'black
	      #:fg         (let ((fg (sprite-fg s)))
			    (if (eq? fg 'black) 'white fg))
	      #:bold?      (sprite-bold?      s)
	      #:underline? (sprite-underline? s)))
(define-method (darken-sprite s) (new-sprite s))

(define-generic lighten-sprite)
(define-method (lighten-sprite (s struct:sprite))
  (new-sprite (sprite-char s)
	      #:bg         'white
	      #:fg         (let ((fg (sprite-fg s)))
			    (if (eq? fg 'white) 'black fg))
	      #:bold?      (sprite-bold?      s)
	      #:underline? (sprite-underline? s)))
(define-method (lighten-sprite s) (new-sprite s #:bg 'white #:fg 'black))
