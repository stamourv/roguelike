#lang racket

(require "../utilities/class.rkt"
         "../utilities/display.rkt")

(provide descriptions-table add-show-method)

;; for in-game help
;; alist of characters and description strings
;; filled up automatically when methods are added to show
(define descriptions-table '())
(define (add-description! sprite description)
  (set! descriptions-table (cons (cons sprite description) descriptions-table)))

(define-syntax-rule (add-show-method type sprite description)
  (begin (define-method (show (o type)) sprite)
         (add-description! sprite description)))
