#lang racket

(require swindle/clos swindle/extra)
(provide (all-defined-out)
         make)

;; wrapper over swindle to emulate David Saint-Hilaire's class.scm

(define-syntax-rule (define-generic name)
  (defgeneric name))
(define-syntax-rule (define-method (name args ...) body ...)
  (defmethod (name args ...) body ...))
;; TODO get rid of the struct: in specifiers

;; I want defstructs, but want to keep the old name from class.scm
(define-syntax-rule (define-class x ...)
  (defstruct x ...))
