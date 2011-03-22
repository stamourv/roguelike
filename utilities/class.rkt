#lang racket

(require swindle/clos swindle/extra)
(provide (all-defined-out))

;; wrapper over swindle to emulate David Saint-Hilaire's class.scm

(define-syntax-rule (define-generic name)
  (defgeneric name))
(define-syntax-rule (define-method (name args ...) body ...)
  (defmethod (name args ...) body ...))

;; I want defstructs, but want to keep the old name from class.scm
(define-syntax-rule (define-class x ...)
  (defstruct x ...))
