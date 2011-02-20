#lang racket

(require swindle/clos swindle/extra)
(provide (all-defined-out)
         make)

;; wrapper over swindle to emulate David Saint-Hilaire's class.scm

(define-syntax-rule (define-generic name)
  (defgeneric name))
(define-syntax-rule (define-method (name args ...) body ...)
  (defmethod (name args ...) body ...)) ;; TODO get rid of the struct: in specifiers
;; TODO not sure this will work, syntax is probably bad for fallback case
(define-syntax-rule (define-class name super-list (slot-key slot) ...)
  (defstruct name super-list slot ...)) ;; TODO get rid of slot-key
;; TODO use init args instead of all my new-X functions?
