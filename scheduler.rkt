#lang racket

(require "class.rkt" "utilities.rkt")
(provide (all-defined-out))

(define-generic turn)
(define-method (turn x reschedule?) #f)

(define turn-no    0) ; in seconds, reset when the level is changed
;; to preserve the ordering from turn to turn, in case of identical speeds
(define turn-id    0)
(define turn-queue '())

(define (reset-turn-no)     (set! turn-no 0))
(define (increment-turn-no) (set! turn-no (add1 turn-no)))
(define (reset-turn-id)     (set! turn-id 0))
(define (reset-turn-queue)  (set! turn-queue '()))

(define (schedule thunk t)
  (set! turn-queue (cons (list t turn-id thunk) turn-queue))
  (set! turn-id (+ turn-id 1)))

(define-generic reschedule)
(define-method (reschedule x) #f)

(define (find-next-active)
  (let* ((minimum (apply min (map car turn-queue)))
	 (next (filter (lambda (x) (= (car x) minimum)) turn-queue)))
    ;; TODO all these list traversals might be costly
    (set! turn-no minimum)
    (for-each (lambda (x) (set! turn-queue (remove x turn-queue))) next)
    ;; order by turn-id, to preserve ordering in the case of identical speeds
    (map caddr (sort next < #:key cadr))))
