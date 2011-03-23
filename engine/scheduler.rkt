#lang racket

(require "../utilities/class.rkt")
(provide turn schedule reschedule
         increment-turn-no
         flush-turn-queue
         find-next-active)

(define-generic turn)
(define-method (turn x reschedule?) #f)

(define turn-no    0) ; in seconds, reset when the level is changed
;; to preserve the ordering from turn to turn, in case of identical speeds
(define turn-id    0)
(define turn-queue '())

(define (increment-turn-no) (set! turn-no (add1 turn-no)))

;; called when we change level. keep cross-level events (temp. effects and co)
(define (flush-turn-queue) (filter fourth turn-queue))

(define (schedule thunk duration #:cross-level [cross-level? #f])
  (set! turn-queue
        (cons (list (+ turn-no duration) turn-id thunk cross-level?)
              turn-queue))
  (set! turn-id (+ turn-id 1)))

(define-generic reschedule)
(define-method (reschedule x) #f)

(define (find-next-active)
  (let* ((minimum (apply min (map car turn-queue)))
	 (next (filter (lambda (x) (= (car x) minimum)) turn-queue)))
    (set! turn-no minimum)
    (for-each (lambda (x) (set! turn-queue (remove x turn-queue))) next)
    ;; order by turn-id, to preserve ordering in the case of identical speeds
    (map caddr (sort next < #:key cadr))))
