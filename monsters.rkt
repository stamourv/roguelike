#lang racket

(require "utilities/utilities.rkt"
         "utilities/class.rkt")
(require "data/items.rkt")
(require "character.rkt"
         "scheduler.rkt"
         "common.rkt")
(provide (all-defined-out))

(define-class <monster> (character)
  ;; function that takes the monster, the floor, and the position of the
  ;; player as parameters and makes the monster act
  behavior)
;; TODO have different speeds (maybe even initiative?) to determine which
;;  monster moves first

;; to handle the repetitive part of generating the hp
;; TODO could be done with a constructor ?
(define (new-monster f . args)
  (let ((m (apply f `(,@(take! args 1) ; name
		      #f #f            ; pos, floor
		      ,@(take! args 6) ; str, dex, con, int, wis, cha
		      ,(make-hash)     ; altered-attrs
		      ,@(take! args 3) ; natural-ac, level, hit-dice
		      #f #f            ; hp, max-hp
		      ,@args))))       ; rest
    (init-hp m)
    m))

(define-method (turn (m struct:monster) reschedule?)
  (when (and (> (character-hp m) 0)
             (eq? (character-floor m) (character-floor player)))
    ((behavior-fun (monster-behavior m))
     m (character-pos player))
    (when reschedule? (reschedule m)))) ;; TODO call-next-method


(define-struct behavior (fun nb-turns-idle) #:mutable #:transparent)
(define (new-behavior fun)
  (let ((b (make-behavior #f 0)))
    (set-behavior-fun! b (fun b))
    b))
