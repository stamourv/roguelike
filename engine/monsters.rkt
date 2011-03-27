#lang racket

(require "../utilities/class.rkt")
(require "../data/items.rkt")
(require "character.rkt"
         "scheduler.rkt"
         "common.rkt")
(provide (all-defined-out))

(define-class <monster> (character)
  ;; function that takes the monster, the floor, and the position of the
  ;; player as parameters and makes the monster act
  behavior)

;; to handle the repetitive part of generating the hp
(define (new-monster f . args)
  (match args
    [(list-rest name
                str dex con int wis cha
                natural-ac level hit-dice
                rest)
     (let ((m (apply f name
                     #f #f            ; pos, floor
                     str dex con int wis cha
                     (make-hash)      ; altered-attrs
                     natural-ac level hit-dice
                     #f #f            ; hp, max-hp
                     rest)))
       (init-hp m)
       m)]))

(define-method (turn (m struct:monster) reschedule?)
  (when (and (> (character-hp m) 0)
             (eq? (character-floor m) (character-floor player)))
    ((behavior-fun (monster-behavior m))
     m (character-pos player))
    (when reschedule? (reschedule m))))


(define-struct behavior (fun nb-turns-idle) #:mutable #:transparent)
(define (new-behavior fun) (make-behavior fun 0))
