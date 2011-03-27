#lang racket

(require (for-syntax (except-in syntax/parse character)
                     unstable/syntax))
(require "../utilities/class.rkt"
         "../utilities/descriptions.rkt")
(require "../data/items.rkt")
(require "character.rkt"
         "scheduler.rkt"
         "common.rkt")
(provide (all-defined-out))

(define-class <monster> (character)
  ;; function that takes the monster, the floor, and the position of the
  ;; player as parameters and makes the monster act
  behavior)

;; to handle the repetitive parts
(define-syntax (define-monster stx)
  (syntax-parse
   stx
   [(_ name (parent) sprite desc rest ...)
    #`(begin
        (define-class #,(format-id #'name "<~a>" (syntax-e #'name)) (parent))
        (add-show-method #,(format-id #'name "struct:~a" (syntax-e #'name))
                         'monster sprite desc)
        (define (#,(format-id #'name "new-~a" (syntax-e #'name)))
          (new-monster #,(format-id #'name "make-~a" (syntax-e #'name))
                       #,(symbol->string (syntax-e #'name))
                       rest ...)))]))

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
