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
  ;; function that takes the monster and the position of the player as
  ;; parameters and makes the monster act
  behavior)

;; to handle the repetitive parts
(define-syntax (define-monster stx)
  (syntax-parse
   stx
   [(_ name (parent) sprite desc
       (stats ...) challenge-rating (hit-dice ...)
       rest ...)
    #`(begin
        (define-class #,(format-id #'name "<~a>" (syntax-e #'name)) (parent))
        (add-show-method #,(format-id #'name "struct:~a" (syntax-e #'name))
                         'monster sprite desc)
        (define (#,(format-id #'name "new-~a" (syntax-e #'name)))
          (new-monster #,(format-id #'name "make-~a" (syntax-e #'name))
                       #,(symbol->string (syntax-e #'name))
                       stats ...
                       challenge-rating (list hit-dice ...)
                       rest ...)))]))

(define (new-monster f name
                     str dex con int wis cha
                     challenge-rating hit-dice
                     behavior
                     #:natural-ac        [natural-ac 0]
                     #:base-attack-bonus [bab        0]
                     #:nb-attacks        [nb-attacks 1]
                     #:speed             [speed      6]
                     #:equipment         [equipment  (new-equipment)])
  (let ((m (f name
              #f #f       ; pos, floor
              str dex con int wis cha
              (make-hash) ; altered-attrs
              natural-ac challenge-rating hit-dice
              #f #f       ; hp, max-hp
              bab bab     ; base-attack-bonus, current-attack-bonus
              nb-attacks speed equipment behavior)))
    (init-hp m)
    m))

(define-method (turn (m struct:monster) reschedule?)
  (when (and (> (character-hp m) 0)
             (eq? (character-floor m) (character-floor player)))
    ((behavior-fun (monster-behavior m))
     m (character-pos player))
    (when reschedule? (reschedule m))))


(define-struct behavior (fun nb-turns-idle) #:mutable #:transparent)
(define (new-behavior fun) (make-behavior fun 0))
