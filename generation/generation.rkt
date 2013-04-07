#lang racket

(require "dungeon.rkt"
         "encounters.rkt"
         "treasure.rkt")
(provide generate-floor)

(define (generate-floor no player-level [pos-stairs-up #f])
  (let ((floor (generate-dungeon-floor pos-stairs-up)))
    ;; add everything else on top
    (place-encounters floor player-level)
    (place-treasure   floor no player-level)
    floor))
