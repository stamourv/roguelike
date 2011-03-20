#lang racket

(require "../engine/encounters.rkt")
(require "monsters.rkt")

(provide (all-defined-out))

(define encounter-types
  ;; TODO have weights, since some would be more common, make it so it can
  ;;  use random-choice
  (map new-encounter-type
       ;; TODO have a "language" to define encounters types, maybe make the
       ;;  probability a function of the level-no ?
       `((,new-bat ,new-bat ,new-bat)
         (,new-bat ,new-bat ,new-bat ,new-bat)
         (,new-rat ,new-rat)
         (,new-rat ,new-rat ,new-rat)
         (,new-rat ,new-rat ,new-rat ,new-rat)
         (,new-kobold ,new-kobold ,new-kobold)
         (,new-kobold ,new-kobold ,new-kobold ,new-kobold)
         (,new-goblin ,new-goblin)
         (,new-goblin ,new-goblin ,new-goblin)
	 (,new-goblin ,new-goblin-archer)
	 (,new-goblin ,new-goblin-archer ,new-goblin-archer)
	 (,new-goblin ,new-goblin ,new-goblin-archer)
         (,new-orc ,new-orc)
         (,new-orc ,new-goblin ,new-goblin))))
