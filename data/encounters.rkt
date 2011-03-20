#lang racket

(require "../engine/encounters.rkt")
(require "monsters.rkt")

(provide (all-defined-out))

(define encounter-types
  (list
   (new-encounter-type 0.95 `(,new-bat ,new-bat ,new-bat))
   (new-encounter-type 0.9  `(,new-bat ,new-bat ,new-bat ,new-bat))
   (new-encounter-type 0.9  `(,new-rat ,new-rat))
   (new-encounter-type 0.95 `(,new-rat ,new-rat ,new-rat))
   (new-encounter-type 0.8  `(,new-rat ,new-rat ,new-rat ,new-rat))
   (new-encounter-type 0.9  `(,new-kobold ,new-kobold ,new-kobold))
   (new-encounter-type 0.75 `(,new-kobold ,new-kobold ,new-kobold ,new-kobold))
   (new-encounter-type 0.9  `(,new-goblin ,new-goblin))
   (new-encounter-type 0.8  `(,new-goblin ,new-goblin ,new-goblin))
   (new-encounter-type 0.85 `(,new-goblin ,new-goblin-archer))
   (new-encounter-type 0.6  `(,new-goblin ,new-goblin-archer ,new-goblin-archer))
   (new-encounter-type 0.8  `(,new-goblin ,new-goblin ,new-goblin-archer))
   (new-encounter-type 0.9  `(,new-orc ,new-orc))
   (new-encounter-type 0.8  `(,new-orc ,new-goblin ,new-goblin))))
