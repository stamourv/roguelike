(define-type-of-character monster
  challenge-rating)
;; returns a function that returns monsters with the given characteristics
(define (new-monster-type name char cr)
  (lambda () (make-monster name (lambda () char) cr)))


(define monster-types
  (list (new-monster-type "goblin" #\g 1/3)
	(new-monster-type "orc"    #\o 1/2)))


(define (generate-monsters level level-no)
  TODO) ;; TODO have a number of monster points function of the level no, and have a monster level cap, again function of the level no, maybe also a function of the level of the player ?
;; TODO maybe place some as groups, or have groups of monsters as a monster choice, for a number of points, and affect them to a room, and place them randomply in the room

;; TODO receive the room graph with the level, to use it
