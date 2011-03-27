#lang racket

(require "../utilities/grid.rkt"
         "../utilities/display.rkt")
(require "cell.rkt"
         "floor.rkt"
         "character.rkt"
         "monsters.rkt"
         "items.rkt"
         "visibility.rkt"
         "pathfinding.rkt")
(provide (all-defined-out))

;; if we would pass through another monster, take into account the number of
;; turns it has been stuck there, to avoid congestion
(define (anti-congestion-heuristic g pos)
  (let ((occ (cell-occupant (grid-ref g pos))))
    (if (and occ (monster? occ))
        (* (behavior-nb-turns-idle
            (monster-behavior occ))
           5)
        0)))

(define (move-or-increment-idle map monster dest)
  (let ((pos (character-pos monster)))
    (move map monster dest)
    (when (equal? pos (character-pos monster))
      ;; we did not move, increment idle counter
      (let ((b (monster-behavior monster)))
        (set-behavior-nb-turns-idle! b (+ (behavior-nb-turns-idle b) 1))))))


(define-syntax-rule
  (define-behavior (name monster monster-pos player-pos map)
    ([state-var state-expr] ...)
    body ...)
  (define (name)
    (new-behavior
     (let ([state-var state-expr] ...)
       (lambda (monster player-pos)
         (let ([monster-pos (character-pos monster)]
               [map         (floor-map (character-floor monster))])
           body ...))))))


(define-behavior (rush-behavior monster pos player-pos map)
  ()
  (when (line-of-sight? map pos player-pos)
    (let ([next (find-path
                 map pos player-pos
                 #:extra-heuristic anti-congestion-heuristic)])
      (when next (move-or-increment-idle map monster next)))))

(define-behavior (pursue-behavior monster pos player-pos map)
  ([last-player-pos #f])
  (let* ([target (cond ((line-of-sight? map pos player-pos)
                        (set! last-player-pos player-pos)
                        player-pos)
                       (else last-player-pos))] ; we try to pursue
         [next (and target
                    (find-path
                     map pos target
                     #:extra-heuristic anti-congestion-heuristic))])
    (when next (move-or-increment-idle map monster next))))

(define-behavior (flee-behavior monster pos player-pos map)
  ()
  (cond ((member player-pos (four-directions pos))
         ;; we are next to the player, attack
         => (lambda (pl)
              (move-or-increment-idle map monster player-pos)))
        ((line-of-sight? map pos player-pos)
         ;; flee
         (let* ((x     (point-x pos))
                (y     (point-y pos))
                (dx    (- (point-x player-pos) x))
                (dy    (- (point-y player-pos) y))
                (adx   (abs dx))
                (ady   (abs dy))
                (vert  (lambda ()
                         (move-or-increment-idle
                          map monster
                          (new-point (- x (/ dx (max adx 1))) y))))
                (horiz (lambda ()
                         (move-or-increment-idle
                          map monster
                          (new-point x (- y (/ dy (max ady 1))))))))
           (if (> adx ady)
               ;; try to move vertically first
               (when (not (vert))  (horiz))
               ;; try to move horizontally first
               (when (not (horiz)) (vert)))))))

(define-behavior (ranged-behavior monster pos player-pos map)
  ([reloading? #f])
  (when (not (ranged-weapon? (equipment-main-hand
                              (character-equipment monster))))
    (error "monster " monster " has no ranged weapon"))
  (if reloading?
      (begin (printf "The ~a reloads.\n"
                     (character-name monster))
             (set! reloading? #f))
      (let ([targets (available-targets monster)])
        (when (not (null? targets))
          (ranged-attack monster (car targets))
          (set! reloading? #t))))) ; can shoot every other turn
