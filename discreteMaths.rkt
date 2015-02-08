#lang racket
;Successor Function - Domain N Range N
(define (s n)
  (if (< n 0)
      (error 'Domain "Domain Natural Numbers Only")
      (+ 1 n)))
;Predessor Function - Domain N 
(define (Pr n)
  (if (equal? 0 n)
      0
      (- n 1)))
;Add strips Successors off of B and put them on A
;until b is zero or has no more successors.
(define (add a b)
  (if (equal? 0 b)
      a
      (add (s a) (Pr b))))
;Subtract strips as many successors off a as long as b has successors
;Since domain is limited to N no negatives will
;be returned.
(define (subtract a b)
  (if (equal? 0 b)
      a
      (subtract (Pr a) (Pr b))))
