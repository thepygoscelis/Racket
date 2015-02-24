#lang racket
(define def-descriptive-statistics
  "The collection, organization, summarization, and presentation part.")
(define def-inferential-statistics
  "The analysis part, such estimation, hypothesis tests and prediction.")
(define def-population
  "Very large group which is under study.")
(define def-sample
  "Subset of the population")
(define def-parameter
  "A numerical measurement describing some characteristic of a population.")
(define def-stastic
  "A numerical measurement describing some charactersitic of a sample.")
(define def-quantitative
  "Numbers; Numerical data")
(define def-discrete
  "Data is count or frequency data")
(define def-continuous
  "Data taken by measurement")
(define def-nominal
  "Data broken up into nonoverlapping groups which cannot be ranked or ordered bsaed on any numerical characteristic (statistic).")
(define def-orginal
  "Data broken up into nonoverlapping groups which can be ranked or ordered based on a numerical characteristic (statistic).")
(define (sample-mean dataset)
  (let ([n (length dataset)])
  (for/sum ([i dataset]) (/ i n))))
(define (median dataset n)
  (let ([data (sort dataset <)])
  (if (integer? (/ n 2))
      (/ (+ (list-ref data (- (/ n 2) 1)) (list-ref data (/ n 2))) 2)
      (list-ref data (floor (/ n 2)))
      )))
(define (midrange dataset)
  (let ([x (sort dataset <)])
    (/ (+ (list-ref x 0) (list-ref x (- (length x) 1))) 2)
    ))

(define (independence)
  "P(A|B) = P(A) And P(A and B) = P(A)P(B)"
  )
(define (sample-variance dataset)
  (let ([sm (sample-mean dataset)])
  (/ (for/sum ([i dataset]) (expt (- i sm) 2)) (- (length dataset) 1)))
  )
(define (standard-deviation dataset)
  (sqrt (sample-variance dataset)))
(define (def-skewness)
  "Symmetric data, mean = median = mode
   right skewed data mean > median > mode
   left skewed data mean < median < mode"
  )
(define (z-score dataset x)
  (/ (- x (sample-mean dataset)) (standard-deviation dataset))
  )
(define (precentile n p)
  (let ([v (/ (* n p) 100)])
    (if (integer? v)
        (+ v 0.5)
        (ceiling v))
    ))
(define (get-precentile dataset p)
  (let ([sorted (sort dataset <)])
    (if (integer? (precentile (length sorted) p))
        (list-ref sorted (- (precentile (length sorted) p) 1))
        (/ (+ (list-ref sorted (inexact->exact (- (floor (precentile (length sorted) p)) 1))) (list-ref sorted (inexact->exact (- (ceiling (precentile (length sorted) p)) 1)))) 2))))
(define (five-number-summary dataset)
  (let ([n (length dataset)]
        [sorted (sort dataset <)])
    (let ([l (list-ref sorted 0)]
          [h (list-ref sorted (- n 1))]
          [tfp (get-precentile sorted 25)]
          [med (get-precentile sorted 50)]
          [sfp (get-precentile sorted 75)])
      `( ("Low" ,@l) ("Twenty-Fifth Precentile" ,@tfp) ("50th Precentile" ,@med) ("75th Precentile" ,@sfp) ("High" ,@h))
      )
   ))
(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))
(define (choose n k)
  (/ (factorial n) (* (factorial k) (factorial (- n k)))))
;n number of trials
;k number of successes
;p probability of success
(define (binomial-distribution n k p)
  (* (choose n k) (* (expt p k) (expt (- 1 p) (- n k)))))
