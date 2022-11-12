#lang racket

(define (step num-steps x1 x2)
  (/ (- x2 x1) num-steps))

"Part 1: Numerical integration"

"Problem 1: Bitdiddle's function"

(define (bitfunc x)
  (+ (expt x 4)
     (- (* 5 (expt x 2)))
     4))

;; Some simple test cases, based on by-eye examination of a graph of the
;; function: https://www.google.com/search?q=x^4-5*x^2%2B4   Run these,
;; and check that they match with the expectations.
(bitfunc 0)  ;; Should be 4
(bitfunc 1)  ;; Should be 0, or very close
(bitfunc 2)  ;; Should also be very close to 0
(bitfunc -1) ;; Should also also be very close to 0
(bitfunc 10) ;; Should be pretty big, and positive


"Problem 2: A rectangle under Bitdiddle's function"

(define (bitfunc-rect x1 x2)
  (* (- x2 x1)
     (bitfunc x1)))

;; Test cases:
(bitfunc-rect 0 1)   ;; Should be 4
(bitfunc-rect 0 0.5) ;; Should be 2
(bitfunc-rect 1.5 2) ;; Should be negative

"Problem 3: Integrating Bitdiddle's function"

(define (bitfunc-integral-recur num-steps x1 x2)
  (define width (step num-steps x1 x2))
  (if (< x1 x2)
      (+ (bitfunc-rect x1 (+ x1 width))
         (bitfunc-integral-recur (- num-steps 1)
                                 (+ x1 width)
                                 x2))
      0))

(define (bitfunc-integral-iter num-steps x1 x2)
  (define width (step num-steps x1 x2))
  (define (bitfunc-integral-iter-helper sum num-steps x1 x2)
    (if (< x1 x2)
        (bitfunc-integral-iter-helper (+ sum (bitfunc-rect x1 (+ x1 width)))
                                      (- num-steps 1)
                                      (+ x1 width)
                                      x2)
        sum))
  (bitfunc-integral-iter-helper num-steps x1 x2))

;; Provide your own test cases for this function.  Think about what are
;; the simplest input values to know are correct, and show that those
;; work as expected before moving on to test a couple more complicated
;; situations.


"Problem 4: Comparing the two integrators"

(define (bitfunc-integral-difference num-steps x1 x2)
  (abs (- (bitfunc-integral-recur num-steps x1 x2)
          (bitfunc-integral-iter num-steps x1 x2))))

;; Provide test cases for this one as well; only a couple should be
;; needed, as this function should be fairly straightforward.
