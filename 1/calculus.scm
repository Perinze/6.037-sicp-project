#lang racket


"Part 1: Numerical integration"

"Problem 1: Integrating any function"

(define (integral func num-steps x1 x2)
  (define step (/ (- x2 x1) num-steps))
  (define (func-rect x1 x2)
    (* (- x2 x1) (func x1)))
  (define (integral-iter sum num-steps x1 x2)
    (if (< x1 x2)
        (integral-iter (+ sum (func-rect x1 (+ x1 step)))
                       (- num-steps 1)
                       (+ x1 step)
                       x2)
        sum))
  (integral-iter 0 num-steps x1 x2))

;; Test cases:

;; With only one step, the integral of y = x^2 from 3 to 5
;; should be 3^2 * 2 = 18
(integral (lambda (x) (expt x 2)) 1 3 5)
;; With two steps, we should get 3^2 + 4^2 = 25
(integral (lambda (x) (expt x 2)) 2 3 5)

"Problem 2: Area of a unit circle"

(define (approx-pi num-steps)
  (* 4 (integral (lambda (x) (sqrt (- 1 (expt x 2))))
                 num-steps
                 0
                 1)))
(approx-pi 1)   ;; Should be 4
(approx-pi 2)   ;; Hopefully lower than 4
(approx-pi 600) ;; Right to the first two decimal places?

"Problem 3: Integrating with pieces of any shape"

(define (rectangle func x1 x2)
  (* (- x2 x1)
     (func x1)))

(define (trapezoid func x1 x2)
  (/ (* (+ (func x1)
           (func x2))
        (- x2 x1))
     2))

(define (integral-with piece func num-steps x1 x2)
  (define step (/ (- x2 x1) num-steps))
  (define (integral-iter sum num-steps x1)
    (if (< x1 x2)
        (integral-iter (+ sum (piece func x1 (+ x1 step)))
                       (- num-steps 1)
                       (+ x1 step))
        sum))
  (integral-iter 0 num-steps x1))

;; Write your own test cases.  Start with checking that calling
;; (integral-with rectangle ...) is the same as calling (integral ...)
;; Then check that (integral-with trapezoid ...) produces better answers
;; for a given num-steps than the same (integral-with rectangle ...)

"Problem 4: Better approximation of pi"

(define (better-pi num-steps)
  (* 4 (integral-with trapezoid
                      (lambda (x) (sqrt (- 1 (expt x 2))))
                      num-steps
                      0
                      1)))

;; How many digits does (better-pi 600) get correct, compared to
;; the earlier (approx-pi 600) ?
(better-pi 600)

"Part 2: Symbolic differentiation"

(define (deriv-constant wrt constant)
    0)
(deriv-constant 'x 3)

"Problem 5: Derivative of a variable"

(define (deriv-variable wrt var)
  (if (eq? wrt var)
      1
      0))
(deriv-variable 'x 'x)
(deriv-variable 'x 'y)

"Problem 6: Calling the right function"

(define (derivative wrt expr)
  (cond
    ((number? expr) (deriv-constant wrt expr))
    ((symbol? expr) (deriv-variable wrt expr))
    ((list? expr) (define operator (car expr))
                  (cond
                    ((eq? operator '+) (deriv-sum wrt expr))
                    ((eq? operator '*) (deriv-product wrt expr))))
    (else (error "Don't know how to differentiate" expr))))
(derivative 'x 3)
(derivative 'x 'x)

"Problem 7: Derivative of a sum"

(define (deriv-sum wrt expr)
  (cons (car expr)
        (map (lambda (elem) (derivative wrt elem))
             (cdr expr))))
(derivative 'x '(+ x 2))

"Problem 8: Derivative of a product"

(define (deriv-product wrt expr)
  (define operands (cdr expr))
  (define a (car operands))
  (define b (car (cdr operands)))
  (define deriv-a (derivative wrt a))
  (define deriv-b (derivative wrt b))
  (list '+ (list '* a deriv-b) (list '* b deriv-a)))
(derivative 'x '(* x 3))

"Problem 9: Additional testing"

; Additional test cases for 'derivative' go here.
