#lang racket

;; By default, Racket doesn't have set-car! and set-cdr! functions.  The
;; following line allows us to use those:
(require r5rs)
;; Unfortunately, it does this by making cons return a "mutable pair"
;; instead of a "pair", which means that many built-ins may fail
;; mysteriously because they expect a pair, but get a mutable pair.
;; Re-define a few common functions in terms of car and friends, which
;; the above line make work with mutable pairs.
(define first car)
(define rest cdr)
(define second cadr)
(define third caddr)
(define fourth cadddr)
;; We also tell DrRacket to print mutable pairs using the compact syntax
;; for ordinary pairs.
(print-as-expression #f)
(print-mpair-curly-braces #f)


"Problem 1"
(define (make-table)
  #f)
(define (table? table)
  #f)
(define (table-put! table key value)
  #f)
(define (table-has-key? table key)
  #f)
(define (table-get table key)
  #f)

"Problem 2"
(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))

;; make-monitored

"Problem 3"

;; make-num-calls-table

"Problem 4"

;; memoize

"Problem 5 (optional)"

;; advise

"Problem 6 (optional)"

;; make-monitored-with-advice


;; Allow this file to be included from elsewhere, and export all defined
;; functions from it.
(provide (all-defined-out))
