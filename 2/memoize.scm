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
  (cons 'table '()))

(define (table? table)
  (cond [(pair? table) (and (eq? (car table) 'table)
                            (list? (cdr table)))]
        [else #f]))
               
(define (table-put! table key value)
  (cond [(table? table)
         (set-cdr! table
                   (cons (cons key value)
                         (cdr table)))]
        [else (error "table: not a table")]))

(define (table-has-key? table key)
  (cond [(table? table)
         (not (not (assoc key (cdr table))))]
        [else (error "table: not a table")]))
      
(define (table-get table key)
  (cond [(table? table)
         (let ([result (assoc key (cdr table))])
           (if result
               (cdr result)
               (error "table: no such key")))]
        [else (error "table: not a table")]))

"Problem 2"
(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))

;; make-monitored
(define make-monitored
  (lambda (f)
    (let ([count 0])
      (lambda (arg)
        (cond [(eq? arg 'how-many-calls?) count]
              [(eq? arg 'reset-call-count) (set! count 0)]
              [else (set! count (add1 count))
                    (f arg)])))))
;(set! fib (make-monitored fib))
;(fib 10)
;(fib 'how-many-calls?)

"Problem 3"

;; make-num-calls-table
(define (make-num-calls-table f max)
  (cond [(= max 0) (make-table)]
        [else (let ([table (make-num-calls-table f (- max 1))])
                (f 'reset-call-count)
                (f max)
                (table-put! table max (f 'how-many-calls?))
                table)]))

"Problem 4"

;; memoize
(define (memoize f)
  (let ([table (make-table)])
    (lambda (arg)
      (cond [(table-has-key? table arg) (table-get table arg)]
            [else (let ([value (f arg)])
                    (table-put! table arg value)
                    value)]))))
;(set! fib (memoize fib))

"Problem 5 (optional)"

;; advise
(define (advise func before after)
  (lambda (arg)
    (before)
    (let ([ret (func arg)])
      (after)
      ret)))

"Problem 6 (optional)"

;; make-monitored-with-advice
(define (make-monitored-with-advice f)
  (let ([mf (make-monitored f)] [depth 0])
    (advise mf
            (lambda () (set! depth (add1 depth)))
            (lambda ()
              (set! depth (sub1 depth))
              (cond [(= depth 0)
                     (display "Num calls: ")
                     (displayln (mf 'how-many-calls?))])))))
(set! fib (make-monitored-with-advice fib))


;; Allow this file to be included from elsewhere, and export all defined
;; functions from it.
(provide (all-defined-out))
