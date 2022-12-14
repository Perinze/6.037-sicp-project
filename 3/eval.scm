#lang racket
;;
;; eval.scm - 6.037
;;
(require r5rs)
(define first car)
(define second cadr)
(define third caddr)
(define fourth cadddr)
(define rest cdr)
;; Tell DrRacket to print mutable pairs using the compact syntax for
;; ordinary pairs.
(print-as-expression #f)
(print-mpair-curly-braces #f)

;; mutable cons cell version of map
(define (mmap f lst)
  (if (null? lst)
      '()
      (cons (f (car lst)) (mmap f (cdr lst)))))

(define (tagged-list? exp tag)
  (and (pair? exp) (eq? (car exp) tag)))

(define (self-evaluating? exp)
  (cond ((number? exp) #t)
        ((string? exp) #t)
        ((boolean? exp) #t)
        (else #f)))

(define (quoted? exp) (tagged-list? exp 'quote))
(define (text-of-quotation exp) (cadr exp))

(define (variable? exp) (symbol? exp))
(define (assignment? exp) (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))
(define (make-assignment var expr)
  (list 'set! var expr))
;; ==== QUESTION 4 ====
(define (undo? exp) (tagged-list? exp 'unset!))
(define (undo-variable exp) (cadr exp))
(define (make-undo var expr)
  (list 'unset! var expr))
;; ==== QUESTION 4 ====

(define (definition? exp) (tagged-list? exp 'define))
(define (definition-variable exp)
  (if (symbol? (cadr exp))   (cadr exp)   (caadr exp)))
(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp) (cddr exp))))  ; formal params, body
(define (make-define var expr)
  (list 'define var expr))

(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters lambda-exp) (cadr lambda-exp))
(define (lambda-body lambda-exp) (cddr lambda-exp))
(define (make-lambda parms body) (cons 'lambda (cons parms body)))

(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp) (cadddr exp))
(define (make-if pred conseq alt) (list 'if pred conseq alt))

(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define first-cond-clause car)
(define rest-cond-clauses cdr)
(define (make-cond seq) (cons 'cond seq))

(define (let? expr) (tagged-list? expr 'let))
(define (let-bound-variables expr) (mmap first (second expr)))
(define (let-values expr) (mmap second (second expr)))
(define (let-body expr) (cddr expr)) ;differs from lecture--body may be a sequence
(define (make-let bindings body)
  (cons 'let (cons bindings body)))

(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions begin-exp) (cdr begin-exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))
(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))
(define (make-begin exp) (cons 'begin exp))

;; ==== QUESTION 2 ====
(define (and? exp) (tagged-list? exp 'and))
;; ==== QUESTION 2 ====

;; ==== QUESTION 3 ====
(define (until? exp)
  ;(printf "until?: (car expr) is ~s~n" (car exp))
  ;(printf "until?: returning ~s~n" (tagged-list? exp 'until))
  (tagged-list? exp 'until))
(define (until-test expr) (cadr expr))
(define (until-exp expr) (cddr expr))
;; ==== QUESTION 3 ====

;; ==== QUESTION 5 ====
(define (proc-env? exp) (tagged-list? exp 'procedure-env))
(define (proc-env-arg exp) (second exp))
(define (curr-env? exp) (tagged-list? exp 'current-env))
;; ==== QUESTION 5 ====

(define (application? exp) (pair? exp))
(define (operator app) (car app))
(define (operands app) (cdr app))
(define (no-operands? args) (null? args))
(define (first-operand args) (car args))
(define (rest-operands args) (cdr args))
(define (make-application rator rands)
  (cons rator rands))

(define (time? exp) (tagged-list? exp 'time))

;;
;; this section is the actual implementation of meval
;;

(define (m-eval exp env)
  ;(displayln exp) ; debug
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ;; ==== QUESTION 4 ====
        ((undo? exp) (eval-undo exp env))
        ;; ==== QUESTION 4 ====
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp) (lambda-body exp) env))
        ((begin? exp) (eval-sequence (begin-actions exp) env))
        ((cond? exp) (m-eval (cond->if exp) env))
        ((let? exp) (m-eval (let->application exp) env))
        ((time? exp) (time (m-eval (second exp) env)))
        ;; ==== QUESTION 2 ====
        ((and? exp) (eval-and (operands exp) env))
        ;; ==== QUESTION 2 ====
        ;; ==== QUESTION 3 ====
        ((until? exp) (m-eval (until->transformed exp) env))
        ;; ==== QUESTION 3 ====
        ;; ==== QUESTION 5 ====
        ((proc-env? exp) (eval-proc-env exp env))
        ((curr-env? exp) (eval-curr-env env))
        ;; ==== QUESTION 5 ====
        ((application? exp)
         (m-apply (m-eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else (error "Unknown expression type -- EVAL" exp))))

(define (m-apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment (make-frame (procedure-parameters procedure)
                                          arguments)
                              (procedure-environment procedure))))
        (else (error "Unknown procedure type -- APPLY" procedure))))

(define (list-of-values exps env)
  (cond ((no-operands? exps) '())
        (else (cons (m-eval (first-operand exps) env)
                    (list-of-values (rest-operands exps) env)))))

(define (eval-if exp env)
  (if (m-eval (if-predicate exp) env)
      (m-eval (if-consequent exp) env)
      (m-eval (if-alternative exp) env)
      ))

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (m-eval (first-exp exps) env))
        (else (m-eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (m-eval (assignment-value exp) env)
                       env))
;; ==== QUESTION 4 ====
(define (eval-undo exp env)
  (unset-variable-value! (undo-variable exp)
                         env))
;; ==== QUESTION 4 ====

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (m-eval (definition-value exp) env)
                    env))

;; ==== QUESTION 2 ====
(define (eval-and exp env)
  (let ((result (m-eval (first-exp exp) env)))
    (cond ((null? exp) #t)
          ((last-exp? exp) result)
          (result (eval-and (rest-exps exp) env))
          (else #f))))
;; ==== QUESTION 2 ====

(define (let->application expr)
  (let ((names (let-bound-variables expr))
        (values (let-values expr))
        (body (let-body expr)))
    (make-application (make-lambda names body)
                      values)))

(define (cond->if expr)
  (let ((clauses (cond-clauses expr)))
    (if (null? clauses)
        #f
        (if (eq? (car (first-cond-clause clauses)) 'else)
            (sequence->exp (cdr (first-cond-clause clauses)))
            (make-if (car (first-cond-clause clauses))
                     (sequence->exp (cdr (first-cond-clause clauses)))
                     (make-cond (rest-cond-clauses clauses)))))))

;; ==== QUESTION 3 ====
(define (until->transformed expr)
  (define (add-loop until-exp loop)
    (cond ((null? until-exp) (cons loop null))
          (else (cons (car until-exp)
                      (add-loop (cdr until-exp) loop)))))
  (define (make-until-begin until-exp loop)
    (cons 'begin (add-loop until-exp loop)))
  (make-let '()
            (list (make-define '(loop)
                               (make-if (until-test expr)
                                        #t
                                        (make-until-begin (until-exp expr) '(loop))))
                  '(loop))))
;; ==== QUESTION 3 ====

;; ==== QUESTION 5 ====
(define (eval-proc-env expr env)
  (box (proc-env (proc-env-arg expr) env)))
(define (eval-curr-env env)
  (box env))
;; ==== QUESTION 5 ====



(define input-prompt ";;; M-Eval input level ")
(define output-prompt ";;; M-Eval value:")

(define (driver-loop) (repl #f))

(define (repl port)
  (if port #f (prompt-for-input input-prompt))
  (let ((input (if port (read port) (read))))
    (cond ((eof-object? input)   'meval-done)
          ((eq? input '**quit**) 'meval-done)
          (else
           (let ((output (m-eval input the-global-environment)))
             (if port #f (begin
                           (announce-output output-prompt)
                           (pretty-display output)))
             (repl port))))))

(define (prompt-for-input string)
  (newline) (newline) (display string) (display meval-depth) (newline))

(define (announce-output string)
  (newline) (display string) (newline))


;;
;;
;; implementation of meval environment model
;;

; double bubbles
(define (make-procedure parameters body env)
  (list 'procedure parameters body env))
(define (compound-procedure? proc)
  (tagged-list? proc 'procedure))
(define (procedure-parameters proc) (second proc))
(define (procedure-body proc) (third proc))
(define (procedure-environment proc) (fourth proc))


; bindings
;; ==== QUESTION 4 ====
(define (make-binding var val)
  (list 'binding var (list val)))
;; ==== QUESTION 4 ====
(define (binding? b)
  (tagged-list? b 'binding))
(define (binding-variable binding)
  (if (binding? binding)
      (second binding)
      (error "Not a binding: " binding)))
;; ==== QUESTION 4 ====
(define (binding-value binding)
  (if (binding? binding)
      (first (third binding))
      (error "Not a binding: " binding)))
(define (set-binding-value! binding val)
  (if (binding? binding)
      (set-car! (cddr binding) (cons val (caddr binding)))
      (error "Not a binding: " binding)))
(define (unset-binding-value! binding)
  (let ((variables (caddr binding)))
    (if (binding? binding)
        (if (not (null? (cdr variables)))
            (set-car! (cddr binding) (cdr variables))))))
;; ==== QUESTION 4 ====

; frames
(define (make-frame variables values)
  (define (make-frame-bindings rest-vars rest-vals)
    (cond ((and (null? rest-vars) (null? rest-vals))
           '())
          ((null? rest-vars)
           (error "Too many args supplied" variables values))
          ((symbol? rest-vars)
           (list (make-binding rest-vars rest-vals)))
          ((null? rest-vals)
           (error "Too few args supplied" variables values))
          (else
           (cons (make-binding (car rest-vars) (car rest-vals))
                 (make-frame-bindings (cdr rest-vars) (cdr rest-vals))))))
  (make-frame-from-bindings (make-frame-bindings variables values)))

(define (make-frame-from-bindings list-of-bindings)
  (cons 'frame list-of-bindings))

(define (frame? frame)
  (tagged-list? frame 'frame))
(define (frame-variables frame)
  (if (frame? frame)
      (mmap binding-variable (cdr frame))
      (error "Not a frame: " frame)))
(define (frame-values frame)
  (if (frame? frame)
      (mmap binding-value (cdr frame))
      (error "Not a frame: " frame)))
(define (add-binding-to-frame! binding frame)
  (if (frame? frame)
      (if (binding? binding)
          (set-cdr! frame (cons binding (cdr frame)))
          (error "Not a binding: " binding))
      (error "Not a frame: " frame)))
(define (find-in-frame var frame)
  (define (search-helper var bindings)
    (if (null? bindings)
        #f
        (if (eq? var (binding-variable (first bindings)))
            (first bindings)
            (search-helper var (rest bindings)))))
  (if (frame? frame)
      (search-helper var (cdr frame))
      (error "Not a frame: " frame)))

; environments
(define the-empty-environment '(environment))
(define (extend-environment frame base-env)
  (if (environment? base-env)
      (if (frame? frame)
          (list 'environment frame base-env)
          (error "Not a frame: " frame))
      (error "Not an environment: " base-env)))
(define (environment? env)
  (tagged-list? env 'environment))
(define (enclosing-environment env)
  (if (environment? env)
      (if (eq? the-empty-environment env)
          (error "No enclosing environment of the empty environment")
          (third env))
      (error "Not an environment: " env)))
(define (environment-first-frame env)
  (if (environment? env)
      (second env)
      (error "Not an environment: " env)))
(define (find-in-environment var env)
  (if (eq? env the-empty-environment)
      #f
      (let ((frame (environment-first-frame env)))
        (let ((binding (find-in-frame var frame)))
          (if binding
              binding
              (find-in-environment var (enclosing-environment env)))))))

;; ==== QUESTION 5 ====
(define (proc-env proc-name env)
  (procedure-environment
   (lookup-variable-value proc-name
                          env)))
;; ==== QUESTION 5 ====

; name rule
(define (lookup-variable-value var env)
  (let ((binding (find-in-environment var env)))
    (if binding
        (binding-value binding)
        (error "Unbound variable -- LOOKUP" var))))

(define (set-variable-value! var val env)
  (let ((binding (find-in-environment var env)))
    (if binding
        (set-binding-value! binding val)
        (error "Unbound variable -- SET" var))))

;; ==== QUESTION 4 ====
(define (unset-variable-value! var env)
  (let ((binding (find-in-environment var env)))
    (if binding
        (unset-binding-value! binding)
        (error "Unbound variable -- UNSET" var))))
;; ==== QUESTION 4 ====

(define (define-variable! var val env)
  (let ((frame (environment-first-frame env)))
    (let ((binding (find-in-frame var frame)))
      (if binding
          (set-binding-value! binding val)
          (add-binding-to-frame!
           (make-binding var val)
           frame)))))

; primitives procedures - hooks to underlying Scheme procs
(define (make-primitive-procedure implementation)
  (list 'primitive implementation))
(define (primitive-procedure? proc) (tagged-list? proc 'primitive))
(define (primitive-implementation proc) (cadr proc))
(define (primitive-procedures)
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'set-car! set-car!)
        (list 'set-cdr! set-cdr!)
        (list 'null? null?)
        (list '+ +)
        (list '- -)
        (list '< <)
        (list '> >)
        (list '= =)
        (list 'display display)
        (list 'not not)
        ;; ==== QUESTION 1 ====
        (list '* *)
        (list '/ /)
        (list 'list list)
        (list 'cadr cadr)
        (list 'cddr cddr)
        (list 'caadr caadr)
        (list 'caddr caddr)
        (list 'cdadr cdadr)
        (list 'cdddr cdddr)
        (list 'cadddr cadddr)
        (list 'cddddr cddddr)
        (list 'newline newline)
        (list 'printf printf)
        (list 'length length)
        (list 'symbol? symbol?)
        (list 'pair? pair?)
        (list 'eq? eq?)
        (list 'number? number?)
        (list 'string? string?)
        (list 'boolean? boolean?)
        ;; ==== QUESTION 1 ====
        ;; ==== QUESTION 5 ====
        (list 'env-variables env-variables)
        (list 'env-parent env-parent)
        (list 'env-value env-value)
        ;; ==== QUESTION 5 ====
        ))

;; ==== QUESTION 5 ====
(define (env-variables env-box)
  (let ((env (unbox env-box)))
    (frame-variables (environment-first-frame env))))
(define (env-parent env-box)
  (let ((env (unbox env-box)))
    (box (enclosing-environment env))))
(define (env-value var env-box)
  (let ((env (unbox env-box)))
    (lookup-variable-value var env)))
;; ==== QUESTION 5 ====

(define (primitive-procedure-names) (mmap car (primitive-procedures)))

(define (primitive-procedure-objects)
  (mmap make-primitive-procedure (mmap cadr (primitive-procedures))))

(define (apply-primitive-procedure proc args)
  (apply (primitive-implementation proc) args))

; used to initialize the environment
(define (setup-environment)
  (extend-environment (make-frame (primitive-procedure-names)
                                  (primitive-procedure-objects))
                      the-empty-environment))

(define the-global-environment (setup-environment))



;;;;;;;; Code necessary for question 6
;;
;; This section doesn't contain any user-servicable parts -- you
;; shouldn't need to edit it for any of the questions on the project,
;; including question 5.  However, if you're curious, comments provide a
;; rough outline of what it does.

;; Keep track of what depth we are into nesting
(define meval-depth 1)

;; These procedures are needed to make it possible to run inside meval
(define additional-primitives
  (list (list 'eof-object?      eof-object?)
        (list 'read             read)
        (list 'read-line        read-line)
        (list 'open-input-file  open-input-file)
        (list 'this-expression-file-name
                    (lambda () (this-expression-file-name)))
        (list 'pretty-display   pretty-display)
        (list 'error            error)
        (list 'apply            m-apply))) ;; <-- This line is somewhat interesting
(define stubs
  '(require r5rs mzlib/etc print-as-expression print-mpair-curly-braces))
(define additional-names (mmap first additional-primitives))
(define additional-values (mmap make-primitive-procedure
                               (mmap second additional-primitives)))

(require mzlib/etc)
(define (load-meval-defs)
  ;; Jam some additional bootstrapping structures into the global
  ;; environment
  (set! the-global-environment
        (extend-environment
         (make-frame stubs
                     (mmap (lambda (name)
                             (m-eval '(lambda (x) x) the-global-environment)) stubs))
         (extend-environment
          (make-frame additional-names
                      additional-values)
          the-global-environment)))
  ;; Open this file for reading
  (let ((stream (open-input-file (this-expression-file-name))))
    (read-line stream) ;; strip off "#lang racket" line
    (repl stream))     ;; feed the rest of the definitions into meval

  ;; Update the meval-depth variable inside the environment we're simulating
  (set-variable-value! 'meval-depth (+ meval-depth 1) the-global-environment)
  'loaded)
