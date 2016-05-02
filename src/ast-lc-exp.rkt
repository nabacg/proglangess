#lang eopl

;
(define-syntax equal??
    (syntax-rules ()
      ((_ x y)
       (let ((x^ x) (y^ y))
         (if (not (equal? x y))
           (eopl:error 'equal??
             "~s is not equal to ~s" 'x 'y)
             #t)))))

  (define report-unit-tests-completed
    (lambda (fn-name)
      (eopl:printf "unit tests completed: ~s~%" fn-name)))

;;Lc-exp ::= Identifier
;;       ::= proc Identifier => Lc-exp
;;       ::= Lc-exp (Lc-exp)

(define identifier? symbol?)

(define report-invalid-concrete-syntax
    (lambda (datum)
      (eopl:error "invalid concrete syntax ~s" datum)))

(define-datatype lc-exp lc-exp?
  (var-exp (var identifier?))
  (lambda-exp
   (bound-var identifier?)
   (body lc-exp?))
  (app-exp
   (rator lc-exp?)
   (rand lc-exp?)))


;;parser assuming input is scheme's lists not string!
;; SchemeList -> lc-exp
(define parse-expression
  (lambda (datum)
    (cond
      ((symbol? datum) (var-exp datum))
      ((pair? datum)
       (if (eqv? (car datum) 'lambda)
           (lambda-exp
            (car (cadr datum))
            (parse-expression (caddr datum)))
           (app-exp
            (parse-expression (car datum))
            (parse-expression (cadr datum)))))
      (else (report-invalid-concrete-syntax datum)))))

;; > (parse-expression '(lambda (x) (y x)))
;; #(struct:lambda-exp x #(struct:app-exp #(struct:var-exp y) #(struct:var-exp x)))

(equal??
 (parse-expression 'x)
 (var-exp 'x))

(equal??
 (parse-expression '(lambda (x) (y x)))
 (lambda-exp 'x (app-exp (var-exp 'y) (var-exp 'x))))

(equal??
 (parse-expression '((lambda (x) x) y))
 (app-exp (lambda-exp 'x (var-exp 'x)) (var-exp 'y)))
                   

(report-unit-tests-completed 'parse-expression)

(define unparse-lc-exp
  (lambda (exp)
    (cases lc-exp exp
        (var-exp (var) var)
      (lambda-exp (bound-var body)
                  `(lambda
                       (,bound-var)
                     ,(unparse-lc-exp body)))
      (app-exp (rator rand)
               (list
                (unparse-lc-exp rator)
                (unparse-lc-exp rand))))))

(equal??
 (unparse-lc-exp (var-exp 'x))
 'x)

(equal??
 (unparse-lc-exp (app-exp (var-exp 'f) (var-exp 'x)))
 '(f x))

(equal??
 (unparse-lc-exp (lambda-exp 'x (app-exp (var-exp 'y) (var-exp 'x))))
 '(lambda (x) (y x)))



(report-unit-tests-completed 'unparse-lc-exp)


