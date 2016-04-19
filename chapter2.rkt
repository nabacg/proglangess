#lang eopl

(define identifier? symbol?)

(define-datatype l-exp l-exp?
  (var-exp
   (var identifier?))
  (lambda-exp
   (bound-var identifier?)
   (body l-exp?))
  (app-exp
   (rator l-exp?)
   (rand l-exp?)))

(display "Hello World" )

(define occurs-free?
  (lambda (search-var exp)
    (cases l-exp exp
      (var-exp (var) (eqv? var search-var))
      (lambda-exp (bound-var body)
                  (and (not (eqv? bound-var search-var))
                       (occurs-free? search-var body)))
      (app-exp (rator rand)
               (or (occurs-free? rator)
                   (occurs-free? rand))))))


(display (occurs-free? 'x (lambda-exp 'x (var-exp 'x))))
(display (occurs-free? 'y (lambda-exp 'x (var-exp 'y))))