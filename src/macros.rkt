#lang racket
 
(define-syntax ==>
  (syntax-rules ()
  [(=>> rand1 rand2 op) (op rand1 rand2)]))



