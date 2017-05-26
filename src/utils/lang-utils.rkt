#lang typed/racket

(struct No ())

(define-type SymTable (HashTable Symbol (U Any No)))

(define-type WExpr Any)

(: local SymTable)
(define local (make-hash))

(: make-entry (->* (SymTable Symbol) (WExpr) SymTable))
(define (make-entry tbl sym [v (No)])
  (hash-set! tbl sym v)
  tbl)
  
 
