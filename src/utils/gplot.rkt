#lang racket

(struct no-arg ())

(define (gp-format x)
  (cond
    ((no-arg? x) "")
    ((string? x) (format "\"~a\"" x))
    ((symbol? x) (symbol->string x))
    ((and (list? x) (> (length x) 0) (eq? (car x) ':)) (format "[~a]" (string-join (map gp-format (cdr x)) " : ")))
    ((and (list? x) (> (length x) 0) (not (eq? (car x) ':))) (string-join (map gp-format x) ","))
    (else (format "~a" x))))

(define (gp-option opt . vals)
  (format "set ~a ~a" opt (string-join (map gp-format vals) " ")))
    
(define (gp-do plot-function expr)
  (format "~a ~a" plot-function expr))

(define (gp-plot . strs)
  (let ([command (format "/usr/local/bin/gnuplot -e '~a; pause -1; quit'" (string-join strs "; "))])
    (displayln command)
    (system command)))

(define (gp-expr-head expr)
  (car expr))

(define (gp-expr-rest expr)
  (cdr expr))

(define (gp-eval-one expr)
  (cond
    ((list? expr)
     (if (eq? (gp-expr-head expr) 'set)
         (apply gp-option (gp-expr-rest expr))
         (apply gp-do expr)))
    (else (error "must be a list"))))

(define (gp-eval exprs)
  (cond
    ((list? exprs)
     (apply gp-plot (map gp-eval-one exprs)))
    (else (error "must be a list of lists"))))
