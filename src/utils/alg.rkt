#lang racket

(define operators '((+ . 1) (- . 1) (* . 2) (/ . 2)))

(define (operator? x)
  (ormap (λ (e) (eq? (car e) x)) operators))

(define (precedence o)
  (let ([a (assoc o operators)])
    (if a
        (cdr a)
        0)))

(define (operand? x)
  (and
   (or (symbol? x)
       (number? x))
   (not (or
         (left-paren? x)
         (right-paren? x)
         (operator? x)))))

(define (right-paren? x)
  (eq? x (string->symbol "?>")))

(define (left-paren? x)
  (eq? x (string->symbol "<?")))

(define (inner-i->p stack rexpr-list result-list)
  ;(displayln (format "stack=~a, rexpr=~a, result=~a" stack rexpr-list result-list))
  (if (empty? rexpr-list)
      (append (reverse result-list) stack)
      (let ((elem (car rexpr-list)))
        (cond
          ((operand? elem)
           (inner-i->p stack
                       (cdr rexpr-list)
                       (cons elem result-list)))
          ((left-paren? elem)
           (inner-i->p (cons elem stack)
                       (cdr rexpr-list)
                       result-list))
          ((operator? elem)
           (if (or (empty? stack)
                   (left-paren? (car stack)))
               (inner-i->p (cons elem stack)
                           (cdr rexpr-list)
                           result-list)
               (begin
                 (for ([i (length stack)])
                   (when (<= (precedence elem)
                             (precedence (car stack)))
                     (set! result-list (cons (car stack) result-list))
                     (set! stack (cdr stack))))
                 (inner-i->p (cons elem stack) (cdr rexpr-list) result-list))))
          ((right-paren? elem)
           (begin
             (for ([i (length stack)])
               (when (not (left-paren? (car stack)))
                 (set! result-list (cons (car stack) result-list))
                 (set! stack (cdr stack))))
             (set! stack (cdr stack))
             (inner-i->p stack (cdr rexpr-list) result-list)))
          (error "Error!")))))

(define (infix->postfix expr-list)
  (inner-i->p '() expr-list '()))

(define (listify expr)
  (define (listify-aux stack expr)
    (if (empty? expr)
        (car stack)
        (if (operator? (car expr))
            (if (>= (length stack) 2)
                (let ([e1 (first stack)]
                      [e2 (second stack)]
                      [new-stack (cddr stack)])
                  (listify-aux (cons (list (car expr)
                                           e2
                                           e1)
                                     new-stack)
                               (cdr expr)))
                (error "Stack empty!"))
            (listify-aux (cons (car expr) stack) (cdr expr)))))
  (listify-aux '() expr))
