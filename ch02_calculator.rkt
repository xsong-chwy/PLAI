#lang plai

;; Arithmetic expressions
(define-type AE
  [num (n number?)]
  [add (lhs AE?)
       (rhs AE?)]
  [sub (lhs AE?)
       (rhs AE?)])

;; parser: sexp -> AE
(define (parse sexp)
  (cond
    [(number? sexp) (num sexp)]
    [(list? sexp) (case (first sexp)
                    [(+) (add (parse (second sexp)) (parse (third sexp)))]
                    [(-) (sub (parse (second sexp)) (parse (third sexp)))])]))

;; calc: AE -> number
(define (calc ae)
  (type-case AE ae
    [num (n) n]
    [add (l r) (+ (calc l) (calc r))]
    [sub (l r) (- (calc l) (calc r))]))

;; tests
(test (calc (parse '3)) 3)
(test (calc (parse '{+ 3 4})) 7)
(test (calc (parse '{+ {- 3 4} 7})) 6)