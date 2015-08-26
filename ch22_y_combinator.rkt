#lang racket

(define fact
  ((lambda (mk-fact)
     (mk-fact mk-fact))
   (lambda (f)
     (lambda (n)
       (if (zero? n)
           1
           (* n ((f f) (sub1 n))))))))

(define make-recursive-procedure
  (lambda (p)
    ((lambda (f) (f f))
     (lambda (f)
       (p (lambda (x) ((f f) x)))))))

(define factorial
  (make-recursive-procedure
   (lambda (fact)
     (lambda (n)
       (if (zero? n)
           1
           (* n (fact (sub1 n))))))))