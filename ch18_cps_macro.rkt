#lang plai

(define the-receiver (box 'dummy-value))
(define receiver-prompt (box 'dummy-value))

;; simulate web display
(define (web-display n)
  (printf "Web output: ~a~n" n))

;; simulate web read
(define (web-read/k p k)
  (begin
    (set-box! receiver-prompt p)
    (set-box! the-receiver k)
    (error 'web-read/k "run (resume) to enter the number and simulate clicking Submit")))

(define (resume)
  (begin
    (display (unbox receiver-prompt))
    ((unbox the-receiver) (read))))

;; macro for define-cps
(define-syntax define-cps
  (syntax-rules ()
    [(define-cps (f arg) body)
     (define-cps f (lambda (arg) body))]
    [(define-cps v val)
     (define v ((cps val) (lambda (x) x)))]))

;; macro for cps
(define-syntax cps
  (syntax-rules (+ lambda web-read)
    [(cps (+ e1 e2))
     (lambda (k)
       ((cps e1) (lambda (l-val)
                   ((cps e2) (lambda (r-val)
                               (k (+ l-val r-val)))))))]
    [(cps (lambda (a) body))
     (lambda (k)
       (k (lambda (a dyn-k)
            ((cps body) dyn-k))))]
    [(cps (web-read prompt))
     (lambda (k)
       (web-read/k prompt k))]
    [(cps (f a))
     (lambda (k)
       ((cps f) (lambda (f-val)
                  ((cps a) (lambda (a-val)
                             (f-val a-val k))))))]
    [(cps v)
     (lambda (k)
       (k v))]))

;; macro for run
(define-syntax run
  (syntax-rules ()
    [(run e)
     ((cps e) (lambda (x) (error 'run "terminating with value ~a" x)))]))

;; tests
(define-cps g (lambda (x) (+ x x)))
(define-cps (h f) (lambda (x) (f x)))
(define-cps (dummy x) ((h g) 10))
(run (dummy 1729))

;; test for [(cps (lambda (a) body)) ...] case
(g 10 (lambda (x) (+ 1 x))) ; correct result is 21. If we use k instead of dyn-k in the closure,
                            ; then the result will be 20 which is wrong
