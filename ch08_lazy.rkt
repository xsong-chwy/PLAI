#lang plai

;; Arithmetic Expression with Identifiers and Functions and Lazy evaluation (call-by-name)
(define-type CFAE/L
  [num (n number?)]
  [add (lhs CFAE/L?) (rhs CFAE/L?)]
  [id (name symbol?)]
  [fun (param symbol?) (body CFAE/L?)]
  [app (fun-expr CFAE/L?) (arg-expr CFAE/L?)])

;; value of CFAE/L expressions
(define-type CFAE/L-Value
  [numV (n number?)]
  [closureV (param symbol?)
            (body CFAE/L?)
            (env Env?)]
  [exprV (expr CFAE/L?)
         (env Env?)])

;; the environment
(define-type Env
  [mtSub]
  [aSub (name symbol?) (value CFAE/L-Value?) (env Env?)])

;; preprocess: sexp -> CFAE/L
(define (preprocess with-sexp)
  (local ([define bound-id (first (second with-sexp))]
          [define named-expr (parse (second (second with-sexp)))]
          [define bound-body (parse (third with-sexp))])
    (app (fun bound-id bound-body) named-expr)))

;; parse: sexp -> CFAE/L
(define (parse sexp)
  (cond
    [(number? sexp) (num sexp)]
    [(symbol? sexp) (id sexp)]
    [(list? sexp) (case (first sexp)
                    [(+) (add (parse (second sexp)) (parse (third sexp)))]
                    [(with) (preprocess sexp)]
                    [(fun) (fun (first (second sexp)) (parse (third sexp)))]
                    [else (app (parse (first sexp)) (parse (second sexp)))])]))

;; lookup: symbol Env -> CFAE/L-Value
(define (lookup name ds)
  (type-case Env ds
    [mtSub () (error 'lookup "no binding for identifier")]
    [aSub (bound-name bound-value rest-ds)
          (if (symbol=? name bound-name)
              bound-value
              (lookup name rest-ds))]))

;; num+: CFAE/L-Value CFAE/L-Value -> CFAE/L-Value
(define (num+ num1 num2)
  (numV (+ (numV-n (strict num1)) (numV-n (strict num2)))))

;; strict: CFAE/L-Value -> CFAE/L-Value
(define (strict e)
  (type-case CFAE/L-Value e
    [exprV (expr env)
           (local ([define the-value (strict (interp expr env))])
             (begin
               (printf "forcing exprV to ~a~n" the-value)
               the-value))]
    [else e]))

;; interp: CFAE/L listof(Env) -> CFAE/L-Value 
(define (interp expr env)
  (type-case CFAE/L expr
    [num (n) (numV n)]
    [add (l r) (num+ (interp l env) (interp r env))]
    [id (v) (lookup v env)]
    [fun (param body)
         (closureV param body env)]
    [app (fun-expr arg-expr)
         (local ([define closure-val (strict (interp fun-expr env))]
                 [define arg-val (exprV arg-expr env)])
           (interp (closureV-body closure-val)
                   (aSub (closureV-param closure-val)
                         arg-val
                         (closureV-env closure-val))))]))

;; final-interp: 
(define (final-interp expr)
  (strict (interp expr (mtSub))))

;; tests
(final-interp (parse '{{with {x 3} {fun {y} {+ x y}}} 4}))
(final-interp (parse '{with {double {fun {x} {+ x x}}} {double {double 2}}}))
(interp       (parse '{with {x {+ 1 1}} x}) (mtSub))
(final-interp (parse '{with {x {+ 1 1}} x}))

