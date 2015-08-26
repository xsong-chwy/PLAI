#lang plai

;; Arithmetic Expression with Identifiers and Functions and Recursion
(define-type RCFAE
  [num (n number?)]
  [add (lhs RCFAE?) (rhs RCFAE?)]
  [sub (lhs RCFAE?) (rhs RCFAE?)]
  [mult (lhs RCFAE?) (rhs RCFAE?)]
  [id (name symbol?)]
  [if0 (condition RCFAE?) (ontrue RCFAE?) (onfalse RCFAE?)]
  [fun (param symbol?) (body RCFAE?)]
  [app (fun-expr RCFAE?) (arg-expr RCFAE?)]
  [reca (id symbol?) (fun fun?) (body RCFAE?)])

;; value of RCFAE expressions
(define-type RCFAE-Value
  [numV (n number?)]
  [closureV (param symbol?)
            (body RCFAE?)
            (env Env?)]
  [errorV (err string?)])

;; return true if v is boxed environment value
(define (boxed-RCFAE-Value? v)
  (and (box? v) (RCFAE-Value? (unbox v))))

;; the environment
(define-type Env
  [mtSub]
  [aSub (name symbol?) (value RCFAE-Value?) (env Env?)]
  [aRecSub (name symbol?) (value boxed-RCFAE-Value?) (env Env?)])

;; preprocess: sexp -> RCFAE
(define (preprocess with-sexp)
  (local ([define bound-id (first (second with-sexp))]
          [define named-expr (parse (second (second with-sexp)))]
          [define bound-body (parse (third with-sexp))])
    (app (fun bound-id bound-body) named-expr)))

;; parse: sexp -> RCFAE
(define (parse sexp)
  (cond
    [(number? sexp) (num sexp)]
    [(symbol? sexp) (id sexp)]
    [(list? sexp) (case (first sexp)
                    [(+) (add (parse (second sexp))
                              (parse (third sexp)))]
                    [(-) (sub (parse (second sexp))
                              (parse (third sexp)))]
                    [(*) (mult (parse (second sexp))
                               (parse (third sexp)))]
                    [(if0) (if0 (parse (second sexp))
                                (parse (third sexp))
                                (parse (fourth sexp)))]
                    [(with) (preprocess sexp)]
                    [(fun) (fun (first (second sexp))
                                (parse (third sexp)))]
                    [(rec) (reca (first (second sexp))
                                  (parse (second (second sexp)))
                                  (parse (third sexp)))]
                    [else (app (parse (first sexp)) (parse (second sexp)))])]))

;; lookup: symbol Env -> RCFAE-Value
(define (lookup name env)
  (type-case Env env
    [mtSub () (error 'lookup "no binding for identifier")]
    [aSub (bound-name bound-value rest-env)
          (if (symbol=? name bound-name)
              bound-value
              (lookup name rest-env))]
    [aRecSub (bound-name boxed-bound-value rest-env)
             (if (symbol=? bound-name name)
                 (unbox boxed-bound-value)
                 (lookup name rest-env))]))

;; num+: RCFAE-Value RCFAE-Value -> RCFAE-Value
(define (num+ num1 num2)
  (numV (+ (numV-n num1) (numV-n num2))))

;; num-: RCFAE-Value RCFAE-Value -> RCFAE-Value
(define (num- num1 num2)
  (numV (- (numV-n num1) (numV-n num2))))

;; num*: RCFAE-Value RCFAE-Value -> RCFAE-Value
(define (num* num1 num2)
  (numV (* (numV-n num1) (numV-n num2))))

;; num-zero?: RCFAE-Value -> boolean
(define (num-zero? num)
  (zero? (numV-n num)))

;; cyclically-bind-and-interp: symbol RCFAE env -> env
(define (cyclically-bind-and-interp bound-id named-expr env)
  (local ([define value-holder (box (errorV "value holder!"))]
          [define new-env (aRecSub bound-id value-holder env)]
          [define named-expr-val (interp named-expr new-env)])
    (begin
      (set-box! value-holder named-expr-val)
      new-env)))

;; interp: RCFAE listof(Env) -> RCFAE-Value 
(define (interp expr env)
  (type-case RCFAE expr
    [num (n) (numV n)]
    [add (l r) (num+ (interp l env) (interp r env))]
    [sub (l r) (num- (interp l env) (interp r env))]
    [mult (l r) (num* (interp l env) (interp r env))]
    [id (v) (lookup v env)]
    [if0 (condition-expr true-expr false-expr)
         (local ([define condition-val (interp condition-expr env)])
           (if (num-zero? condition-val)
               (interp true-expr env)
               (interp false-expr env)))]
    [fun (param body)
         (closureV param body env)]
    [app (fun-expr arg-expr)
         (local ([define closure-val (interp fun-expr env)]
                 [define arg-val (interp arg-expr env)])
           (interp (closureV-body closure-val)
                   (aSub (closureV-param closure-val)
                         arg-val
                         (closureV-env closure-val))))]
    [reca (bound-id named-expr bound-body)
      (local ([define new-env (cyclically-bind-and-interp bound-id
                                                          named-expr
                                                          env)])
        (interp bound-body new-env))]))

;; final-interp: 
(define (final-interp expr)
  (interp expr (mtSub)))

;; tests
(final-interp (parse '{{with {x 3} {fun {y} {+ x y}}} 4}))
(final-interp (parse '{with {double {fun {x} {+ x x}}} {double {double 2}}}))
(final-interp (parse '{with {x {+ 1 1}} x}))
(final-interp (parse '{rec {fact {fun {x} {if0 x 1 {* x {fact {- x 1}}}}}} {fact {+ 1 2}}}))

