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

;; the environment
(define (Env? x)
  (procedure? x))

;; mtSub: () -> Env
(define (mtSub)
  (lambda (name)
    (error 'lookup "no binding for identifier")))

;; aSub: symbol schemeVal Env -> Env
(define (aSub bound-name bound-value env)
  (lambda (name)
    (cond
      [(symbol=? name bound-name) bound-value]
      [else (env name)])))

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

;; cyclically-bind-and-interp: symbol RCFAE env -> env
(define (cyclically-bind-and-interp bound-id named-expr env)
  (local ([define rec-env
            (lambda (name)
              (cond
                [(symbol=? name bound-id)
                 (interp named-expr rec-env)]
                [else (env name)]))])
    rec-env))

;; interp: RCFAE listof(Env) -> RCFAE-Value 
(define (interp expr env)
  (type-case RCFAE expr
    [num (n) n]
    [add (l r) (+ (interp l env) (interp r env))]
    [sub (l r) (- (interp l env) (interp r env))]
    [mult (l r) (* (interp l env) (interp r env))]
    [id (v) (env v)]
    [if0 (condition-expr true-expr false-expr)
         (local ([define condition-val (interp condition-expr env)])
           (if (zero? condition-val)
               (interp true-expr env)
               (interp false-expr env)))]
    [fun (param body)
         (lambda (arg-val)
           (interp body (aSub param arg-val env)))]
    [app (fun-expr arg-expr)
         (local ([define closure-val (interp fun-expr env)]
                 [define arg-val (interp arg-expr env)])
           (closure-val arg-val))]
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
(final-interp (parse '{rec {fact {fun {x} {if0 x 1 {* x {fact {- x 1}}}}}} {fact {+ 2 3}}}))

