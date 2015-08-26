#lang plai

;; Arithmetic Expression with Identifiers and Functions and Recursion and Continuation
(define-type KRCFAE
  [num (n number?)]
  [add (lhs KRCFAE?) (rhs KRCFAE?)]
  [sub (lhs KRCFAE?) (rhs KRCFAE?)]
  [mult (lhs KRCFAE?) (rhs KRCFAE?)]
  [id (name symbol?)]
  [if0 (condition KRCFAE?) (ontrue KRCFAE?) (onfalse KRCFAE?)]
  [fun (param symbol?) (body KRCFAE?)]
  [app (fun-expr KRCFAE?) (arg-expr KRCFAE?)]
  [bindcc (id symbol?) (body KRCFAE?)]
  [reca (id symbol?) (fun fun?) (body KRCFAE?)])

;; KRCFAE-Value
(define-type KRCFAE-Value
  [closureV (p procedure?)]
  [contV (c procedure?)])

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

;; preprocess: sexp -> KRCFAE
(define (preprocess with-sexp)
  (local ([define bound-id (first (second with-sexp))]
          [define named-expr (parse (second (second with-sexp)))]
          [define bound-body (parse (third with-sexp))])
    (app (fun bound-id bound-body) named-expr)))

;; parse: sexp -> KRCFAE
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
                    [(bindcc) (bindcc (second sexp) (parse (third sexp)))]
                    [else (app (parse (first sexp)) (parse (second sexp)))])]))

;; cyclically-bind-and-interp: symbol KRCFAE env -> env
(define (cyclically-bind-and-interp bound-id named-expr env k)
  (local ([define rec-env
            (lambda (name)
              (cond
                [(symbol=? name bound-id)
                 (interp named-expr rec-env k)]
                [else (env name)]))])
    rec-env))

;; interp: KRCFAE listof(Env) (KRCFAE-Value -> KRCFAE-Value) -> KRCFAE-Value 
(define (interp expr env k)
  (type-case KRCFAE expr
    [num (n) (k n)]
    [add (l r) (interp l
                       env
                       (lambda (lv)
                         (interp r
                                 env
                                 (lambda (rv)
                                   (k (+ lv rv))))))]
    [sub (l r) (interp l
                       env
                       (lambda (lv)
                         (interp r
                                 env
                                 (lambda (rv)
                                   (k (- lv rv))))))]
    [mult (l r) (interp l
                        env
                        (lambda (lv)
                          (interp r
                                  env
                                  (lambda (rv)
                                    (k (* lv rv))))))]
    [id (v) (k (env v))]
    [if0 (condition-expr true-expr false-expr)
         (interp condition-expr
                 env
                 (lambda (condv)
                   (if (zero? condv)
                       (interp true-expr env k)
                       (interp false-expr env k))))]
    [fun (param body)
         (k (closureV (lambda (arg-val dyn-k)
                        (interp body (aSub param arg-val env) dyn-k))))]
    [app (fun-expr arg-expr)
         (interp fun-expr
                 env
                 (lambda (fun-val)
                   (interp arg-expr
                           env
                           (lambda (arg-val)
                             (type-case KRCFAE-Value fun-val
                               [closureV (c) (c arg-val k)]
                               [contV (c) (c arg-val)])))))]
    [bindcc (cont-var body)
            (interp body
                    (aSub cont-var
                          (contV (lambda (val) (k val)))
                          env)
                    k)]
    [reca (bound-id named-expr bound-body)
          (local ([define new-env (cyclically-bind-and-interp bound-id
                                                              named-expr
                                                              env
                                                              k)])
            (interp bound-body new-env k))]))

;; final-interp: 
(define (final-interp expr)
  (interp expr (mtSub) (lambda (x) x)))

;; tests
;(final-interp (parse '{{with {x 3} {fun {y} {+ x y}}} 4}))
;(final-interp (parse '{with {double {fun {x} {+ x x}}} {double {double 2}}}))
;(final-interp (parse '{with {x {+ 1 1}} x}))
(final-interp (parse '{bindcc k 3}))
(final-interp (parse '{bindcc k {k 3}}))
(final-interp (parse '{bindcc k {+ 1 {k 3}}}))
(final-interp (parse '{+ 1 {bindcc k {+ 1 {k 3}}}}))
(final-interp (parse '{{bindcc k {k {fun {dummy} 3}}} 1729}))
(final-interp (parse '{bindcc k {k {k {k 3}}}}))
(final-interp (parse '{{{bindcc k k} {fun {x} x}} 42}))
(final-interp (parse '{rec {fact {fun {x} {if0 x 1 {* x {fact {- x 1}}}}}} {{bindcc k {k {fun {x} {fact x}}}} 5}}))

