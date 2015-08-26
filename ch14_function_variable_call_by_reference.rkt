#lang plai

;; Arithmetic Expression with Identifiers and Functions and Variables and Call-By-Reference
(define-type RVCFAE
  [num (n number?)]
  [add (lhs RVCFAE?) (rhs RVCFAE?)]
  [id (name symbol?)]
  [if0 (test RVCFAE?) (ontrue RVCFAE?) (onfalse RVCFAE?)]
  [fun (param symbol?) (body RVCFAE?)]
  [app (fun-expr RVCFAE?) (arg-expr RVCFAE?)]
  [mutate (var symbol?) (value-expr RVCFAE?)]
  [seqn (e1 RVCFAE?) (e2 RVCFAE?)]
  [refun (param symbol?) (body RVCFAE?)])

;; Environment
(define-type Env
  [mtSub]
  [aSub (name symbol?) (location number?) (env Env?)])

;; Store
(define-type Store
  [mtSto]
  [aSto (location number?) (value RVCFAE-Value?) (store Store?)])

;; value of RVCFAE expressions
(define-type RVCFAE-Value
  [numV (n number?)]
  [closureV (param symbol?) (body RVCFAE?) (env Env?)]
  [refclosureV (param symbol?) (body RVCFAE?) (env Env?)])

;; preprocess: sexp -> RVCFAE
(define (preprocess with-sexp)
  (local ([define bound-id (first (second with-sexp))]
          [define named-expr (parse (second (second with-sexp)))]
          [define bound-body (parse (third with-sexp))])
    (app (fun bound-id bound-body) named-expr)))

;; parse: sexp -> RVCFAE
(define (parse sexp)
  (cond
    [(number? sexp) (num sexp)]
    [(symbol? sexp) (id sexp)]
    [(list? sexp) (case (first sexp)
                    [(+) (add (parse (second sexp)) (parse (third sexp)))]
                    [(with) (preprocess sexp)]
                    [(if0) (if0 (parse (second sexp))
                                (parse (third sexp))
                                (parse (fourth sexp)))]
                    [(fun) (fun (first (second sexp)) (parse (third sexp)))]
                    [(set) (mutate (second sexp) (parse (third sexp)))]
                    [(seqn) (seqn (parse (second sexp)) (parse (third sexp)))]
                    [(refun) (refun (first (second sexp)) (parse (third sexp)))]
                    [else (app (parse (first sexp)) (parse (second sexp)))])]))

;; env-lookup: symbol Env -> location
(define (env-lookup name env)
  (type-case Env env
    [mtSub () (error 'env-lookup "no binding for identifier")]
    [aSub (bound-name bound-location rest-env)
          (if (symbol=? name bound-name)
              bound-location
              (env-lookup name rest-env))]))

;; store-lookup: location Store -> RVCFAE-Value
(define (store-lookup loc-index sto)
  (type-case Store sto
    [mtSto () (error 'store-lookup "no value at location")]
    [aSto (location value rest-store)
          (if (= loc-index location)
              value
              (store-lookup loc-index rest-store))]))

;; Value and Store pair
(define-type Value*Store
  [v*s (value RVCFAE-Value?) (store Store?)])

;; add-num: RVCFAE RVCFAE -> RVCFAE
;; returns a num which is the result of adding the contents of two given nums
(define (num+ num1 num2)
  (numV (+ (numV-n num1) (numV-n num2))))

;; num-zero? RVCFAE RVCFAE -> boolean
;; returns whether a numV represents zero
(define (num-zero? num)
  (zero? (numV-n num)))

;; next-location: Store -> number
(define (next-location store)
  (type-case Store store
    [mtSto () 0]
    [aSto (location value rest-store)
          (+ 1 (next-location rest-store))]))

;; interp: RVCFAE Env Store -> Value*Store
(define (interp expr env store)
  (type-case RVCFAE expr
    [num (n) (v*s (numV n) store)]
    [add (l r)
         (type-case Value*Store (interp l env store)
           [v*s (l-value l-store)
                (type-case Value*Store (interp r env l-store)
                  [v*s (r-value r-store)
                       (v*s (num+ l-value r-value) r-store)])])]
    [id (v) (v*s (store-lookup (env-lookup v env) store) store)]
    [if0 (condition on-true on-false)
         (type-case Value*Store (interp condition env store)
           [v*s (condition-val condition-store)
                (if (num-zero? condition-val)
                    (interp on-true env condition-store)
                    (interp on-false env condition-store))])]
    [fun (param body)
         (v*s (closureV param body env) store)]
    [app (fun-expr arg-expr)
         (type-case Value*Store (interp fun-expr env store)
           [v*s (fun-val fun-store)
                (type-case RVCFAE-Value fun-val
                  [closureV (cl-param cl-body cl-env)
                            (type-case Value*Store (interp arg-expr env fun-store)
                              [v*s (arg-val arg-store)
                                   (local ([define new-loc (next-location arg-store)])
                                     (interp cl-body
                                             (aSub cl-param
                                                   new-loc
                                                   cl-env)
                                             (aSto new-loc
                                                   arg-val
                                                   arg-store)))])]
                  [refclosureV (cl-param cl-body cl-env)
                               (local ([define arg-loc (env-lookup (id-name arg-expr) env)])
                                 (interp cl-body
                                         (aSub cl-param
                                               arg-loc
                                               cl-env)
                                         fun-store))]
                  [numV (_) (error 'interp "trying to apply a number")])])]
    [mutate (var value-expr)
            (type-case Value*Store (interp value-expr env store)
              [v*s (value-value value-store)
                   (local ([define the-loc (env-lookup var env)])
                     (v*s value-value
                          (aSto the-loc value-value value-store)))])]
    [seqn (e1 e2)
          (type-case Value*Store (interp e1 env store)
            [v*s (e1-val e1-store)
                 (interp e2 env e1-store)])]
    [refun (param body)
           (v*s (refclosureV param body env) store)]))

;; final-interp: RVCFAE -> Value
(define (final-interp expr)
  (v*s-value (interp expr (mtSub) (mtSto))))

;; tests
(final-interp (parse '{{with {x 3} {fun {y} {+ x y}}} 4}))
(final-interp (parse '{with {double {fun {x} {+ x x}}} {double {double 2}}}))
(final-interp (parse '{with {switch 0}
                            {with {toggle {fun {dummy}
                                               {if0 switch
                                                    {seqn {set switch 1}
                                                          1}
                                                    {seqn {set switch 0}
                                                          0}}}}
                                  {+ {toggle 1729} {+ {toggle 1729} {toggle 1729}}}}}))
(final-interp (parse '{with {v 0}
                            {with {f {refun {y} {set y 5}}}
                                  {seqn {f v}
                                        v}}}))

