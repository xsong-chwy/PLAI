#lang plai

;; Arithmetic Expression with Identifiers and Functions and Mutation through Boxes
(define-type BCFAE
  [num (n number?)]
  [add (lhs BCFAE?) (rhs BCFAE?)]
  [id (name symbol?)]
  [if0 (test BCFAE?) (ontrue BCFAE?) (onfalse BCFAE?)]
  [fun (param symbol?) (body BCFAE?)]
  [app (fun-expr BCFAE?) (arg-expr BCFAE?)]
  [newbox (content BCFAE?)]
  [setbox (box BCFAE?) (content BCFAE?)]
  [openbox (box BCFAE?)]
  [seqn (first-stat BCFAE?) (second-stat BCFAE?)])

;; Environment
(define-type Env
  [mtSub]
  [aSub (name symbol?) (location number?) (env Env?)])

;; Store
(define-type Store
  [mtSto]
  [aSto (location number?) (value BCFAE-Value?) (store Store?)])

;; value of BCFAE expressions
(define-type BCFAE-Value
  [numV (n number?)]
  [closureV (param symbol?) (body BCFAE?) (env Env?)]
  [boxV (location number?)])

;; preprocess: sexp -> BCFAE
(define (preprocess with-sexp)
  (local ([define bound-id (first (second with-sexp))]
          [define named-expr (parse (second (second with-sexp)))]
          [define bound-body (parse (third with-sexp))])
    (app (fun bound-id bound-body) named-expr)))

;; parse: sexp -> BCFAE
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
                    [(newbox) (newbox (parse (second sexp)))]
                    [(setbox) (setbox (parse (second sexp)) (parse (third sexp)))]
                    [(openbox) (openbox (parse (second sexp)))]
                    [(seqn) (seqn (parse (second sexp)) (parse (third sexp)))]
                    [else (app (parse (first sexp)) (parse (second sexp)))])]))

;; env-lookup: symbol Env -> location
(define (env-lookup name env)
  (type-case Env env
    [mtSub () (error 'env-lookup "no binding for identifier")]
    [aSub (bound-name bound-location rest-env)
          (if (symbol=? name bound-name)
              bound-location
              (env-lookup name rest-env))]))

;; store-lookup: location Store -> BCFAE-Value
(define (store-lookup loc-index sto)
  (type-case Store sto
    [mtSto () (error 'store-lookup "no value at location")]
    [aSto (location value rest-store)
          (if (= loc-index location)
              value
              (store-lookup loc-index rest-store))]))

;; Value and Store pair
(define-type Value*Store
  [v*s (value BCFAE-Value?) (store Store?)])

;; add-num: BCFAE BCFAE -> BCFAE
;; returns a num which is the result of adding the contents of two given nums
(define (num+ num1 num2)
  (numV (+ (numV-n num1) (numV-n num2))))

;; num-zero? BCFAE BCFAE -> boolean
;; returns whether a numV represents zero
(define (num-zero? num)
  (zero? (numV-n num)))

;; next-location: Store -> number
(define (next-location store)
  (type-case Store store
    [mtSto () 0]
    [aSto (location value rest-store)
          (+ 1 (next-location rest-store))]))

;; interp: BCFAE Env Store -> Value*Store
(define (interp expr env store)
  (type-case BCFAE expr
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
                (type-case Value*Store (interp arg-expr env fun-store)
                  [v*s (arg-val arg-store)
                       (local ([define new-loc (next-location arg-store)])
                       (interp (closureV-body fun-val)
                               (aSub (closureV-param fun-val)
                                     new-loc
                                     (closureV-env fun-val))
                               (aSto new-loc
                                     arg-val
                                     arg-store)))])])]
    [newbox (value-expr)
            (type-case Value*Store (interp value-expr env store)
              [v*s (value value-store)
                   (local ([define new-loc (next-location value-store)])
                     (v*s (boxV new-loc)
                          (aSto new-loc
                                value
                                value-store)))])]
    [setbox (box-expr value-expr)
            (type-case Value*Store (interp box-expr env store)
              [v*s (box-value box-store)
                   (type-case Value*Store (interp value-expr env box-store)
                     [v*s (value value-store)
                          (v*s value
                               (aSto (boxV-location box-value)
                                     value
                                     value-store))])])]
    [openbox (box-expr)
             (type-case Value*Store (interp box-expr env store)
               [v*s (box-val box-store)
                    (v*s (store-lookup (boxV-location box-val) box-store)
                         box-store)])]
    [seqn (e1 e2)
          (type-case Value*Store (interp e1 env store)
            [v*s (e1-val e1-store)
                 (interp e2 env e1-store)])]))

;; final-interp: BCFAE -> Value
(define (final-interp expr)
  (v*s-value (interp expr (mtSub) (mtSto))))

;; tests
(final-interp (parse '{{with {x 3} {fun {y} {+ x y}}} 4}))
(final-interp (parse '{with {double {fun {x} {+ x x}}} {double {double 2}}}))
(final-interp (parse '{with {switch {newbox 1}}
                            {with {toggle {fun {dummy}
                                               {if0 {openbox switch}
                                                    {seqn {setbox switch 1}
                                                          1}
                                                    {seqn {setbox switch 0}
                                                          0}}}}
                                  {+ {toggle 1729} {+ {toggle 1729} {toggle 1729}}}}}))

