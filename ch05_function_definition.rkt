#lang plai

;; Arithmetic Expression with Identifiers and Functions
(define-type F1WAE
  [num (n number?)]
  [add (lhs F1WAE?) (rhs F1WAE?)]
  [sub (lhs F1WAE?) (rhs F1WAE?)]
  [with (name symbol?) (named-expr F1WAE?) (body F1WAE?)]
  [id (name symbol?)]
  [app (fun-name symbol?) (arg-expr F1WAE?)])

;; Function Definition
(define-type FunDef
  [fundef (fun-name symbol?) (arg-name symbol?) (body F1WAE?)])

;; Deferred Substitution Repository
(define-type DeferredSub
  [mtSub]
  [aSub (name symbol?) (value number?) (ds DeferredSub?)])

;; parse: sexp -> WAE
(define (parse sexp)
  (cond
    [(number? sexp) (num sexp)]
    [(symbol? sexp) (id sexp)]
    [(list? sexp) (case (first sexp)
                    [(+) (add (parse (second sexp)) (parse (third sexp)))]
                    [(-) (sub (parse (second sexp)) (parse (third sexp)))]
                    [(with)
                     (local ([define bind-id (first (second sexp))]
                             [define bind-expr (second (second sexp))]
                             [define body (third sexp)])
                       (with bind-id (parse bind-expr) (parse body)))]
                    [else (app (first sexp) (parse (second sexp)))])]))

;; lookup: symbol DeferredSub -> F1WAE
(define (lookup name ds)
  (type-case DeferredSub ds
    [mtSub () (error 'lookup "no binding for identifier")]
    [aSub (bound-name bound-value rest-ds)
          (if (symbol=? name bound-name)
              bound-value
              (lookup name rest-ds))]))

;; subst: WAE symbol WAE -> WAE 
(define (subst expr sub-id val)
  (type-case F1WAE expr
    [num (n) expr]
    [add (l r) (add (subst l sub-id val)
                    (subst r sub-id val))]
    [sub (l r) (sub (subst l sub-id val)
                    (subst r sub-id val))]
    [with (bound-id named-expr bound-body)
          (if (symbol=? bound-id sub-id)
              (with bound-id
                    (subst named-expr sub-id val)
                    bound-body)
              (with bound-id
                    (subst named-expr sub-id val)
                    (subst bound-body sub-id val)))]
    [id (v) (if (symbol=? v sub-id) val expr)]
    [app (fun-name arg-expr)
         (app fun-name (subst arg-expr sub-id val))]))

;; lookup: symbol listof(FunDef) -> FunDef
(define (lookup-fundef fun-name fun-defs)
  (local ([define matched-defs
            (filter (lambda (fun-def) (symbol=? fun-name (fundef-fun-name fun-def)))
                    fun-defs)])
    (first matched-defs)))

;; interp: F1WAE listof(FunDef) -> number
(define (interp expr fun-defs ds)
  (type-case F1WAE expr
    [num (n) n]
    [add (l r) (+ (interp l fun-defs ds) (interp r fun-defs ds))]
    [sub (l r) (- (interp l fun-defs ds) (interp r fun-defs ds))]
    [with (bound-id named-expr bound-body)
          (interp bound-body
                  fun-defs
                  (aSub bound-id
                        (interp named-expr fun-defs ds)
                        ds))]
    [id (v) (lookup v ds)]
    [app (fun-name arg-expr)
         (local ([define the-fun-def (lookup-fundef fun-name fun-defs)])
           (interp (fundef-body the-fun-def)
                   fun-defs
                   (aSub (fundef-arg-name the-fun-def)
                         (interp arg-expr fun-defs ds)
                         (mtSub))))]))

;; tests
(test (interp (parse '{double {double 5}})
              (list (fundef 'double 'n (add (id 'n) (id 'n)))) (mtSub))
      20)
