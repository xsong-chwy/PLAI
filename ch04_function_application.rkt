#lang plai

;; Arithmetic Expression with Identifiers and Function Applications
(define-type F1WAE
  [num (n number?)]
  [add (lhs F1WAE?) (rhs F1WAE?)]
  [sub (lhs F1WAE?) (rhs F1WAE?)]
  [with (name symbol?) (named-expr F1WAE?) (body F1WAE?)]
  [id (name symbol?)]
  [app (fun-name symbol?) (arg F1WAE?)])

;; Function Definition
(define-type FunDef
  [fundef (fun-name symbol?) (arg-name symbol?) (body F1WAE?)])

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
                    [(app) (app (second sexp) (parse (third sexp)))])]))

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
(define (interp expr fun-defs)
  (type-case F1WAE expr
    [num (n) n]
    [add (l r) (+ (interp l fun-defs) (interp r fun-defs))]
    [sub (l r) (- (interp l fun-defs) (interp r fun-defs))]
    [with (bound-id named-expr bound-body)
          (interp (subst bound-body bound-id (num (interp named-expr fun-defs))))]
    [id (v) (error 'interp "free identifier")]
    [app (fun-name arg-expr)
         (local ([define the-fun-def (lookup-fundef fun-name fun-defs)])
           (interp (subst (fundef-body the-fun-def)
                          (fundef-arg-name the-fun-def)
                          (num (interp arg-expr fun-defs)))
                   fun-defs))]))

;; tests
(test (interp (parse '{app double {app double 5}})
              (list (fundef 'double 'n (add (id 'n) (id 'n)))))
      20)
