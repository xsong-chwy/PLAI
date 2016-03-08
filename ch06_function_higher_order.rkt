#lang plai

;; Arithmetic Expression with Identifiers and Functions
(define-type FAE
  [num (n number?)]
  [add (lhs FAE?) (rhs FAE?)]
  [id (name symbol?)]
  [fun (param symbol?) (body FAE?)]
  [app (fun-expr FAE?) (arg-expr FAE?)])

;; Deferred Substitution Repository
(define-type DeferredSub
  [mtSub]
  [aSub (name symbol?) (value FAE-value?) (ds DeferredSub?)])

;; value of FAE expressions
(define-type FAE-value
  [numV (n number?)]
  [closureV (param symbol?) (body FAE?) (ds DeferredSub?)])

;; preprocess: sexp -> FAE
(define (preprocess with-sexp)
  (local ([define bound-id (first (second with-sexp))]
          [define named-expr (parse (second (second with-sexp)))]
          [define bound-body (parse (third with-sexp))])
    (app (fun bound-id bound-body) named-expr)))

;; parse: sexp -> FAE
(define (parse sexp)
  (cond
    [(number? sexp) (num sexp)]
    [(symbol? sexp) (id sexp)]
    [(list? sexp) (case (first sexp)
                    [(+) (add (parse (second sexp)) (parse (third sexp)))]
                    [(with) (preprocess sexp)]
                    [(fun) (fun (first (second sexp)) (parse (third sexp)))]
                    [else (app (parse (first sexp)) (parse (second sexp)))])]))

;; lookup: symbol DeferredSub -> FAE
(define (lookup name ds)
  (type-case DeferredSub ds
    [mtSub () (error 'lookup "no binding for identifier")]
    [aSub (bound-name bound-value rest-ds)
          (if (symbol=? name bound-name)
              bound-value
              (lookup name rest-ds))]))

;; subst: FAE symbol FAE -> FAE 
(define (subst expr sub-id val)
  (type-case FAE expr
    [num (n) expr]
    [add (l r) (add (subst l sub-id val)
                    (subst r sub-id val))]
    [id (v) (if (symbol=? v sub-id) val expr)]
    [fun (param body)
         (if (symbol=? param sub-id)
             expr
             (fun param (subst body sub-id val)))]
    [app (fun-expr arg-expr)
         (app (subst fun-expr sub-id val) (subst arg-expr sub-id val))]))

;; add-num: FAE FAE -> FAE
;; return a num which is the result of adding the contents of two given nums
(define (add-num num1 num2)
  (numV (+ (numV-n num1) (numV-n num2))))

;; interp: FAE -> FAE
;(define (interp expr)
;  (type-case FAE expr
;    [num (n) expr]
;    [add (l r) (add-numbers (interp l) (interp r))]
;    [id (v) (error 'interp "free identifier")]
;    [fun (param body) expr]
;    [app (fun-expr arg-expr)
;         (local ([define fun-val (interp fun-expr)])
;           (interp (subst (fun-body fun-val)
;                          (fun-param fun-val)
;                          (interp arg-expr))))]))

;; interp: FAE listof(DeferredSub) -> FAE-value 
(define (interp expr ds)
  (type-case FAE expr
    [num (n) (numV n)]
    [add (l r) (add-num (interp l ds) (interp r ds))]
    [id (v) (lookup v ds)]
    [fun (param body)
         (closureV param body ds)]
    [app (fun-expr arg-expr)
         (local ([define closure-val (interp fun-expr ds)])
           (interp (closureV-body closure-val)
                   (aSub (closureV-param closure-val)
                         (interp arg-expr ds)
                         (closureV-ds closure-val))))]))


;; tests
(interp (parse '{{with {x 3} {fun {y} {+ x y}}} 4}) (mtSub))
(interp (parse '{with {double {fun {x} {+ x x}}} {double {double 2}}}) (mtSub))

