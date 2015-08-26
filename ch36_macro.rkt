#lang plai

;; macro my-time
(define-syntax my-time
  (syntax-rules ()
    [(my-time e)
     (let ([begin-time (current-process-milliseconds)])
       (begin
         e
         (- (current-process-milliseconds) begin-time)))]))

;; tests
(my-time (expt 3 100000))


;; interpeter
(define (run machine init-state stream)
  (define (walker state stream)
    (or (empty? stream)
        (let ([transitions (cdr (assv state machine))]
              [in (first stream)])
          (let ([new-state (assv in transitions)])
            (if new-state
                (walker (cadr new-state) (rest stream))
                false)))))
  (walker init-state stream))

;; compiler
(define machine
  (letrec ([init
            (lambda (stream)
              (or (empty? stream)
                  (case (first stream)
                    [(c) (more (rest stream))]
                    [else false])))]
           [more
            (lambda (stream)
              (or (empty? stream)
                  (case (first stream)
                    [(a) (more (rest stream))]
                    [(d) (more (rest stream))]
                    [(r) (end (rest stream))]
                    [else false])))]
           [end
            (lambda (stream)
              (or (empty? stream)
                  (case (first stream)
                    [else false])))])
    init))

;; macro as compiler
(define-syntax automaton
  (syntax-rules (: to)
    [(automaton init-state
                (state : (label to target)...)...)
     (letrec ([state
               (lambda (stream)
                 (or (empty? stream)
                     (case (first stream)
                       [(label) (target (rest stream))]
                       ...
                       [else false])))]
              ...)
       init-state)]))


;; tests
(define m (automaton init
                     [init : (c to more)]
                     [more : (a to more)
                             (d to more)
                             (r to end)]
                     [end : ]))
(m '(c a d a d d r))
(m '(c a d a d d r r))