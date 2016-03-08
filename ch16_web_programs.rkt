#lang plai

(define the-receiver (box 'dummy-value))
(define receiver-prompt (box 'dummy-value))

;; simulate web display
(define (web-display n)
  (printf "Web output: ~a~n" n))

;; simulate web read
(define (web-read/k p k)
  (begin
    (set-box! receiver-prompt p)
    (set-box! the-receiver k)
    (error 'web-read/k "run (resume) to enter the number and simulate clicking Submit")))

(define (resume)
  (begin
    (display (unbox receiver-prompt))
    ((unbox the-receiver) (read))))

;; Original program
;(web-display
; (+ (web-read "First Number: ")
;    (web-read "Second Number: ")))

;; Transfomed program
;(web-read/k "First Number"
;            (lambda (v1)
;              (web-read/k "Second Number: "
;                          (lambda (v2)
;                            (web-display (+ v1 v2))))))