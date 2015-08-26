#lang racket
(define route-producer
  (local ([define resume (box false)])
    (lambda (real-send)
      (local ([define send-to (box real-send)]
              [define send (lambda (value)
                             (set-box! send-to (let/cc k
                                                 (begin
                                                   (set-box! resume k)
                                                   ((unbox send-to) value)))))])
        (if (unbox resume)
            ((unbox resume) real-send)
            (begin
              (send 'providence)
              (send 'houston)
              (send 'bangalore)))))))

(define (get producer)
  (let/cc k (producer k)))