#lang rosette/safe

(provide (all-defined-out))

(define (make-vector size init-val)
  (letrec ([f (lambda (l n) (if (> n 0) (f (cons init-val l) (- n 1)) l))])
    (list->vector (f '() size))))

(define-syntax-rule (make-memory hi-addr zero) (make-vector hi-addr zero))

(define-syntax-rule (memory-ref m a) (vector-ref m a))

(define-syntax-rule (memory-set! m a val) (vector-set! m a val))

(define-syntax-rule (memory-copy m) (vector-copy! m))
