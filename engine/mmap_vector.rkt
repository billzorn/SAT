#lang rosette

(provide mmap-vector%)

(require "mmap.rkt")

(define (make-vector size init-val)
  (letrec ([f (lambda (l n) (if (> n 0) (f (cons init-val l) (- n 1)) l))])
    (list->vector (f '() size))))

(define mmap-vector% (class* object% (mmap)
  (init hiaddr)
  (init zero)
  (define data (make-vector hiaddr zero))
  (define/public (mmap-ref a) (vector-ref data a))
  (define/public (mmap-set! a val) (vector-set! data a val))
  (super-new)))
