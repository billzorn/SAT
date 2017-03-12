#lang racket

; TODO: We can't use this with tests yet. Raw interval maps don't like symbolic indices:
;
; interval-map-ref: contract violation
;  expected: exact-integer?
;  given: i$16
;  in: the k argument of
;      (->i
;       ((im interval-map?) (k (im) (key-c im)))
;       ((d any/c))
;       any)

(provide (all-defined-out))

(require data/interval-map)

; This is not provided by the library for some reason
(define (interval-map-copy m)
  (define n (make-interval-map))
  (for ([(range val) (in-dict m)])
    (interval-map-set! n (car range) (cdr range) val))
  n)

(struct mem (map zero) #:transparent)

(define-syntax-rule (make-memory hi-addr zero) 
  (let ([ivmap (make-interval-map)])
    (interval-map-set! ivmap 0 hi-addr zero)
    (mem ivmap zero)))

(define-syntax-rule (memory-ref m a)
  (interval-map-ref (mem-map m) a))

(define-syntax-rule (memory-set! m a val)
  (interval-map-set! (mem-map m) a (+ a 1) val))

(define-syntax-rule (memory-copy m)
  (mem (interval-map-copy (mem-map m)) (mem-zero m)))



