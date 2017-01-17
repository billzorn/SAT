#lang rosette/safe

(require "../lib/bv.rkt")

(provide (all-defined-out))

; macro for defining structures
(define-syntax (define-instruction stx)
  (syntax-case stx ()
    [(_ [id kind])
     #'(begin (struct id kind () #:transparent))]
    [(_ [id kind] more ...)
     #'(begin
         (define-instruction [id kind])
         (define-instruction more ...))]))

; execution helper macros

; truncation
(define-syntax-rule (trunc16 x)
  (bvand x (mspx-bv #x0ffff)))

(define-syntax-rule (trunc8 x)
  (bvand x (mspx-bv #x000ff)))

(define-syntax-rule (high8 x)
  (trunc8 (bvlshr x (mspx-bv 8))))

(define-syntax-rule (trunc1 x)
  (bvand x (mspx-bv #x00001)))

; address lookup
(define-syntax-rule (addr->integer addr)
  (bitvector->integer (bvlshr addr (mspx-bv 1))))

; memory dereference
(define-syntax-rule (memory-ref16 memory addr)
  (trunc16 (vector-ref memory (addr->integer addr))))

(define-syntax-rule (memory-ref8 memory addr)
  (if (bveq (trunc1 addr) (mspx-bv 0))
      (trunc8 (vector-ref memory (addr->integer addr)))
      (high8 (vector-ref memory (addr->integer addr)))))

; memory assignment
(define-syntax-rule (memory-set16! memory addr x)
  (vector-set! memory (addr->integer addr) (trunc16 x)))

; this is horrible
(define-syntax-rule (memory-set8! memory addr x)
  (let ([m (vector-ref memory (addr->integer addr))])
    (if (bveq (trunc1 addr) (mspx-bv 0))
        (vector-set! memory (addr->integer addr) (bvor (bvshl (high8 m) (mspx-bv 8)) (trunc8 x)))
        (vector-set! memory (addr->integer addr) (bvor (bvshl (trunc8 x) (mspx-bv 8)) (trunc8 m))))))

; register dereference
(define-syntax-rule (register-ref registers r)
  (vector-ref registers r))

(define-syntax-rule (register-ref16 registers r)
  (trunc16 (register-ref registers r)))

(define-syntax-rule (register-ref8 registers r)
  (trunc8 (register-ref registers r)))

; register assignment
(define-syntax-rule (register-set! registers r x)
  (vector-set! registers r x))

(define-syntax-rule (register-set16! registers r x)
  (vector-set! registers r (trunc16 x)))

(define-syntax-rule (register-set8! registers r x)
  (vector-set! registers r (trunc8 x)))
