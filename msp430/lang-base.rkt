#lang rosette/safe

(require "../lib/bv.rkt" "../lib/mem_simple.rkt")

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

; truncation and masking
(define-syntax-rule (trunc20 x)
  (bvand x (mspx-bv #xfffff)))

(define-syntax-rule (trunc16 x)
  (bvand x (mspx-bv #x0ffff)))

(define-syntax-rule (trunc8 x)
  (bvand x (mspx-bv #x000ff)))

(define-syntax-rule (high8 x)
  (bvand x (mspx-bv #x0ff00)))

(define-syntax-rule (high8->low x)
  (trunc8 (bvlshr x (mspx-bv 8))))

(define-syntax-rule (low8->high x)
  (bvshl (trunc8 x) (mspx-bv 8)))

(define-syntax-rule (trunc1 x)
  (bvand x (mspx-bv #x00001)))

(define-syntax-rule (mask1 x)
  (bvand x (mspx-bv #xffffe)))

(define-syntax-rule (sign16 x)
  (bvand x (mspx-bv #x08000)))

(define-syntax-rule (sign8 x)
  (bvand x (mspx-bv #x00080)))

; address lookup
(define-syntax-rule (addr->integer addr)
  (bitvector->integer (bvlshr addr (mspx-bv 1))))

; memory dereference

(define-syntax-rule (memory-ref20 m addr)
  (let ([loword (memory-ref m (addr->integer addr))]
        [hiword (memory-ref m (+ (addr->integer addr) 1))])
    (bvor (trunc16 loword) (bvshl (bvand hiword (mspx-bv #x0000f)) (mspx-bv 16)))))

(define-syntax-rule (memory-ref16 m addr)
  (trunc16 (memory-ref m (addr->integer addr))))

(define-syntax-rule (memory-ref8 m addr)
  (if (bveq (trunc1 addr) (mspx-bv 0))
      (trunc8 (memory-ref m (addr->integer addr)))
      (high8->low (memory-ref m (addr->integer addr)))))

; memory assignment

(define-syntax-rule (memory-set20! m addr x)
  (let ([loword (trunc16 x)]
        [hiword (bvlshr x (mspx-bv 16))])
      (begin (memory-set! m (addr->integer addr) loword)
             (memory-set! m (+ (addr->integer addr) 1) hiword))))

(define-syntax-rule (memory-set16! m addr x)
  (memory-set! m (addr->integer addr) (trunc16 x)))

(define-syntax-rule (memory-set8! m addr x)
  (let ([val (memory-ref m (addr->integer addr))])
    (if (bveq (trunc1 addr) (mspx-bv 0))
        (memory-set! m (addr->integer addr) (bvor (high8 val) (trunc8 x)))
        (memory-set! m (addr->integer addr) (bvor (bvshl (trunc8 x) (mspx-bv 8)) (trunc8 val))))))

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

; limited parametric versions

(define-syntax-rule (mspx->. width x)
  (case width
    [(8) (mspx->byte x)]
    [(16) (mspx->word x)]
    [(20) x]))

(define-syntax-rule (ext->. width x)
  (case width
    [(8) (ext->byte x)]
    [(16) (ext->word x)]
    [(20) (ext->mspx x)]))

(define-syntax-rule (trunc. width x)
  (case width
    [(8) (trunc8 x)]
    [(16) (trunc16 x)]))

(define-syntax-rule (register-ref. width r n)
  (case width
    [(8) (register-ref8 r n)]
    [(16) (register-ref16 r n)]
    [(20) (register-ref r n)]))

(define-syntax-rule (memory-ref. width m n)
  (case width
    [(8) (memory-ref8 m n)]
    [(16) (memory-ref16 m n)]
    [(20) (memory-ref20 m n)]))

(define-syntax-rule (memory-set.! width m n val)
  (case width
    [(8) (memory-set8! m n val)]
    [(16) (memory-set16! m n val)]
    [(20) (memory-set20! m n val)]))

(define-syntax-rule (bytewidth. width)
  (case width
    [(8) 1]
    [(16) 2]
    [(20) 4]))
