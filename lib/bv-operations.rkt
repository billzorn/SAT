#lang rosette
(require "../lib/bv.rkt")
(provide (all-defined-out))

(define (sr-carry20 sr) (if (bit-set? sr 0) (bv 1 20) (bv 0 20)))

(define (pass x) x)
(define (eq0 x) (if (= (bitvector->natural x) 0) (bv 1 1) (bv 0 1)))
(define (sr-carry sr)       (if (bit-set? sr 0) (bv 1 1) (bv 0 1)))
(define (sr-zero sr)        (if (bit-set? sr 1) (bv 1 1) (bv 0 1)))
(define (sr-negative sr)    (if (bit-set? sr 2) (bv 1 1) (bv 0 1)))
(define (sr-overflow sr)    (if (bit-set? sr 8) (bv 1 1) (bv 0 1)))
(define (bit8 x) (if (bit-set? x 8) (bv 1 1) (bv 0 1)))
(define (bit9 x) (if (bit-set? x 9) (bv 1 1) (bv 0 1)))
(define (bit16 x) (if (bit-set? x 16) (bv 1 1) (bv 0 1)))
(define (bit17 x) (if (bit-set? x 17) (bv 1 1) (bv 0 1)))
(define (diffsign8 x y) (if (xor (bit-set? x 8) (bit-set? y 8)) (bv 1 1) (bv 0 1)))
(define (samesign8 x y) (if (not (xor (bit-set? x 8) (bit-set? y 8))) (bv 1 1) (bv 0 1)))
(define (diffsign16 x y) (if (xor (bit-set? x 16) (bit-set? y 16)) (bv 1 1) (bv 0 1)))
(define (samesign16 x y) (if (not (xor (bit-set? x 16) (bit-set? y 16))) (bv 1 1) (bv 0 1)))
(define (bveq2 x y) (if (bveq x y) (bv 1 1) (bv 0 1)))
(define (shr4 x) (bvlshr x (bv 4 20)))
(define (msp_dcarry x) (if (or (bvsgt x 9) (bvslt x 0)) (+ x (bv 6 20)) x))

(define (n4-up width)
  (λ (valfn carryfn sr op1 op2)
    (let ([c (sr-carry sr)])
      (for/fold ([val (bv 0 20)]) 
                ([i (in-range 0 width 4)])
        (let* ([op1 (bvand op1 (bv #xff 20))]
               [op2 (bvand op2 (bv #xff 20))]
               [dst (valfn c op1 op2)]
               [c- (carryfn sr op1 op2 dst)])
          (set! c c-)
          (bvor val (bvshl (bvand dst #xff) (bv i 20))))))))

(define n4-up.b (n4-up 8))
(define n4-up.w (n4-up 16))
(define n4-up.a (n4-up 20))

(define (n4-up/c width)
  (λ (valfn carryfn sr op1 op2)
    (let ([c (sr-carry sr)])
      (for/fold ([val (bv 0 20)]) 
                ([i (in-range 0 width 4)])
        (let* ([op1 (bvand op1 (bv #xff 20))]
               [op2 (bvand op2 (bv #xff 20))]
               [dst (valfn c op1 op2)]
               [c- (carryfn sr op1 op2 dst)])
          (set! c c-)
          (bvor val (bvshl (bvand dst #xff) (bv i 20)))))
      c)))

(define n4-up.b/c (n4-up 8))
(define n4-up.w/c (n4-up 16))
(define n4-up.a/c (n4-up 20))
