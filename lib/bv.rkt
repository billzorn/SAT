#lang rosette/safe

(provide (all-defined-out))

; common bitwidths
(define byte-bits 8)
(define byte? (bitvector byte-bits))
(define-syntax-rule (byte x) (bv x byte-bits))

(define word-bits 16)
(define word? (bitvector word-bits))
(define-syntax-rule (word x) (bv x word-bits))

(define mspx-bits 20)
(define mspx-bv? (bitvector mspx-bits))
(define-syntax-rule (mspx-bv x) (bv x mspx-bits))

(define ext-bits 32)
(define ext-bv? (bitvector ext-bits))
(define-syntax-rule (ext-bv x) (bv x ext-bits))

; use 20 bits all the time
(current-bitwidth mspx-bits)

; bitwidth conversion
(define-syntax-rule (byte->word x)
  (concat (bv 0 8) x))
(define-syntax-rule (word->byte x)
  (extract 7 0 x))

(define-syntax-rule (word->mspx x)
  (concat (bv 0 4) x))
(define-syntax-rule (mspx->word x)
  (extract 15 0 x))

(define-syntax-rule (byte->mspx x)
  (concat (bv 0 12) x))
(define-syntax-rule (mspx->byte x)
  (extract 7 0 x))

(define-syntax-rule (mspx->ext x)
  (concat (bv 0 12) x))
(define-syntax-rule (ext->mspx x)
  (extract 19 0 x))

(define-syntax-rule (word->ext x)
  (concat (bv 0 24) x))
(define-syntax-rule (ext->word x)
  (extract 7 0 x))

(define-syntax-rule (byte->ext x)
  (concat (bv 0 24) x))
(define-syntax-rule (ext->byte x)
  (extract 7 0 x))

(define-syntax-rule (bit-set? x n) 
  (let* ([w (type-of x)]
         [two-to-n (bv (expt 2 n) w)])
    (not (bveq (bvand x two-to-n) (bv 0 w)))))

(define-syntax-rule (bits-set? x b)
  (bveq (bvand x b) b ))
