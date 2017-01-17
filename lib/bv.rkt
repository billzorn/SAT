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
