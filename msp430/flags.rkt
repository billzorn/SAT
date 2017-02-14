#lang rosette/safe

(require rosette/lib/match "../lib/bv.rkt" "lang-base.rkt")

(provide (all-defined-out))


; Flag register bitmasks
(define FLAG/C (mspx-bv #b00000001))
(define FLAG/Z (mspx-bv #b00000010))
(define FLAG/N (mspx-bv #b00000100))
(define FLAG/V (mspx-bv #b10000000))

(define-syntax-rule (clear-flags sr)
  (bvand sr (mspx-bv #b11111111111011111000)))

(define-syntax-rule (compute-flags c z n v sr)
  (bvor (clear-flags sr)
        (if c FLAG/C (mspx-bv 0))
        (if z FLAG/Z (mspx-bv 0))
        (if n FLAG/N (mspx-bv 0))
        (if v FLAG/V (mspx-bv 0))))
