#lang racket/signature

impl-bv   ; (integer? -> bitvector?)
decode    ; (bitvector? -> addr-expression?)
mmap-ref  ; (state? integer? bitvector? -> bitvector?)
mmap-set! ; (state? integer? bitvector? bitvector? -> void?)
