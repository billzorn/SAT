#lang racket/signature

impl-bv             ; (integer? -> bitvector?)
decode-operand      ; (bitvector? -> addr-expression?)
mmap-ref            ; (state? integer? bitvector? -> bitvector?)
mmap-set!           ; (state? integer? bitvector? bitvector? -> void?)
