#lang racket/signature

impl-bv             ; (integer? -> bitvector?)
mmap-ref            ; (state? integer? bitvector? -> bitvector?)
mmap-set!           ; (state? integer? bitvector? bitvector? -> void?)
comp-addr           ; (operand? -> bitvector?)
decode              ; (stream? -> (stream? . decoded?)
dispatch            ; (op? integer? bitvector? bitvector? bitvector? -> bitvector?)
dispatch-sr         ; (op? integer? bitvector? bitvector? bitvector? bitvector? -> bitvector?)
step/read           ; (state? decoded? bitvector? -> stepctx?)
step/write          ; (state? decoded? stepctx? -> void?)

