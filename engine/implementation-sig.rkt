#lang racket/signature

impl-bv             ; (integer? -> bitvector?)
mmap-ref            ; (state? integer? bitvector? -> bitvector?)
mmap-set!           ; (state? integer? bitvector? bitvector? -> void?)
comp-addr           ; (operand? -> bitvector?)
decode              ; (stream? -> decoded?)
step/read           ; (state? decoded? bitvector? -> stepctx?)
step/exec           ; (op? bitvector? stepctx? -> stepctx?)
step/write          ; (state? decoded? stepctx? -> void?)

