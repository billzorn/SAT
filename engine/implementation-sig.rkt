#lang racket/signature

impl-bv             ; integer? -> bitvector?
mmap-ref            ; (integer? integer?) -> (state? bitvector?) -> bitvector?
mmap-set!           ; (integer? integer?) -> (state? bitvector? bitvector?) -> void?
comp-addr           ; operand? -> bitvector?
decode              ; stream? -> (stream? . decoded?)
dispatch            ; (op? bitvector? bitvector? bitvector?) -> bitvector?
dispatch-sr         ; (op? bitvector? bitvector? bitvector? bitvector?) -> bitvector?
step/read           ; (state? decoded? bitvector? -> stepctx?)
step/write          ; (state? decoded? stepctx? -> void?)

