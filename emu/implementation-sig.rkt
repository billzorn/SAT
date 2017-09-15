#lang racket/signature

; Signature of implementations for cpu-specific parts of emulation

; impl-bv: returns a rosette bitvector of the most convenient width for the
; implementation representing the given integer
impl-bv ; integer? -> bitvector?

; mmap-ref: a partially curried function that performs a lookup in the cpu
; state's memory maps given:
;   (map bw): the index of the memory map in which to look up, and the
;     bitwidth at which to perform the lookup
;   (state address): a reference to the concrete state in which to perform the
;     reference, and the address to reference
mmap-ref ; (integer? integer?) -> (state? bitvector?) -> bitvector?

; mmap-set!: a corresponding function to mmap-ref that sets the referenced
; location to a given value:
;   (map bw): the index of the memory map in which to look up, and the
;     bitwidth at which to perform the lookup
;   (state address): a reference to the concrete state in which to perform the
;     reference, and the address to reference
mmap-set!           ; (integer? integer?) -> (state? bitvector? bitvector?) -> void?

; comp-addr: evaluates a decoded addressing mode to an address query
;   operand: an operand as returned by decode
comp-addr           ; operand? -> bitvector?

; decode: decodes an operation from an instruction stream
decode              ; stream? -> decoded?

; decode-taken: returns the number of items used to decode the next instruction
; in the stream
decode-taken        ; stream? -> integer?

; dispatch: dispatches an operation to the correct synthesized instruction
; handler
;   bitwidth: the bit-width at which to perform the operation
;   op: the opcode of the instruction to run
;   sr: the status register value at the beginning of the operation
;   op1: the left operand of the instruction
;   op2: the right operand of the instruction
dispatch            ; (integer? op? bitvector? bitvector? bitvector?) -> bitvector?

; dispatch: dispatches an operation to the correct synthesized instruction
; handler to calculate the resulting status register
;   bitwidth: the bit-width at which to perform the operation
;   op: the opcode of the instruction to run
;   sr: the status register value at the beginning of the operation
;   op1: the left operand of the instruction
;   op2: the right operand of the instruction
;   dst: the result of the operation
dispatch-sr         ; (integer? op? bitvector? bitvector? bitvector? bitvector?) -> bitvector?

; step/read: creates the context for the process of stepping the emulated cpu
; through an instruction by:
; -calculating the program counter's value post-decoding
; -reading the status register from the memory maps
; -performing the actual memory lookup for the left and right operands
;   state: the emulated cpu state
;   dec: the decoded operation
;   pc-incr: the predetermined amount by which to increment the program counter
step/read           ; (state? decoded? bitvector? -> stepctx?)

; step/write: finishes off execution of an instruction by writing back out the
; program counter, status register, and result value to the corresponding places
; in the memory maps. 
step/write          ; (state? decoded? stepctx? -> void?)

