#lang racket

(provide framework@ framework^
         state state-mmaps state?
         decoded decoded?
         operand operand?
         stepctx stepctx? stepctx-pc stepctx-sr)

(require "implementation-sig.rkt" "framework-addr.rkt")

; Engine framework

(struct state (mmaps) #:transparent)

; decoded: Common struct for passing around the result of instruction decoding
;  op: opcode of the instruction
;  bw: bitwidth of the instruction (e.g. 8 for byte, 16 for word, etc)
;  op1: an operand? struct specifying the first operand of the instruction 
;  op2: if applicable, an operand? struct specifying the second operand of the instruction 
;       (null if not applicable)
; implementations can inherit from this if they need to pass around additional
;  information from decoding
(struct decoded (op bw op1 op2) #:transparent)

; operand: Common struct for passing around the result of operand decoding
; implementations can inherit from this to specify what pieces of data their
;   operands require
(struct operand () #:transparent)

; stepctx: Common struct for passing around pieces of state between stages of
;  execution.
;   pc: current value of the program counter (presumably retrieved from the
;       register file by step/read)  
;   sr: current value of the status/flags register (presumably retrieved from
;       the register file by step/read)
;   op1: calculated value of the first operand
;   op2: calculated value of the second operand
;   dst: the result of the instruction, or null if not yet computed
; implementations can inherit from this if they require additional pieces of
;  state data.
(struct stepctx (pc [sr #:mutable] op1 op2 [dst #:mutable]) #:transparent)

(define-signature framework^
  (read-op         ; (state? integer? operand? -> bitvector?) 
   write-op        ; (state? integer? operand? bitvector? -> void) 
   step))          ; (state? instr-stream? -> void?)

(define-unit framework@
  (import implementation^ framework-addr^)
  (export framework^)

  ; read-op and write-op perform a read or write of the state using the address
  ; computed by the implementation for the given operand
  (define (read-op state bw op)
    ((perform-read bw) state (comp-addr op)))

  (define (write-op state bw op dst)
    ((perform-write bw) state (comp-addr op) dst))

  ; step one instruction ahead in the instruction stream
  (define (step state instr-stream)
    (match (decode instr-stream) [(cons instr-stream dec)
      (let* ([ctx (step/read state dec (impl-bv (/ (decoded-bw dec) 8)))])
        (step/exec (decoded-op dec) (decoded-bw dec) ctx)
        (step/write state dec ctx)
        instr-stream)]))

  ; dispatch to the implementation-defined processor behavior once we have
  ; concrete values for operators
  (define (step/exec op bw ctx)
    (match ctx [(stepctx _ sr op1 op2 _)
      (let* ([dst (dispatch op sr op1 op2)]
             [sr  (dispatch-sr op sr op1 op2 dst)])
      (set-stepctx-dst! ctx dst)
      (set-stepctx-sr! ctx sr))]))
)

; To-do:
; ✓ load correct bitwidth from register/memory
; - constant generator
; - implement symbolic-compatible interval map
; - test other operand types
