#lang rosette

(provide framework@ framework^
         state state-mmaps state?
         op op? define-ops 
         decoded decoded?
         operand operand?
         stepctx stepctx? stepctx-pc stepctx-sr
         addr-expression constant ref add mul)

(require "implementation-sig.rkt")

; Domain Specific Language for describing operations over processor state

(struct addr-expression () #:transparent)

(struct constant addr-expression (i) #:transparent)
(struct ref addr-expression (map addr) #:transparent)
(struct add addr-expression (a1 a2) #:transparent)
(struct mul addr-expression (a1 i) #:transparent)
(struct switch addr-expression () #:transparent)

; Engine framework

(struct state (mmaps) #:transparent)

(struct op () #:transparent)
(define-syntax (define-ops stx)
  (syntax-case stx ()
    [(_ [id])
     #'(begin (struct id op () #:transparent))]
    [(_ [id more ...])
     #'(begin
         (define-ops [id])
         (define-ops [more ...]))]))

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
  (perform-read    ; (addr-expression? state? -> bitvector?)
   perform-write   ; (addr-expression? state? bitvector? -> void?)
   read-op         ; (state? integer? operand? -> bitvector?) 
   write-op        ; (state? integer? operand? bitvector? -> void) 
   step))          ; (state? instr-stream? -> void?)

(define-unit framework@
  (import implementation^)
  (export framework^)

  ; The following functions evaluate addressing queries to actual reads/writes
  ; They can be thought of as simple interpreters of the DSL that actually run
  ; queries written in it.

  ; perform-read takes an addressing query and performs the actual value retrieval from
  ; the state.

  (define (perform-read bw)
    (lambda (state addr)
      (match addr
        [(constant i) i]
        [(ref map a1) ((mmap-ref map bw) state ((perform-read bw) state a1))]
        [(add a1 a2) (+ ((perform-read bw) state a1) ((perform-read bw) state a2))]
        [(mul a1 i) (* i ((perform-read bw) state a1))])))

  (define (perform-write bw)
    (lambda (state addr val)
      (match addr
        ; The outermost expression of a write must be a ref.
        [(ref map addr) ((mmap-set! map bw) state ((perform-read bw) state addr) val)])))

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
      ; TODO: zero-extend or sign-extend?
      (set-stepctx-dst! ctx (zero-extend 
        (for/fold ([result null]) 
                  ([i (in-range 0 bw 4)])
          (let* ([op1 (extract (+ i 3) i op1)]
                 [op2 (extract (+ i 3) i op2)]
                 [dst (dispatch op sr op1 op2)]
                 [sr  (dispatch-sr op sr op1 op2 dst)])
            ; Combine bits i : i+3 with existing result
            (if (null? result) dst (concat dst result))))
        (type-of (impl-bv 0))))
      (set-stepctx-sr! ctx sr)]))
)

; To-do:
; ✓ dispatch operates on 4 bits at a time
; ✓ load correct bitwidth from register/memory
; - implement symbolic-compatible interval map
; - test other operand types
