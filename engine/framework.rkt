#lang rosette

(provide framework@ framework^
         state state-mmaps state?
         op op?
         decoded decoded?
         operand operand?
         stepctx stepctx? stepctx-pc stepctx-sr set-stepctx-sr! set-stepctx-dst!
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
(struct decoded (op bw) #:transparent)
(struct operand () #:transparent)
(struct stepctx (pc [sr #:mutable] op1 op2 [dst #:mutable]) #:transparent)

(define-signature framework^
  (perform-write   ; (addr-expression? state? bitvector? -> void?)
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
  (define (perform-read addr state) ; TODO bitwidth
    (match addr
      [(constant i) i]
      [(ref map a1) (mmap-ref state map (perform-read a1 state))]
      [(add a1 a2) (+ (perform-read a1 state) (perform-read a2 state))]
      [(mul a1 i) (* i (perform-read a1 state))]))

  (define (evaluate-write addr)
    (match addr
      [(constant i) i]
      [(add a1 a2) (+ (evaluate-write a1) (evaluate-write a2))]
      [(mul a1 i) (* i (evaluate-write a1))]))

  (define (perform-write addr state val)
    (match addr
      ; The outermost expression of a write must be a ref.
      [(ref map addr) (mmap-set! state map (evaluate-write addr) val)]))

  (define (read-op state bw op)
    (perform-read (comp-addr op) state))

  (define (write-op state bw op dst)
    (perform-write (comp-addr op) state dst))

  (define (step state instr-stream)
    (let* ([dec (decode instr-stream)]
           [ctx (step/read state dec (impl-bv 2))])
        (step/exec (decoded-op dec) (decoded-bw dec) ctx)
        (step/write state dec ctx)))
)

; Open questions: 
; - What parts of what are currently in the msp430 implementation can be moved out?
; - Which parameters can be moved to the common framework structures?
; - What parts of the msp430 impl can be synthesized and what parts must be
;   hand-written?
