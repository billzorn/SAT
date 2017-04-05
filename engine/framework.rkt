#lang rosette

(provide framework@ framework^
         state state-mmaps state?
         addr-expression constant ref add mul)

(require "implementation-sig.rkt")

; Domain Specific Language for describing operations over processor state

(struct addr-expression () #:transparent)

(struct constant addr-expression (i) #:transparent)
(struct ref addr-expression (map addr) #:transparent)
(struct add addr-expression (a1 a2) #:transparent)
(struct mul addr-expression (a1 i) #:transparent)

; Engine framework

(struct state (mmaps) #:transparent)

(define-signature framework^
  (decode/read    ; (bitvector? -> state? -> bitvector?)
   decode/write)) ; (bitvector? -> state? bitvector? -> void?

(define-unit framework@
  (import implementation^)
  (export framework^)

  ; The following functions evaluate addressing queries to actual reads/writes
  ; They can be thought of as simple interpreters of the DSL that actually run
  ; queries written in it. 

  ; execute-read takes an addressing query and performs the actual value retrieval from
  ; the state.
  (define (perform-read addr state)
    (match addr
      [(constant i) (impl-bv i)]
      [(ref map a1) (mmap-ref state map (perform-read a1 state))]
      [(add a1 a2) (bvadd (perform-read a1 state) (perform-read a2 state))]
      [(mul a1 i) (bvmul (impl-bv i) (perform-read a1 state))]))

  (define (evaluate-write addr)
    (match addr
      [(constant i) (impl-bv i)]
      [(add a1 a2) (bvadd (evaluate-write a1) (evaluate-write a2))]
      [(mul a1 i) (bvmul (impl-bv i) (evaluate-write a1))]))

  (define (perform-write addr state val)
    (match addr
      ; The outermost expression of a write must be a ref.
      [(ref map addr) (mmap-set! state map (evaluate-write addr) val)]))

  (define decode/read
    (lambda (encoding)
      (let ([addr (decode-operand encoding)])
        (lambda (state) (perform-read addr state)))))
  
  (define decode/write
    (lambda (encoding)
      (let ([addr (decode-operand encoding)])
        (lambda (state val) (perform-write addr state val))))))

; Specification question:
;  - Who is responsible for knowing how to get the PC/SR?
;  - More broadly, how much of the spec of execute is the responsibility of
;    client impls? i.e. if we have the following spec for execute:
;       (pc sr op1 op2) -> (pc sr ai dest)
;    is there another function that's responsible for breaking op1/op2 into 4
;    bit chunks? Is that the domain of implementations or should the framework
;    provide it? If it's provided by the framework, how does it tell the
;    implementation which instruction its executing?
; 
;  (define (decode/execute encoding)
;    (match (decode-instruction encoding)
;      [(list pc sr op1 op2)] (execute pc sr op1 op2))))
