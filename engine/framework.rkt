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
  (define (execute-read addr state)
    (match addr
      [(constant i) (impl-bv i)]
      [(ref map a1) (mmap-ref state map (execute-read a1 state))]
      [(add a1 a2) (bvadd (execute-read a1 state) (execute-read a2 state))]
      [(mul a1 i) (bvmul (impl-bv i) (execute-read a1 state))]))

  (define (evaluate-write addr)
    (match addr
      [(constant i) (impl-bv i)]
      [(add a1 a2) (bvadd (evaluate-write a1) (evaluate-write a2))]
      [(mul a1 i) (bvmul (impl-bv i) (evaluate-write a1))]))

  (define (execute-write addr state val)
    (match addr
      ; The outermost expression of a write must be a ref.
      [(ref map addr) (mmap-set! state map (evaluate-write addr) val)]))

  (define decode/read
    (lambda (encoding)
      (let ([addr (decode encoding)])
        (lambda (state) (execute-read addr state)))))
  
  (define decode/write
    (lambda (encoding)
      (let ([addr (decode encoding)])
        (lambda (state val) (execute-write addr state val))))))
