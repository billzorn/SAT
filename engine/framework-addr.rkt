#lang rosette

(provide framework-addr@ framework-addr^
         addr-expression constant ref add mul)

(require "implementation-sig.rkt")

; Domain Specific Language for describing operations over processor state

(struct addr-expression () #:transparent)

(struct constant addr-expression (i) #:transparent)
(struct ref addr-expression (map addr) #:transparent)
(struct add addr-expression (a1 a2) #:transparent)
(struct mul addr-expression (a1 i) #:transparent)
(struct switch addr-expression () #:transparent)

(define-signature framework-addr^
  (perform-read    ; (addr-expression? state? -> bitvector?)
   perform-write)) ; (addr-expression? state? bitvector? -> void?)

(define-unit framework-addr@
  (import implementation^)
  (export framework-addr^)

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
        [(add a1 a2) (bvadd ((perform-read bw) state a1) ((perform-read bw) state a2))]
        [(mul a1 i) (bvmul (impl-bv i) ((perform-read bw) state a1))])))

  (define (perform-write bw)
    (lambda (state addr val)
      (match addr
        ; The outermost expression of a write must be a ref, otherwise it's dropped.
        [(ref map addr) ((mmap-set! map bw) state ((perform-read bw) state addr) val)]
        [_ (void)])))
  )
