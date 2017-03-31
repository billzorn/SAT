#lang racket

(require "mmap.rkt")

(provide (all-defined-out))

(struct state (mmaps) #:transparent)

; Domain Specific Language for describing operations over processor state

(struct addr-expression () #:transparent)

(struct constant addr-expression (i) #:transparent)
(struct ref addr-expression (map addr) #:transparent)
(struct add addr-expression (a1 a2) #:transparent)
(struct mul addr-expression (a1 i) #:transparent)

; The following functions evaluate addressing queries to actual reads/writes
; They can be thought of as simple interpreters of the DSL that actually run
; queries written in it. 

; execute-read takes an addressing query and performs the actual value retrieval from
; the state.
(define (execute-read addr state)
  (match addr
    [(constant i) i]
    [(ref map addr) (send (vector-ref (state-mmaps state) map) mmap-ref (execute-read addr state))]
    [(add a1 a2) (+ (execute-read a1 state) (execute-read a2 state))]
    [(mul a1 i) (* i (execute-read a1 state))]))

(define (evaluate-write addr)
  (match addr
    [(constant i) i]
    [(add a1 a2) (+ (evaluate-write a1) (evaluate-write a2))]
    [(mul a1 i) (* i (evaluate-write a1))]))

(define (execute-write addr state val)
  (match addr
    ; The outermost expression of a write must be a ref.
    [(ref map addr) (send (vector-ref (state-mmaps state) map) mmap-set! (evaluate-write addr) val)]))

; Interpreter framework

;(define read 
;  (lambda (encoding)
;    (let ([addr (decode encoding)])
;      (lambda (state) (execute-read addr state)))))
;
;(define write
;  (lambda (encoding)
;    (let ([addr (decode encoding)])
;      (lambda (state value) (execute-write addr state value)))))
