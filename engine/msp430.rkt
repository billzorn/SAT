#lang rosette

(require "implementation-sig.rkt"
         "framework.rkt" 
         "../lib/bv.rkt" 
         (rename-in "../lib/mem_simple.rkt" [make-memory make-memory/vector] [memory-ref memory-ref/vector] [memory-set! memory-set!/vector] [memory-copy memory-copy/vector])
         (rename-in "../lib/mem_ivmap.rkt"  [make-memory make-memory/intervalmap]))

(define (msp430-state rn hiaddr)
  (state (vector (make-memory/vector rn (mspx-bv 0)) 
                 (make-memory/intervalmap hiaddr (mspx-bv 0)))))

(define MAP/REG 0)
(define MAP/MEM 1)

; Sample implementations of the addressing modes. These could also be generated
; by synthesizing the results of several tests on the hardware.
(define (imm a) (constant a))
(define (reg r) (ref MAP/REG (constant r)))
(define (ind r) (ref MAP/MEM (reg r)))
(define (abs a) (ref MAP/MEM (constant a)))
(define (idx r i) (ref MAP/MEM (add (constant i) (reg r))))

(define-unit msp430-implementation@
  (import)
  (export implementation^)

  (define (impl-bv i) (mspx-bv i))

  ; Dummy decoder
  ; Presumably, the actual implementation would be synthesized somehow
  (define (decode src)
    (if (bveq src (mspx-bv 0))
      (begin (reg 0))
      (begin (abs 0))))

  (define (mmap-ref state map a)
    (let ([mmap (vector-ref (state-mmaps state) map)]
          [i (bitvector->integer (bvlshr a (mspx-bv 1)))])
      (match map
        [0 (vector-ref mmap i)]
        [1 (memory-ref mmap i)])))

  (define (mmap-set! state map a val)
    (let ([mmap (vector-ref (state-mmaps state) map)]
          [i (bitvector->integer (bvlshr a (mspx-bv 1)))])
      (match map
        [0 (vector-set! mmap i val)]
        [1 (memory-set! mmap i val)]))))

(define-compound-unit/infer msp430-engine@
  (import) (export implementation^ framework^)
  (link msp430-implementation@ framework@))
(define-values/invoke-unit/infer msp430-engine@)

; Test code

(define s (msp430-state 4 4))
((decode/read (mspx-bv 1)) s) 
((decode/write (mspx-bv 1)) s (mspx-bv 17))
((decode/read (mspx-bv 1)) s)
