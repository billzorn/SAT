#lang rosette

(require "implementation-sig.rkt"
         "framework.rkt" 
         "../lib/bv.rkt" 
         "../msp430/regs.rkt" 
         (rename-in "../lib/mem_simple.rkt" [make-memory make-memory/vector] [memory-ref memory-ref/vector] [memory-set! memory-set!/vector] [memory-copy memory-copy/vector])
         (rename-in "../lib/mem_ivmap.rkt"  [make-memory make-memory/intervalmap]))

(define (msp430-state rn hiaddr)
  (state (vector (make-memory/vector rn (mspx-bv 0)) 
                 (make-memory/intervalmap hiaddr (mspx-bv 0)))))

(struct msp430-op op () #:transparent)
(struct mov msp430-op () #:transparent)
(struct msp430-operand operand (as rsrc imm) #:transparent)
(struct msp430-decoded decoded (op1 op2) #:transparent)

(define MAP/REG 0)
(define MAP/MEM 1)

(define TODO (void))

; (define (imm a) (constant a))
; (define (reg r) (ref MAP/REG (constant r)))
; (define (ind r) (ref MAP/MEM (reg r)))
; (define (abs a) (ref MAP/MEM (constant a)))
; (define (idx r i) (ref MAP/MEM (add (constant i) (reg r))))

(define-unit msp430-implementation@
  (import)
  (export implementation^)

  ; SECTION: Utility functions needed by the framework

  (define (impl-bv i) (mspx-bv i))

  (define (mmap-ref state map a)
    (let ([mmap (vector-ref (state-mmaps state) map)])
      (match map
        [0 (vector-ref mmap a)]
        [1 (memory-ref mmap (/ a 2))])))

  (define (mmap-set! state map a val)
    (let ([mmap (vector-ref (state-mmaps state) map)])
      (match map
        [0 (vector-set! mmap a val)]
        [1 (memory-set! mmap (/ a 2) val)])))

  ; SECTION: Synthesizable processor behavior

  (define (comp-addr oper)
    (match oper [(msp430-operand as rsrc imm)
      (case as
        [(#b00) (ref MAP/REG (constant rsrc))]
        [(#b01) (ref MAP/MEM (add (ref MAP/REG (constant rsrc)) (constant imm)))]
        [(#b10) (ref MAP/MEM (ref MAP/REG (constant rsrc)))]
        [(#b11) (ref MAP/MEM (ref MAP/REG (constant rsrc)))])]))

  ; SECTION: Easily implemented (specified) processor behavior

  (define (operator opcode)
    (if (bveq (bvand opcode (mspx-bv #xf000)) (mspx-bv #x4000)) (mov) 
        TODO))

  (define (dispatch op bw sr op1 op2) 
    (match op
      [(mov) op1]
    ))
  (define (dispatch/flags op bw sr op1 op2 dst) 
    (match op
      [(mov) sr]
    ))

  (define (decode stream) 
    (let ([peek (stream-first stream)])
      (if (bvuge peek (mspx-bv #x4000)) 
        (decode-2arg stream)
        (decode-1arg stream))))

  (define (decode-2arg stream) 
    (let ([word (stream-first stream)]
          [stream (stream-rest stream)])
      (define opcode  (bvand word (mspx-bv #xf000)))
      (define rsrc    (bitvector->natural (extract 11 8 word)))
      (define rdst    (bitvector->natural (extract 3 0 word)))
      (define as      (bitvector->natural (extract 5 4 word)))
      (define ad      (bitvector->natural (extract 7 7 word)))
      (define bw      (bitvector->natural (extract 6 6 word)))
      (define imm0    null)
      (define imm1    null)
      (case as
        [(1) (set! imm0 (stream-first stream)) (set! stream (stream-rest stream))]
        [(3) (if (= rsrc REG/PC) ; imm mode
               (begin (set! imm0 (stream-first stream)) (set! stream (stream-rest stream))) 
               (void))])
      (case ad
        [(1) (set! imm1 (stream-first stream)) (set! stream (stream-rest stream))])
      (msp430-decoded (operator opcode) bw (msp430-operand as rsrc imm0) (msp430-operand ad rdst imm1))))

  (define (decode-1arg stream) TODO)

  ; SECTION: Processor step pipeline model

  (define (step/read state dec pc-incr)
    (match dec [(msp430-decoded op bw op1 op2)
      (let ([pc (bvadd (mmap-ref state MAP/REG REG/PC) pc-incr)]
            [sr (mmap-ref state MAP/REG REG/SR)]
            [op1-val (read-op state bw op1)]
            [op2-val (read-op state bw op2)])
        (stepctx pc sr op1-val op2-val null))]))

  (define (step/exec op bw ctx)
    (match ctx [(stepctx _ sr op1 op2 _)
      (let* ([dst (dispatch op bw sr op1 op2)]
             [sr  (dispatch/flags op bw sr op1 op2 dst)])
        (set-stepctx-dst! ctx dst)
        (set-stepctx-sr! ctx sr))]))

  (define (step/write state dec ctx)
    (match (cons dec ctx) 
      [(cons (msp430-decoded _ bw (msp430-operand as rsrc _) op2) (stepctx pc sr _ _ dst)) 
        (begin (perform-write (ref MAP/REG (constant REG/PC)) state pc)
               (perform-write (ref MAP/REG (constant REG/SR)) state sr)
               ;(perform-write (ref MAP/REG (constant rsrc)) state stuff)
               (write-op state bw op2 dst))]))
  )

(define-compound-unit/infer msp430-engine@
  (import) (export implementation^ framework^)
  (link msp430-implementation@ framework@))
(define-values/invoke-unit/infer msp430-engine@)

; Test code

(define instr-stream (stream (mspx-bv #x4102))) ; mov r1, r2
(define s (msp430-state 4 4))
(perform-write (ref MAP/REG (constant 1)) s (mspx-bv 17))
(displayln s)
(step s instr-stream)
(displayln s)
