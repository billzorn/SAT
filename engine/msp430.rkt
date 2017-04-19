#lang rosette

(require "implementation-sig.rkt"
         "framework.rkt" 
         "../lib/bv.rkt" 
         "../msp430/regs.rkt" 
         (rename-in "../lib/mem_simple.rkt" [make-memory make-memory/vector] [memory-ref memory-ref/vector] [memory-set! memory-set!/vector] [memory-copy memory-copy/vector])
         (rename-in "../lib/mem_ivmap.rkt"  [make-memory make-memory/intervalmap]))

(struct msp430-operand operand (as rsrc imm) #:transparent)

(define (msp430-state rn hiaddr)
  (state (vector (make-memory/vector rn (mspx-bv 0)) 
                 (make-memory/intervalmap hiaddr (mspx-bv 0)))))

(define-ops [nop mov dadd])

(define MAP/REG 0)
(define MAP/MEM 1)

(define-unit msp430-implementation@
  (import)
  (export implementation^)

  ; SECTION: Utility functions needed by the framework

  (define (impl-bv i) (mspx-bv i))

  (define (trunc8 x)
    (bvand x (mspx-bv #x000ff)))

  (define (trunc16 x)
    (bvand x (mspx-bv #x0ffff)))
  
  (define (mmap-ref map bw)
    (let ([ref (lambda (state a)
      (let ([mmap (vector-ref (state-mmaps state) map)])
        (match map
          [0 (vector-ref mmap a)]
          [1 (memory-ref mmap (/ a 2))])))])
      (match* (map bw)
        [(_ 8) (compose trunc8 ref)]
        [(0 16) (compose trunc16 ref)]
        [(1 16) ref]
        [(0 20) ref]
        [(1 20) (lambda (s a)
                  (bvor (trunc16 (ref s a))
                        (bvshl (ref s (+ a 2)) (mspx-bv 16))))])))

  (define (mmap-set! map bw) 
    (let ([set (lambda (state a val)
      (let ([mmap (vector-ref (state-mmaps state) map)])
        (match map
          [0 (vector-set! mmap a val)]
          [1 (memory-set! mmap (/ a 2) val)])))])
      (match* (map bw)
        [(_ 8) (lambda (s a v) (set s a (trunc8 v)))]
        [(0 16) (lambda (s a v) (set s a (trunc16 v)))]
        [(0 20) set]
        [(1 16) set]
        [(1 20) (lambda (s a v)
                  (set s a (trunc16 v))
                  (set s (+ a 2) (bvlshr v (mspx-bv 16))))])))

  (define register-ref  (mmap-ref MAP/REG mspx-bits))
  (define register-set! (mmap-set! MAP/REG mspx-bits))

  ; SECTION: Synthesizable processor behavior

  (define (comp-addr oper)
    (match oper [(msp430-operand as rsrc imm)
      (case as
        [(#b00) (ref MAP/REG (constant rsrc))]
        [(#b01) (case rsrc
                  [(2) (ref MAP/MEM (constant imm))]
                  [else (ref MAP/MEM (add (ref MAP/REG (constant rsrc)) (constant imm)))])]
        [(#b10) (ref MAP/MEM (ref MAP/REG (constant rsrc)))]
        [(#b11) (case rsrc
                  [(0) (constant imm)]
                  [else (ref MAP/MEM (ref MAP/REG (constant rsrc)))])])]))

  (define (operator opcode)
    (cond
      [((bveq-masked #xf000) opcode (mspx-bv #x4000)) (mov)]
      [((bveq-masked #xf000) opcode (mspx-bv #xa000)) (dadd)]
      [else (nop)]))

  (define (dispatch op sr op1 op2) 
    (match op
      [(mov) op1]
      [(dadd) (bvadd op1 op2)] ; placeholder
    ))
  (define (dispatch-sr op sr op1 op2 dst) 
    (match op
      [(mov) sr]
      [(dadd) (mspx-bv #xff)] ; placeholder
    ))

  ; SECTION: Easily implemented (specified) processor behavior

  (define (decode stream) 
    (let ([peek (stream-first stream)])
      (if (or (bvuge peek (mspx-bv #x4000)) (bvule peek (mspx-bv #x0fff))) 
        (decode-fmt1 stream)
        (decode-fmt2 stream))))

  (define (decode-fmt1 stream) 
    (let ([word (stream-first stream)]
          [stream (stream-rest stream)])
      (define opcode  (mask word #xf000))
      (define rsrc    (bitvector->natural (extract 11 8 word)))
      (define rdst    (bitvector->natural (extract 3 0 word)))
      (define as      (bitvector->natural (extract 5 4 word)))
      (define ad      (bitvector->natural (extract 7 7 word)))
      (define bw (case (bitvector->natural (extract 6 6 word))
        [(0) 16]
        [(1) 8]))
      (define imm0 null)
      (define imm1 null)
      (case as
        [(1) (set! imm0 (stream-first stream)) (set! stream (stream-rest stream))]
        [(3) (if (= rsrc REG/PC) ; imm mode
               (begin (set! imm0 (stream-first stream)) (set! stream (stream-rest stream))) 
               (void))])
      (case ad
        [(1) (set! imm1 (stream-first stream)) (set! stream (stream-rest stream))])
      (cons stream (decoded (operator opcode) bw (msp430-operand as rsrc imm0) (msp430-operand ad rdst imm1)))))

  (define (decode-fmt2 stream) (void))

  ; SECTION: Processor step pipeline model

  (define (step/read state dec pc-incr)
    (match dec [(decoded op bw op1 op2)
      (let ([pc (bvadd (register-ref state REG/PC) pc-incr)]
            [sr (register-ref state REG/SR)]
            [op1-val (read-op state bw op1)]
            [op2-val (read-op state bw op2)])
        (stepctx pc sr op1-val op2-val null))]))

  (define (step/write state dec ctx)
    (define mspx-read  (perform-read mspx-bits))
    (define mspx-write (perform-write mspx-bits))
    (match (cons dec ctx) 
      [(cons (decoded _ bw (msp430-operand as rsrc _) op2) (stepctx pc sr _ _ dst)) 
        (begin (mspx-write state (ref MAP/REG (constant REG/PC)) pc)
               (mspx-write state (ref MAP/REG (constant REG/SR)) sr)
               (if (and (= as #b11) (not (= rsrc REG/PC)))
                 (mspx-write state (ref MAP/REG (constant rsrc))
                   (bvadd (mspx-read state (ref MAP/REG (constant rsrc))) (mspx-bv bw)))
                 (void))
               (write-op state bw op2 dst))]))
  )

; Link framework and implementation
(define-compound-unit/infer msp430-engine@
  (import) (export implementation^ framework^)
  (link msp430-implementation@ framework@))
(define-values/invoke-unit/infer msp430-engine@)


; basic test code
(define instr-stream (stream (mspx-bv #x4441)                    ; mov.b r4, r1
                             (mspx-bv #x4034) (mspx-bv #x1111)   ; mov 0x1111, r4
                             (mspx-bv #xa034) (mspx-bv #x1234))) ; dadd 0x1234, r4
(define s (msp430-state 5 4))
((perform-write 16) s (ref MAP/REG (constant 4)) (mspx-bv #x10111))
(displayln s)
(set! instr-stream (step s instr-stream))
(displayln s)
(set! instr-stream (step s instr-stream))
(displayln s)
(set! instr-stream (step s instr-stream))
(displayln s)
