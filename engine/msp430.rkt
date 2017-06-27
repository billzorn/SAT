#lang rosette

(provide msp430-implementation@ 
         msp430-state msp430-state-memory msp430-state-registers
         step perform-write 
         MAP/REG MAP/MEM)

(require "implementation-sig.rkt"
         "framework.rkt" 
         "framework-addr.rkt" 
         "msp430-synthesized.rkt" 
         "../lib/bv.rkt" 
         "../msp430/regs.rkt" 
         (rename-in "../lib/mem_simple.rkt" [make-memory make-memory/vector] [memory-ref memory-ref/vector] [memory-set! memory-set!/vector] [memory-copy memory-copy/vector])
         (rename-in "../lib/mem_ivmap.rkt"  [make-memory make-memory/intervalmap]))

(struct msp430-operand operand (as rsrc imm) #:transparent)

(define (msp430-state rn hiaddr)
  (state (vector (make-memory/vector rn (mspx-bv 0)) 
                 (make-memory/intervalmap hiaddr (mspx-bv 0)))))

(define MAP/REG 0)
(define MAP/MEM 1)

(define (msp430-state-memory s) 
  (vector-ref (state-mmaps s) MAP/MEM))

(define (msp430-state-registers s) 
  (vector-ref (state-mmaps s) MAP/REG))

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
          [1 (memory-ref mmap (bitvector->integer (bvlshr a (mspx-bv 1))))])))])
      (match* (map bw)
        [(_ 8) (compose trunc8 ref)]
        [(0 16) (compose trunc16 ref)]
        [(1 16) ref]
        [(0 20) ref]
        [(1 20) (lambda (s a)
                  (bvor (trunc16 (ref s a))
                        (bvshl (ref s (bvadd a (mspx-bv 2))) (mspx-bv 16))))])))

  (define (mmap-set! map bw) 
    (let ([set (lambda (state a val)
      (let ([mmap (vector-ref (state-mmaps state) map)])
        (match map
          [0 (vector-set! mmap a val)]
          [1 (memory-set! mmap (bitvector->integer (bvlshr a (mspx-bv 1))) val)])))])
      (match* (map bw)
        [(_ 8) (lambda (s a v) (set s a (trunc8 v)))]
        [(0 16) (lambda (s a v) (set s a (trunc16 v)))]
        [(0 20) set]
        [(1 16) set]
        [(1 20) (lambda (s a v)
                  (set s a (trunc16 v))
                  (set s (bvadd a (mspx-bv 2)) (bvlshr v (mspx-bv 16))))])))

  (define register-ref  (mmap-ref MAP/REG mspx-bits))
  (define register-set! (mmap-set! MAP/REG mspx-bits))

  ; SECTION: Synthesizable processor behavior

  (define (comp-addr oper)
    (match oper [(msp430-operand as rsrc imm)
      (case as
        [(#b00) (case rsrc
                  [(3) (constant (mspx-bv 0))]
                  [else (ref MAP/REG (constant rsrc))])]
        [(#b01) (case rsrc
                  [(2) (ref MAP/MEM (constant imm))]
                  [(3) (constant (mspx-bv 1))]
                  [else (ref MAP/MEM (add (ref MAP/REG (constant rsrc)) (constant imm)))])]
        [(#b10) (case rsrc
                  [(2) (constant (mspx-bv 4))]
                  [(3) (constant (mspx-bv 2))]
                  [else (ref MAP/MEM (ref MAP/REG (constant rsrc)))])]
        [(#b11) (case rsrc
                  [(0) (constant imm)]
                  [(2) (constant (mspx-bv 8))]
                  [(3) (constant (mspx-bv -1))]
                  [else (ref MAP/MEM (ref MAP/REG (constant rsrc)))])])]))

  (define (dispatch bw op sr op1 op2) 
    ((cond
      [((bveq-masked #xf000) op (mspx-bv #x4000)) msp-mov]
      [((bveq-masked #xf000) op (mspx-bv #x5000)) msp-add]
      [((bveq-masked #xf000) op (mspx-bv #x6000)) msp-addc]
      [((bveq-masked #xf000) op (mspx-bv #x7000)) msp-subc]
      [((bveq-masked #xf000) op (mspx-bv #x8000)) msp-sub]
      [((bveq-masked #xf000) op (mspx-bv #x9000)) msp-cmp]
      [((bveq-masked #xf000) op (mspx-bv #xa000)) msp-dadd]
      [((bveq-masked #xf000) op (mspx-bv #xb000)) msp-bit]
      [((bveq-masked #xf000) op (mspx-bv #xc000)) msp-bic]
      [((bveq-masked #xf000) op (mspx-bv #xd000)) msp-bis]
      [((bveq-masked #xf000) op (mspx-bv #xe000)) msp-xor]
      [((bveq-masked #xf000) op (mspx-bv #xf000)) msp-and])
     bw sr op1 op2))

  (define (dispatch-sr bw op sr op1 op2 dst) 
    ((cond
      [((bveq-masked #xf000) op (mspx-bv #x4000)) msp-sr-mov]
      [((bveq-masked #xf000) op (mspx-bv #x5000)) msp-sr-add]
      [((bveq-masked #xf000) op (mspx-bv #x6000)) msp-sr-addc]
      [((bveq-masked #xf000) op (mspx-bv #x7000)) msp-sr-subc]
      [((bveq-masked #xf000) op (mspx-bv #x8000)) msp-sr-sub]
      [((bveq-masked #xf000) op (mspx-bv #x9000)) msp-sr-cmp]
      [((bveq-masked #xf000) op (mspx-bv #xa000)) msp-sr-dadd]
      [((bveq-masked #xf000) op (mspx-bv #xb000)) msp-sr-bit]
      [((bveq-masked #xf000) op (mspx-bv #xc000)) msp-sr-bic]
      [((bveq-masked #xf000) op (mspx-bv #xd000)) msp-sr-bis]
      [((bveq-masked #xf000) op (mspx-bv #xe000)) msp-sr-xor]
      [((bveq-masked #xf000) op (mspx-bv #xf000)) msp-sr-and]) 
     bw sr op1 op2 dst))

  ; SECTION: Easily implemented (specified) processor behavior

  (define (decode stream) 
    (let ([peek (stream-first stream)])
      (if (or (bvuge peek (mspx-bv #x4000))) 
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
        [(1) (case rsrc
               [(3) (void)]
               [else (set! imm0 (stream-first stream)) (set! stream (stream-rest stream))])]
        [(3) (case rsrc
               ; imm mode
               [(0) (set! imm0 (stream-first stream)) (set! stream (stream-rest stream))])])
      (case ad
        [(1) (set! imm1 (stream-first stream)) (set! stream (stream-rest stream))])
      (decoded opcode bw (msp430-operand as rsrc imm0) (msp430-operand ad rdst imm1))))

  (define (decode-taken d)
    (match d 
      [(decoded opcode bw (msp430-operand as rsrc imm0) (msp430-operand ad rdst imm1))
        (+ (if (or (and (= as 1) (not (= rsrc 3))) (and (= as 3) (= rsrc 0))) 1 0)
          (if (= ad 1) 1 0)
          1)]
      [void 0]))

  (define (decode-fmt2 stream) (void))

  ; SECTION: Processor step pipeline model

  (define (step/read state dec pc-incr)
    (match dec [(decoded op bw op1 op2)
      (let ([pc (bvadd (register-ref state REG/PC) (impl-bv pc-incr))]
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
                   (bvadd (mspx-read state (ref MAP/REG (constant rsrc))) (mspx-bv (/ bw 8))))
                 (void))
               (write-op state bw op2 dst))]))
  )

(define-compound-unit/infer msp430-engine@
  (import) (export implementation^ framework^ framework-addr^)
  (link msp430-implementation@ framework@ framework-addr@))
(define-values/invoke-unit/infer msp430-engine@)
