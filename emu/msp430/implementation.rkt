#lang rosette

(provide msp430-implementation@ 
         msp430-state msp430-state-memory msp430-state-registers
         step perform-write 
         MAP/REG MAP/MEM)

(require "../implementation-sig.rkt"
         "../framework.rkt" 
         "../framework-addr.rkt" 
         "synthesized.rkt" 
         "../../lib/bv.rkt" 
         "../../mmcu/msp430/regs.rkt" 
         (rename-in "../../lib/mem_simple.rkt" [make-memory make-memory/vector] [memory-ref memory-ref/vector] [memory-set! memory-set!/vector] [memory-copy memory-copy/vector])
         (rename-in "../../lib/mem_ivmap.rkt"  [make-memory make-memory/intervalmap]))

; A specialization of operand for the msp430. msp430 operands can have:
;  - an addressing mode (2 bit bitvector represented as an integer,
;  - a register to use as either the direct destination or a memory reference
;  - an immediate value to factor in depending on the addressing mode
(struct msp430-operand operand (as rsrc imm) #:transparent)

; An msp430 status consists of two memory maps:
;  - a vector of 20-bit bitvectors for the register file
;  - an interval map of bitvectors (20-bit for convenience but treated as 16-bit
;    by the mmap-ref and mmap-set functions) for the memory as seen by the cpu
(define (msp430-state rn hiaddr)
  (state (vector (make-memory/vector rn (mspx-bv 0)) 
                 (make-memory/intervalmap hiaddr (mspx-bv 0)))))

; Constants to use when referring to mmaps in a semantic way
(define MAP/REG 0)
(define MAP/MEM 1)

; Shorcuts for referring to the memory and registers in the state
(define (msp430-state-memory s) 
  (vector-ref (state-mmaps s) MAP/MEM))
(define (msp430-state-registers s) 
  (vector-ref (state-mmaps s) MAP/REG))

; The unit that actually implements the cpu-specific interface for the emulator
(define-unit msp430-implementation@
  (import)
  (export implementation^)

  ; The msp430's maximum operation width is 20 bits, so we'll use that
  ; everywhere so we don't have to convert up and down
  (define (impl-bv i) (mspx-bv i))

  ; Masks off everything but the low 8 bits of x
  (define (trunc8 x)
    (bvand x (mspx-bv #x000ff)))

  ; Masks off everything but the low 16 bits of x
  (define (trunc16 x)
    (bvand x (mspx-bv #x0ffff)))
  
  ; An mmap reference for the msp430. Our concerns here are storage addressing 
  ; (the memory in the state is treated as an array of 16-bit words, but addressing
  ; is done by byte) and proper truncation/combination if values are referenced
  ; with a bitwidth of 8 or a bitwidth of 20
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

  ; The concerns for mmap-set are similar to those of mmap-ref. We essentially
  ; do the same thing but with the appropriate "set" function called instead of
  ; ref
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

  ; Shortcuts for reading and writing to registers
  (define register-ref  (mmap-ref MAP/REG mspx-bits))
  (define register-set! (mmap-set! MAP/REG mspx-bits))


  ; Creates an expression in the addressing DSL based on the operand.
  ; This is basically just a straightforward code translation of what's
  ; expressed in the TI MSP430 user manual.
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

  ; Dispatches to the functions in msp430/synthesized.rkt.
  ; These are all format 1 for now, so we can get the opcode from the
  ; instruction easily by masking out all but the top 4 bits
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

  ; Dispatches with the status register.
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


  ; Dispatches to decode-fmt1 or decode-fmt2 (not yet implemented, returns 0) to
  ; create an operand structure based on the next 1-3 words in the instruction
  ; stream.
  (define (decode stream) 
    (let ([peek (stream-first stream)])
      (if (or (bvuge peek (mspx-bv #x4000))) 
        (decode-fmt1 stream)
        (decode-fmt2 stream))))

  ; Decoding for the msp430. More or less a straightforward code transcription
  ; of what's described in the TI MSP430 user manual
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
      ; Maybe load immediates
      (define imm0 null)
      (define imm1 null)
      ; We need imm0 for immediate mode and indirect mode
      (case as
        [(1) (case rsrc
               [(3) (void)]
               [else (set! imm0 (stream-first stream)) (set! stream (stream-rest stream))])]
        [(3) (case rsrc
               ; imm mode
               [(0) (set! imm0 (stream-first stream)) (set! stream (stream-rest stream))])])
      ; We need imm1 for destination indirect mode
      (case ad
        [(1) (set! imm1 (stream-first stream)) (set! stream (stream-rest stream))])
      (decoded opcode bw (msp430-operand as rsrc imm0) (msp430-operand ad rdst imm1))))

  ; Replicate the logic above for taking immediates to figure out how many words
  ; we consumed from the stream
  (define (decode-taken d)
    (match d 
      [(decoded opcode bw (msp430-operand as rsrc imm0) (msp430-operand ad rdst imm1))
        (+ (if (or (and (= as 1) (not (= rsrc 3))) (and (= as 3) (= rsrc 0))) 1 0)
          (if (= ad 1) 1 0)
          1)]
      [void 0]))

  ; fmt 2 not yet implemented
  (define (decode-fmt2 stream) (void))


  ; step/read for the msp430 is a pretty straightforward filling out of the
  ; stepctx struct. The only interesting thing to note is that we have to set
  ; the program counter in that struct to the value it would take after
  ; decoding, so that its pointing to the next instruction in the stream when we
  ; eventually write it back to the state. However, during execution, the old
  ; value is still in the memory map so that if it's referenced by the
  ; instruction, it still points to the instruction itself and not the next one.
  (define (step/read state dec pc-incr)
    (match dec [(decoded op bw op1 op2)
      (let ([pc (bvadd (register-ref state REG/PC) (impl-bv pc-incr))]
            [sr (register-ref state REG/SR)]
            [op1-val (read-op state bw op1)]
            [op2-val (read-op state bw op2)])
        (stepctx pc sr op1-val op2-val null))]))

  ; step/write is a little more complicated. here we actually have to do the
  ; work of checking if the addressing mode is indirect autoincrement, because
  ; then we'll have to actually do the increment (at the appropriate bitwidth)
  ; of the register. Other than that it's essentially the inverse of step/read
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

; Link the implementation with the framework and export the resulting code
; from this module
(define-compound-unit/infer msp430-engine@
  (import) (export implementation^ framework^ framework-addr^)
  (link msp430-implementation@ framework@ framework-addr@))
(define-values/invoke-unit/infer msp430-engine@)
