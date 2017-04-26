#lang rosette

(require "framework.rkt"
         "framework-addr.rkt"
         "msp430.rkt"
         "../lib/bv.rkt"
         "../msp430/regs.rkt"
         "../lib/mem_ivmap.rkt")

(define (ignore x) (void))

(define s (msp430-state 5 #x1000))

(define (copy s) (state (vector (vector-copy (vector-ref (state-mmaps s) MAP/REG)) 
                                (memory-copy (vector-ref (state-mmaps s) MAP/MEM)))))

(define (make-fmt2 opc bw as ad rsrc rdst) 
  (concat (bv 0 4) (bv opc 4) (bv rsrc 4) (bv ad 1) (bv bw 1) (bv as 2) (bv rdst 4)))

(define-syntax-rule (example desc prep step)
  (let ([s-orig s])
    (set! s (copy s))
    (displayln desc) 
    (ignore prep) 
    (ignore step) 
    (printf "~a\n\n" s)
    (set! s s-orig)))

; Source addressing modes:

; Register
(example
  "register -> register\n  mov r1, r4 (0x1000)"
  ((perform-write 16) s (ref MAP/REG (constant 1)) (mspx-bv #x1000))
  (step s (stream (make-fmt2 4 0 #b00 #b0 1 4))))

;  cg 0
(example
  "cg #0 -> register\n  mov #0, r4"
  (void)
  (step s (stream (make-fmt2 4 0 #b00 #b0 3 4))))

;  cg 1
(example 
  "cg #1 -> register\n  mov #1, r4"
  (void)
  (step s (stream (make-fmt2 4 0 #b01 #b0 3 4))))

;  cg 2
(example 
  "cg #2 -> register\n  mov #2, r4"
  (void)
  (step s (stream (make-fmt2 4 0 #b10 #b0 3 4))))

;  cg 4
(example 
  "cg #4 -> register\n  mov #4, r4"
  (void)
  (step s (stream (make-fmt2 4 0 #b10 #b0 2 4))))

;  cg 8
(example 
  "cg #8 -> register\n  mov #8, r4"
  (void)
  (step s (stream (make-fmt2 4 0 #b11 #b0 2 4))))

;  cg -1 
(example 
  "cg #-1 -> register\n  mov #-1, r4"
  (void)
  (step s (stream (make-fmt2 4 0 #b11 #b0 3 4))))

; Indexed
(example 
  "indexed -> register\n  mov 2(r4), r1"
  (begin ((perform-write 16) s (ref MAP/MEM (constant (mspx-bv 2))) (mspx-bv #x2000))
         ((perform-write 16) s (ref MAP/REG (constant 1)) (mspx-bv #x0000)))
  (step s (stream (make-fmt2 4 0 #b01 #b0 4 1) (mspx-bv 2))))

; Symbolic
(example 
  "symbolic -> register\n  mov 2, r4"
  (begin ((perform-write 16) s (ref MAP/MEM (constant (mspx-bv 2))) (mspx-bv #x2001))
         ((perform-write 16) s (ref MAP/REG (constant 0)) (mspx-bv #x0000)))
  (step s (stream (make-fmt2 4 0 #b01 #b0 0 4) (mspx-bv 2))))

; Absolute
(example 
  "absolute -> register\n  mov &2, r4"
  ((perform-write 16) s (ref MAP/MEM (constant (mspx-bv 2))) (mspx-bv #x2002))
  (step s (stream (make-fmt2 4 0 #b01 #b0 1 4) (mspx-bv 2))))

; Indirect
(example 
  "indirect -> register\n  mov @r1, r4"
  (begin ((perform-write 16) s (ref MAP/MEM (constant (mspx-bv 0))) (mspx-bv #x2003))
         ((perform-write 16) s (ref MAP/REG (constant 1)) (mspx-bv #x0000)))
  (step s (stream (make-fmt2 4 0 #b10 #b0 1 4) (mspx-bv 2))))

; Indirect Autoincrement
(example 
  "autoincrement -> register\n  mov @r1+, r4"
  (begin ((perform-write 16) s (ref MAP/MEM (constant (mspx-bv 0))) (mspx-bv #x2004))
         ((perform-write 16) s (ref MAP/REG (constant 1)) (mspx-bv #x0000)))
  (step s (stream (make-fmt2 4 0 #b11 #b0 1 4) (mspx-bv 2))))

; Immediate
(example 
  "immediate -> register\n  mov #2005, r4"
  (void)
  (step s (stream (make-fmt2 4 0 #b11 #b0 0 4) (mspx-bv #x2005))))


; Dest addressing modes:
; Register
(example 
  "immediate -> register\n  mov #3000, r4"
  (void)
  (step s (stream (make-fmt2 4 0 #b11 #b0 0 4) (mspx-bv #x3000))))

;  cg dropped
(example 
  "immediate -> cg (dropped)\n  mov #3001, r3"
  (void)
  (step s (stream (make-fmt2 4 0 #b11 #b0 0 3) (mspx-bv #x3001))))

; Indexed
(example 
  "immediate -> indexed\n  mov #3002, 2(r4)"
  ((perform-write 16) s (ref MAP/REG (constant 4)) (mspx-bv #x0002))
  (step s (stream (make-fmt2 4 0 #b11 #b1 0 4) (mspx-bv #x3002) (mspx-bv 1))))

; Symbolic
(example 
  "immediate -> symbolic\n  mov #3003, 2"
  (void)
  (step s (stream (make-fmt2 4 0 #b11 #b1 0 0) (mspx-bv #x3003) (mspx-bv 2))))

; Absolute
(example 
  "immediate -> absolute\n  mov #3004, &2"
  (void)
  (step s (stream (make-fmt2 4 0 #b11 #b1 0 1) (mspx-bv #x3004) (mspx-bv 2))))















; (define instr-stream (stream (mspx-bv #x4441)                    ; mov.b r4, r1
;                              (mspx-bv #x4034) (mspx-bv #x1111)   ; mov 0x1111, r4
;                              (mspx-bv #xa034) (mspx-bv #x1234))) ; dadd 0x1234, r4
; 
; (printf "The msp430-state constructor creates a new state for us with two memory maps: a register file and a memory space:\n ~a\n\n" s)
; 
; (println "Let's initialize the memory map with a constant in one of the registers. We perform a 16-bit write of 0x10111 to r4, which should leave 0x111 in the register:\n")
; ((perform-write 16) s (ref MAP/REG (constant 4)) (mspx-bv #x10111))
; (printf "  R: ~a\n\n" (vector-ref (state-mmaps s) MAP/REG))
; 
; (println "From here on out we'll operate within the framework using actual instructions as they'd be encoded by an assembler. First, let's make sure truncation works within step using only register operands:")
; (println "  41 44 # mov.b r4, r1")
; (set! instr-stream (step s instr-stream))
; (printf "  R: ~a\n\n" (vector-ref (state-mmaps s) MAP/REG))
; 
; (println "Now we'll do a mov instruction with an immediate (0x1111 -- 4369).")
; (println "  34 40 11 11 # mov.w 0x1111, r4")
; (set! instr-stream (step s instr-stream))
; (printf "  R: ~a\n\n" (vector-ref (state-mmaps s) MAP/REG))
; 
; (println "Finally, execute our (placeholder) dadd with an immediate operand. We should see a digit-wise add of 0x1111 and 0x1234, yielding 0x2345 (9029).")
; (println "  34 a0 34 12 # dadd.w 0x1234, r4")
; (set! instr-stream (step s instr-stream))
; (printf "  R: ~a\n\n" (vector-ref (state-mmaps s) MAP/REG))
