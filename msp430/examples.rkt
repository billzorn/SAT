#lang rosette/safe

(require rosette/lib/synthax "lang-simple.rkt")


(printf "simple case - let's synthesize a constant\n\n")

; define a symbolic bitvector for a register (this is what we'll synthesize)
(define-symbolic ex1-r0 mspx-bv?)
; create registers, memory, a "running" flag, and a state
(define ex1-regs (vector ex1-r0 (mspx-bv 0) (mspx-bv 0) (mspx-bv 0)))
(define ex1-mem (vector (mspx-bv 0) (mspx-bv 0) (mspx-bv 0) (mspx-bv 0)))
(define ex1-running (box #t))

(define ex1-state (state (halt (list (add.w (reg 0) (reg 0))
                                     (add.w (reg 0) (reg 0))
                                     (add.w (reg 0) (reg 0))))
                         ex1-regs
                         ex1-mem
                         ex1-running))

; run the state for a concrete number of steps
(stepn ex1-state 4)
(printf "running should be false (a concrete value): ~a\n\n" (unbox ex1-running))

(printf "our program adds r0 to itself 3 times. this is the same as multiplying by 8.\n")
(printf "what number, multiplied by 8, gives 48?\n")
(define ex1-sol1 (solve (assert (bveq (register-ref ex1-regs 0) (mspx-bv 48)))))
(evaluate ex1-r0 ex1-sol1)

(printf "what if we want the result in r0 to be 42 instead?\n")
(define ex1-sol2 (solve (assert (bveq (register-ref ex1-regs 0) (mspx-bv 42)))))
ex1-sol2


(printf "\n\nmore complicated - synthesize part of a program\n\n")

; make r1 symbolic, so that we can synthesize a program that works for all possible values
(define-symbolic ex2-r1 mspx-bv?)
; we use choose to define symbolic structs, such as instructions
(define ex2-br (choose (jz) (jnz) (jmp)))

; this program is more complex. first we define two basic blocks, one that adds r0 to itself
; and one that subtracts 1 from r0 (twice)
(define ex2-state2add (halt (list (add.w (reg 0) (reg 0)))))
(define ex2-state2sub (halt (list (sub.w (imm (mspx-bv 1)) (reg 0))
                                  (sub.w (imm (mspx-bv 1)) (reg 0)))))
; this block adds r0 to itself twice, then branches to one of the other blocks using the
; symbolic branch condition we defined above
(define ex2-state1
   (jump (list (add.w (reg 0) (reg 0))
               (add.w (reg 0) (reg 0)))
         ex2-br
         ex2-state2add
         ex2-state2sub))

(define ex2-regs (vector (mspx-bv 1) ex2-r1 (mspx-bv 0) (mspx-bv 0)))
(define ex2-mem (vector (mspx-bv 0) (mspx-bv 0) (mspx-bv 0) (mspx-bv 0)))
(define ex2-running (box #t))

(define ex2-state (state ex2-state1 ex2-regs ex2-mem ex2-running))
(stepn ex2-state 5)
(printf "running is symbolic - we might not have actually halted:\n  ~a\n" (unbox ex2-running))
(printf "this isn't a problem, as we've done all of the computation, we just haven't set the flag\n")
(printf "there isn't a way to resume execution yet - it's on the todo list\n\n")

(printf "we start with a value of 1 in r0.\n")
(printf "our guarantee is that after we run to completion, r0 holds 8 if bit 1 of r1 is set,\n")
(printf "and 2 otherwise. what should we choose for our branch condition?\n")
(define ex2-sol1
 (synthesize #:forall (list ex2-r1)
             #:guarantee (assert (if (bveq (mspx-bv 2) (bvand (mspx-bv 2) ex2-r1))
                                     (bveq (mspx-bv 8) (register-ref ex2-regs 0))
                                     (bveq (mspx-bv 2) (register-ref ex2-regs 0))))))
(evaluate ex2-br ex2-sol1)
(printf "this makes sense. r1 is the status register, and bit 1 is the zero flag\n\n")

(printf "we can also try to get a program that always halts in 5 steps:\n")
(define ex2-sol2
 (synthesize #:forall (list ex2-r1)
             #:guarantee (assert (not (unbox ex2-running)))))
(evaluate ex2-br ex2-sol2)

(printf "\nthere is no program that never halts in 5 steps: there's only one kind of unconditional jmp\n")
(define ex2-sol3
 (synthesize #:forall (list ex2-r1)
             #:guarantee (assert (unbox ex2-running))))
ex2-sol3


(printf "\n\nwe can use the solver to write tests - let's test the add instruction\n\n")
(define-symbolic ex3-i1 ex3-i2 integer?) ; register numbers
(define-symbolic ex3-x1 ex3-x2 mspx-bv?) ; 20-bit address immediates
(define-symbolic ex3-v1 ex3-v2 word?)    ; 16-bit values

; symbolic operands in terms of our symbolic values
(define ex3-op1 (choose (reg ex3-i1) (abs ex3-x1) (idx ex3-i1 ex3-x1)))
(define ex3-op2 (choose (reg ex3-i2) (abs ex3-x2) (idx ex3-i2 ex3-x2)))

; we make the initial state concrete...
(define ex3-regs (vector (mspx-bv 0) (mspx-bv 0) (mspx-bv 0) (mspx-bv 0)))
(define ex3-mem (vector (mspx-bv 0) (mspx-bv 0) (mspx-bv 0) (mspx-bv 0)))
(define ex3-running (box #t))
(define ex3-state (state (halt (list (add.w ex3-op1 ex3-op2))) ex3-regs ex3-mem ex3-running))

; ...but store to it using our symbolic operands, before we run it
(store16 (word->mspx ex3-v1) ex3-op1 ex3-regs ex3-mem)
(store16 (word->mspx ex3-v2) ex3-op2 ex3-regs ex3-mem)
(stepn ex3-state 2)
(printf "running should be false (a concrete value): ~a\n\n" (unbox ex3-running))

(printf "loading the destination (and taking the low 16 bits) should give us the same thing as adding v1 and v2 directly, right?\n")
(define ex3-sol 
  (verify (assert (bveq (bvadd ex3-v1 ex3-v2) (mspx->word (load16 ex3-op2 ex3-regs ex3-mem))))))
ex3-sol

(printf "\nwhat's going on here? we can evaluate the symbolic operands to get a better illustration:\n")
(printf "  v1: ~a\n" (evaluate ex3-v1 ex3-sol))
(printf "  op1: ~a\n" (evaluate ex3-op1 ex3-sol))
(printf "  v1: ~a\n" (evaluate ex3-v2 ex3-sol))
(printf "  op2: ~a\n\n" (evaluate ex3-op2 ex3-sol))

(printf "we can also plugging in the values to see what's happening\n\n")

(define test-r (vector (bv 0 20) (bv 0 20) (bv 0 20) (bv 0 20)))
(define test-m (vector (bv 0 20) (bv 0 20) (bv 0 20) (bv 0 20)))
(define test-running (box #t))
(define test-state (state (halt (list (add.w (evaluate ex3-op1 ex3-sol)
                                             (evaluate ex3-op2 ex3-sol))))
                          test-r test-m test-running))

(printf "initial, empty state\n")
test-state

(store16 (word->mspx (evaluate ex3-v1 ex3-sol)) (evaluate ex3-op1 ex3-sol) test-r test-m)
(printf "\nafter first store\n")
test-state

(store16 (word->mspx (evaluate ex3-v2 ex3-sol)) (evaluate ex3-op2 ex3-sol) test-r test-m)
(printf "\nafter second store \n")
test-state

(stepn test-state 2)
(printf "\nafter running the program\n")
test-state

(printf "\nactual value from loading op2: ~a\n"
        (mspx->word (load16 (evaluate ex3-op2 ex3-sol) test-r test-m)))
(printf "expected value from adding v1 and v2: ~a\n\n"
        (bvadd (evaluate ex3-v1 ex3-sol) (evaluate ex3-v2 ex3-sol)))

(printf "what's happening (assuming rosette is finding the same behavior i saw) is that the operands alias.\n")
(printf "the second store overwrites the first, and we get v2 + v2 instead of v1 + v2. oops.\n")
(printf "we can avoid this by just making the whole state symbolic, and getting v1 and v2 by loading before we run.\n")
(printf "let's use some test macros so it isn't such a pain to write all the definitions.\n\n")

(require "test-simple.rkt")
(set-box! test-verbosity 2)

(check-unsat?
 (define-test/ops r 4 m 4 op1 op2
   (begin
     (define-symbolic v1/immx mspx-bv?)
     (define op1/imm (choose op1 (imm v1/immx)))
     (define running (box #t))
     (define test-state (state
                         (halt (list (add.w op1/imm op2)))
                         r m running))
     (define v1 (mspx->word (load16 op1/imm r m)))
     (define v2 (mspx->word (load16 op2 r m)))
     (stepn test-state 2))
   (bveq (bvadd v1 v2) (mspx->word (load16 op2 r m)))))

; what about backward edges

(define loop-regs (vector (mspx-bv 3) (mspx-bv 0) (mspx-bv 0) (mspx-bv 0)))
(define loop-mem (vector (mspx-bv 0) (mspx-bv 0) (mspx-bv 0) (mspx-bv 0)))
(define loop-running (box #t))
(letrec ([loop-body (state (jump (list
                                (sub.w (imm (mspx-bv 3)) (reg 0))
                                (cmp.w (imm (mspx-bv 0)) (reg 0)))
                               (jnz)
                               loop-tmp
                               loop-end)
                         loop-regs loop-mem loop-running)]
         [loop-end (state (halt (list (mov.w (imm (mspx-bv 1)) (reg 1))))
                        loop-regs loop-mem loop-running)]
         [loop-tmp (state (jump (list) (jmp) loop-body loop-body))]
         )
  
  (stepn loop-body 1))
loop-regs
loop-mem
loop-running
