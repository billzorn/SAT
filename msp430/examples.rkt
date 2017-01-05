#lang rosette

(require rosette/lib/match rosette/lib/synthax "lang-simple.rkt")

(println "simple case; find the number that when multiplied by 8 by repeated self-addition gives 48")

(define-symbolic r0 (bitvector 20))
(define state1 (state (halt (list (add.w (reg 0) (reg 0))
                                  (add.w (reg 0) (reg 0))
                                  (add.w (reg 0) (reg 0))))
                      (vector r0 (bv 0 20) (bv 0 20) (bv 0 20))
                      (vector (bv 0 20) (bv 0 20) (bv 0 20) (bv 0 20))
                      (box #t)))

(stepn state1 4)

(define-values (r1 m1 running1)
  (match state1
    [(state _ r m running) (values r m running)]))

(define sol1 (solve
              (assert (bveq (register-ref r1 0) (bv 48 20)))
              (assert (not (unbox running1)))))

(evaluate r0 sol1)

(println "more complicated; synthesize a program that uses jz to either multiply by 8 or multiply by 4 and subtract 1, depending on the value in register 1")

(define-symbolic reg2 (bitvector 20))
(define br2 (choose (jz) (jnz) (jmp)))
;(define reg2 (bv 0 20))

(define state2b3 (halt (list (add.w (reg 0) (reg 0)))))
(define state2b2 (halt (list (sub.w (imm (bv 1 20)) (reg 0)))))
(define state2b1
   (jump (list (add.w (reg 0) (reg 0))
               (add.w (reg 0) (reg 0)))
         br2
         ;(jmp)
         state2b3
         state2b2))

(define r2 (vector (bv 1 20) reg2 (bv 0 20) (bv 0 20)))
(define m2 (vector (bv 0 20) (bv 0 20) (bv 0 20) (bv 0 20)))
(define running2 (box #t))

(define state2 (state state2b1 r2 m2 running2))
(stepn state2 5)

;(define state2persistent (box state2))
;(run state2persistent 1)
;state2
;(run state2persistent 1)
;state2
;(run state2persistent 1)
;state2
;(run state2persistent 1)
;state2
;(run state2persistent 1)
;state2

(define sol2
 (synthesize #:forall (list reg2)
             #:guarantee (assert (if (bveq (bv 2 20) (bvand (bv 2 20) reg2)) (bveq (bv 8 20) (register-ref r2 0)) (bveq (bv 3 20) (register-ref r2 0))))))

(evaluate br2 sol2)

(println "verify that add is correct for all combinations of non-immediate operands")
(define-symbolic reg3_1 reg3_2 integer?)
(define-symbolic i1 i2 (bitvector 20))
(define-symbolic v1 v2 (bitvector 16))

(define op1 (choose (reg reg3_1) (abs i1) (idx reg3_1 i1)))
(define op2 (choose (reg reg3_2) (abs i2) (idx reg3_1 i2)))
(define r3 (vector (bv 0 20) (bv 0 20) (bv 0 20) (bv 0 20)))
(define m3 (vector (bv 0 20) (bv 0 20) (bv 0 20) (bv 0 20)))
(define running3 (box #t))

(define state3 (state (halt (list (add.w op1 op2))) r3 m3 running3))

(store16 (concat (bv 0 4) v1) op1 r3 m3)
(store16 (concat (bv 0 4) v2) op2 r3 m3)
(stepn state3 2)

(define sol3 
  (verify (assert (bveq (concat (bv 0 4) (bvadd v1 v2)) (load16 op2 r3 m3)))))

sol3
(evaluate op1 sol3)
(evaluate op2 sol3)

(println "issue: operands alias")

(define test-r (vector (bv 0 20) (bv 0 20) (bv 0 20) (bv 0 20)))
(define test-m (vector (bv 0 20) (bv 0 20) (bv 0 20) (bv 0 20)))
(define test-running (box #t))
(define test-state (state (halt (list (add.w (evaluate op1 sol3) (evaluate op2 sol3)))) test-r test-m test-running))

(store16 (concat (bv 0 4) (evaluate v1 sol3)) (evaluate op1 sol3) test-r test-m)
(store16 (concat (bv 0 4) (evaluate v2 sol3)) (evaluate op2 sol3) test-r test-m)
(stepn test-state 2)
test-state
(load16 (evaluate op2 sol3) test-r test-m)
(concat (bv 0 4) (bvadd (evaluate v1 sol3) (evaluate v2 sol3)))