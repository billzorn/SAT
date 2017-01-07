#lang rosette/safe

(require rosette/lib/match rosette/lib/synthax "lang-simple.rkt")

(define (symbolic-int)
  (define-symbolic* i integer?)
  i)

(define (symbolic-bv n)
  (define-symbolic* x (bitvector n))
  x)

;; look this state works

(define s (state (halt (list (add.w (idx 0 (bv 6 20))
                                    (abs (bv 4 20)))))
                 (vector (bv 0 20) (bv 0 20) (bv 0 20) (bv 0 20))
                 (vector (bv 0 20) (bv 0 20) (bv 5119 20) (bv 65279 20))
                 (box #t)))

(stepn s 2)
s

;; now hardcode a test of MOV

(define-symbolic r1 r2 integer?)
(define-symbolic x1 x2 (bitvector 20))
(define-symbolic v1 v2 (bitvector 16))

(define op1 (choose (reg r1) (abs x1) (idx r1 x1)))
(define op2 (choose (reg r2) (abs x2) (idx r2 x2)))

(define regs (vector (bv 0 20) (bv 0 20) (bv 0 20) (bv 0 20)))
(define mem (vector (bv 0 20) (bv 0 20) (bv 0 20) (bv 0 20)))
(define running (box #t))

(define test-state (state
                    (halt (list (mov.w op1 op2)))
                    regs mem running))

(store16 (concat (bv 0 4) v1) op1 regs mem)
(store16 (concat (bv 0 4) v2) op2 regs mem)
;; these writes could hit the same location, in which case v1 will be lost
(define v1b (extract 15 0 (load16 op1 regs mem)))
(define v2b (extract 15 0 (load16 op2 regs mem)))

v1
v2
v1b
v2b

(stepn test-state 2)
test-state

(printf "fails due to aliasing\n")
(define test-sol1
  (verify (assert (bveq (concat (bv 0 4) v1) (load16 op2 regs mem)))))
test-sol1
(evaluate op1 test-sol1)
(evaluate op2 test-sol1)

(printf "should work\n")
(define test-sol2
  (verify (assert (bveq (concat (bv 0 4) v1b) (load16 op2 regs mem)))))
test-sol2

(printf "this doesn't work because storing v2 might change what you get when you load op1 in a way other than overwriting v1\n")
(define test-sol3
  (verify (assert (if (and (bveq v1b v2b) (not (bveq v1 v2)))
                      (bveq (concat (bv 0 4) v2) (load16 op2 regs mem))
                      (bveq (concat (bv 0 4) v1) (load16 op2 regs mem))))))
test-sol3
(evaluate op1 test-sol3)
(evaluate op2 test-sol3)
(evaluate v1 test-sol3)
(evaluate v2 test-sol3)
(evaluate v1b test-sol3)
(evaluate v2b test-sol3)