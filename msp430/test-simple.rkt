#lang rosette/safe

(require rosette/lib/match rosette/lib/synthax "lang-simple.rkt")

(define (symbolic-int)
  (define-symbolic* i integer?)
  i)

(define (symbolic-bv n)
  (define-symbolic* x (bitvector n))
  x)

;; test store-load
(define (test-store-load-16)
  (define-symbolic r1 integer?)
  (define-symbolic x1 (bitvector 20))
  (define-symbolic rv1 rv2 rv3 rv4 mv1 mv2 mv3 mv4 (bitvector 20))
  
  (define op1 (choose (reg r1) (abs x1) (idx r1 x1)))
  
  (define regs (vector rv1 rv2 rv3 rv4))
  (define mem (vector mv1 mv2 mv3 mv4))

  (define-symbolic v1 (bitvector 16))
  (store16 (concat (bv 0 4) v1) op1 regs mem)

  (define test-sol
    (verify (assert (bveq (concat (bv 0 4) v1) (load16 op1 regs mem)))))
  (printf "should be unsat: ~a\n" test-sol)
  )
(test-store-load-16)
(define (test-store-load-8)
  (define-symbolic r1 integer?)
  (define-symbolic x1 (bitvector 20))
  (define-symbolic rv1 rv2 rv3 rv4 mv1 mv2 mv3 mv4 (bitvector 20))
  
  (define op1 (choose (reg r1) (abs x1) (idx r1 x1)))
  
  (define regs (vector rv1 rv2 rv3 rv4))
  (define mem (vector mv1 mv2 mv3 mv4))

  (define-symbolic v1 (bitvector 8))
  (store8 (concat (bv 0 12) v1) op1 regs mem)

  (define test-sol
    (verify (assert (bveq (concat (bv 0 12) v1) (load8 op1 regs mem)))))
  (printf "should be unsat: ~a\n" test-sol)
  )
(test-store-load-8)

;; now hardcode a test of MOV
(define (test-MOV-1)
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
  
  (printf "~a\n" v1)
  (printf "~a\n" v2)
  (printf "~a\n" v1b)
  (printf "~a\n" v2b)
  
  (stepn test-state 2)
  (printf "~a\n" test-state)
  
  (printf "fails due to aliasing\n")
  (define test-sol1
    (verify (assert (bveq (concat (bv 0 4) v1) (load16 op2 regs mem)))))
  (printf "~a\n" test-sol1)
  (printf "~a\n" (evaluate op1 test-sol1))
  (printf "~a\n" (evaluate op2 test-sol1))
  
  (printf "should work\n")
  (define test-sol2
    (verify (assert (bveq (concat (bv 0 4) v1b) (load16 op2 regs mem)))))
  (printf "~a\n" test-sol2)
  
  (printf "this doesn't work because storing v2 might change what you get when you load op1 in a way other than overwriting v1\n")
  (define test-sol3
    (verify (assert (if (and (bveq v1b v2b) (not (bveq v1 v2)))
                        (bveq (concat (bv 0 4) v2) (load16 op2 regs mem))
                        (bveq (concat (bv 0 4) v1) (load16 op2 regs mem))))))
  (printf "~a\n" test-sol3)
  (printf "~a\n" (evaluate op1 test-sol3))
  (printf "~a\n" (evaluate op2 test-sol3))
  (printf "~a\n" (evaluate v1 test-sol3))
  (printf "~a\n" (evaluate v2 test-sol3))
  (printf "~a\n" (evaluate v1b test-sol3))
  (printf "~a\n" (evaluate v2b test-sol3))
  )
(test-MOV-1)

;; what if we just make the whole state symbolic????
(define (test-MOV-2)
  (define-symbolic r1 r2 integer?)
  (define-symbolic x1 x2 (bitvector 20))
  (define-symbolic rv1 rv2 rv3 rv4 mv1 mv2 mv3 mv4 (bitvector 20))
  
  (define op1 (choose (reg r1) (abs x1) (idx r1 x1)))
  (define op2 (choose (reg r2) (abs x2) (idx r2 x2)))
  
  (define regs (vector rv1 rv2 rv3 rv4))
  (define mem (vector mv1 mv2 mv3 mv4))
  (define running (box #t))
  
  (define test-state (state
                      (halt (list (mov.w op1 op2)))
                      regs mem running))

  (define v1 (extract 15 0 (load16 op1 regs mem)))
  (define v2 (extract 15 0 (load16 op2 regs mem)))
  (printf "values should be range over rvs and mvs in terms of rs and xs...\n")
  (printf "~a\n" v1)
  (printf "~a\n" v2)

  (stepn test-state 2)
  (printf "~a\n" test-state)

  (printf "should work fine\n")
  (define test-sol1
    (verify (assert (bveq (concat (bv 0 4) v1) (load16 op2 regs mem)))))
  (printf "~a\n" test-sol1)
  )
(test-MOV-2)
  