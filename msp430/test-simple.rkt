#lang rosette/safe

(require rosette/lib/synthax "lang-simple.rkt")
(require rackunit rackunit/text-ui)

(provide check-sat? check-unsat? define-test/ops test-verbosity)

; test suite infrastructure courtesy of rackunit

(define-check (check-sat? model)
  (when (unsat? model) (fail-check "unsat - expecting a model")))

(define-check (check-unsat? model)
  (unless (unsat? model) (fail-check "found a model - expecting unsat")))

(define test-verbosity (box 0))

(define-syntax-rule (vprintf v s args ...)
  (when (>= (unbox test-verbosity) v) (printf s args ...)))

; unnamed streams of symbolic values

(define (tabulate-list f length)
  (if (> length 0)
      (cons (f) (tabulate-list f (- length 1)))
      null))

(define (symbolic-int)
  (define-symbolic* i integer?)
  i)

(define (symbolic-bv n)
  (define-symbolic* x (bitvector n))
  x)

(define (symbolic-bv-vector n length)
  (define (symbolic-bv-fixed) (symbolic-bv n))
  (list->vector (tabulate-list symbolic-bv-fixed length)))

; main test macro. creates symbolic operands as needed

(define-syntax define-test/ops
  (syntax-rules ()
    ; base case. create symbolic registers and memory, then run the test
    [(define-test r rn m mn test-body test-assertion)
     (let ([r (symbolic-bv-vector mspx-bits rn)]
           [m (symbolic-bv-vector mspx-bits mn)])
       test-body
       (vprintf 1 "verifying assertion ~a for test:\n  ~a\n"
               (quote test-assertion) (quote test-body))
       (define test-sol
         (verify (assert test-assertion)))
       (if (unsat? test-sol)
         (vprintf 1 "~a - verification succeeded\n" test-sol)
         (begin
           (vprintf 1 "found a model - verification failed\n")
           (vprintf 2 "  registers: ~a\n" (evaluate r test-sol))
           (vprintf 2 "  memory: ~a\n" (evaluate m test-sol))))
       ; return the model so enclosing let statements can do something with it
       test-sol)]
    ; inductive case. create a symbolic operand in an enclosing let statement
    [(define-test r rn m mn op1 op2 ... test-body test-assertion)
     (let* ([r1 (symbolic-int)]
            [x1 (symbolic-bv mspx-bits)]
            [op1 (choose (reg r1) (abs x1) (idx r1 x1))]
            ; we've created the symbolic op. also bind the result of the inner call so we can look
            ; at the model and evaluate the op if verification failed
            [test-sol (define-test/ops r rn m mn op2 ... test-body test-assertion)])
       (unless (unsat? test-sol)
         (vprintf 2 "    ~a: ~a\n" (quote op1) (evaluate op1 test-sol)))
       test-sol)]))
                      
; test settings

(define test-rn 4)
(define test-mn 4)
(set-box! test-verbosity 0)

; tests

(define-test-suite ts-store/load

  ; if you store, then load at the same width, you always load what you stored
  (check-unsat?
   (define-test/ops r test-rn m test-mn op1
     (begin
       (define-symbolic v1 word?)
       (store16 (word->mspx v1) op1 r m))
     (bveq (word->mspx v1) (load16 op1 r m))))
  
  (check-unsat?
   (define-test/ops r test-rn m test-mn op1
     (begin
       (define-symbolic v1 byte?)
       (store8 (byte->mspx v1) op1 r m))
     (bveq (byte->mspx v1) (load8 op1 r m))))

  ; if you don't use the same width, then you might not load what you stored
  (check-sat?
   (define-test/ops r test-rn m test-mn op1
     (begin
       (define-symbolic v1 word?)
       (store16 (word->mspx v1) op1 r m))
     (bveq (word->mspx v1) (load8 op1 r m))))
  
  (check-sat?
   (define-test/ops r test-rn m test-mn op1
     (begin
       (define-symbolic v1 byte?)
       (store8 (byte->mspx v1) op1 r m))
     (bveq (byte->mspx v1) (load16 op1 r m))))
  )

(define-test-suite ts-double-op
  (test-case
   "mov.w"
   ; simple case. if we move from a register or memory to somewhere else,
   ; then loading the desination should give us the same value as loading
   ; the source before the operation took place
   (check-unsat?
    (define-test/ops r test-rn m test-mn op1 op2
      (begin
        (define running (box #t))
        (define test-state (state
                            (halt (list (mov.w op1 op2)))
                            r m running))
        (define v1 (mspx->word (load16 op1 r m)))
        (stepn test-state 2))
      (bveq v1 (mspx->word (load16 op2 r m)))))
   ; if we move from an immediate, then loading the destination should give us
   ; back the immediate
   (check-unsat?
    (define-test/ops r test-rn m test-mn op2
      (begin
        (define-symbolic v1/imm word?)
        (define op1/imm (imm (word->mspx v1/imm)))
        (define running (box #t))
        (define test-state (state
                            (halt (list (mov.w op1/imm op2)))
                            r m running))
        (stepn test-state 2))
      (bveq v1/imm (mspx->word (load16 op2 r m)))))
   ; we can also test both at the same time, by creating a new symbolic value
   ; that chooses between a source that loads from the state and an immediate
   (check-unsat?
    (define-test/ops r test-rn m test-mn op1 op2
      (begin
        (define-symbolic v1/imm word?)
        (define op1/imm (choose op1 (imm (word->mspx v1/imm))))
        (define running (box #t))
        (define test-state (state
                            (halt (list (mov.w op1/imm op2)))
                            r m running))
        (define v1 (mspx->word (load16 op1/imm r m)))
        (stepn test-state 2))
      (bveq v1 (mspx->word (load16 op2 r m)))))
   ; internally, the emulator stores values as masked 20-bit bitvectors,
   ; but we have been checking the accuracy of the low 16 bits. we could
   ; handle immediates and comparisons with full 20-bit values
   (check-unsat?
    (define-test/ops r test-rn m test-mn op1 op2
      (begin
        (define-symbolic v1/immx mspx-bv?)
        (define op1/imm (choose op1 (imm v1/immx)))
        (define running (box #t))
        (define test-state (state
                            (halt (list (mov.w op1/imm op2)))
                            r m running))
        (define v1 (load16 op1/imm r m))
        (stepn test-state 2))
      (bveq v1 (load16 op2 r m))))
   ; if we compare the immediate directly (without getting it through a load16,
   ; which will mask out the high bits) then we should be able to find cases
   ; where 20-bit immediates aren't preserved by a 16-bit mov.w
   (check-sat?
    (define-test/ops r test-rn m test-mn op2
      (begin
        (define-symbolic v1/immx mspx-bv?)
        (define op1/imm (imm v1/immx))
        (define running (box #t))
        (define test-state (state
                            (halt (list (mov.w op1/imm op2)))
                            r m running))
        (stepn test-state 2))
      (bveq v1/immx (load16 op2 r m))))
   ; one thing that might make sense to check is that whenever we do a mov.w,
   ; the high bits of the destination are 0. here all we're really testing is
   ; load16, as that masks to 16 bits, but we could do a real test with mov.b
   ; and a 16-bit load
   (check-unsat?
    (define-test/ops r test-rn m test-mn op1 op2
      (begin
        (define-symbolic v1/immx mspx-bv?)
        (define op1/imm (choose op1 (imm v1/immx)))
        (define running (box #t))
        (define test-state (state
                            (halt (list (mov.w op1/imm op2)))
                            r m running))
        (define v1 (load16 op1/imm r m))
        (stepn test-state 2))
      (bveq (extract 19 16 v1) (bv 0 4)))))
  )

;(run-tests ts-store/load)
;(run-tests ts-double-op)

;;; now hardcode a test of MOV
;(define (test-MOV-1)
;  (define-symbolic r1 r2 integer?)
;  (define-symbolic x1 x2 (bitvector 20))
;  (define-symbolic v1 v2 (bitvector 16))
;  
;  (define op1 (choose (reg r1) (abs x1) (idx r1 x1)))
;  (define op2 (choose (reg r2) (abs x2) (idx r2 x2)))
;  
;  (define regs (vector (bv 0 20) (bv 0 20) (bv 0 20) (bv 0 20)))
;  (define mem (vector (bv 0 20) (bv 0 20) (bv 0 20) (bv 0 20)))
;  (define running (box #t))
;  
;  (define test-state (state
;                      (halt (list (mov.w op1 op2)))
;                      regs mem running))
;  
;  (store16 (concat (bv 0 4) v1) op1 regs mem)
;  (store16 (concat (bv 0 4) v2) op2 regs mem)
;  ;; these writes could hit the same location, in which case v1 will be lost
;  (define v1b (extract 15 0 (load16 op1 regs mem)))
;  (define v2b (extract 15 0 (load16 op2 regs mem)))
;  
;  (printf "~a\n" v1)
;  (printf "~a\n" v2)
;  (printf "~a\n" v1b)
;  (printf "~a\n" v2b)
;  
;  (stepn test-state 2)
;  (printf "~a\n" test-state)
;  
;  (printf "fails due to aliasing\n")
;  (define test-sol1
;    (verify (assert (bveq (concat (bv 0 4) v1) (load16 op2 regs mem)))))
;  (printf "~a\n" test-sol1)
;  (printf "~a\n" (evaluate op1 test-sol1))
;  (printf "~a\n" (evaluate op2 test-sol1))
;  
;  (printf "should work\n")
;  (define test-sol2
;    (verify (assert (bveq (concat (bv 0 4) v1b) (load16 op2 regs mem)))))
;  (printf "~a\n" test-sol2)
;  
;  (printf "this doesn't work because storing v2 might change what you get when you load op1 in a way other than overwriting v1\n")
;  (define test-sol3
;    (verify (assert (if (and (bveq v1b v2b) (not (bveq v1 v2)))
;                        (bveq (concat (bv 0 4) v2) (load16 op2 regs mem))
;                        (bveq (concat (bv 0 4) v1) (load16 op2 regs mem))))))
;  (printf "~a\n" test-sol3)
;  (printf "~a\n" (evaluate op1 test-sol3))
;  (printf "~a\n" (evaluate op2 test-sol3))
;  (printf "~a\n" (evaluate v1 test-sol3))
;  (printf "~a\n" (evaluate v2 test-sol3))
;  (printf "~a\n" (evaluate v1b test-sol3))
;  (printf "~a\n" (evaluate v2b test-sol3))
;  )
;(test-MOV-1)
;
;;; what if we just make the whole state symbolic????
;(define (test-MOV-2)
;  (define-symbolic r1 r2 integer?)
;  (define-symbolic x1 x2 (bitvector 20))
;  (define-symbolic rv1 rv2 rv3 rv4 mv1 mv2 mv3 mv4 (bitvector 20))
;  
;  (define op1 (choose (reg r1) (abs x1) (idx r1 x1)))
;  (define op2 (choose (reg r2) (abs x2) (idx r2 x2)))
;  
;  (define regs (vector rv1 rv2 rv3 rv4))
;  (define mem (vector mv1 mv2 mv3 mv4))
;  (define running (box #t))
;  
;  (define test-state (state
;                      (halt (list (mov.w op1 op2)))
;                      regs mem running))
;
;  (define v1 (extract 15 0 (load16 op1 regs mem)))
;  (define v2 (extract 15 0 (load16 op2 regs mem)))
;  (printf "values should be range over rvs and mvs in terms of rs and xs...\n")
;  (printf "~a\n" v1)
;  (printf "~a\n" v2)
;
;  (stepn test-state 2)
;  (printf "~a\n" test-state)
;
;  (printf "should work fine\n")
;  (define test-sol1
;    (verify (assert (bveq (concat (bv 0 4) v1) (load16 op2 regs mem)))))
;  (printf "~a\n" test-sol1)
;  )
;(test-MOV-2)
;  