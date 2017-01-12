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
       ; this definition prevents any exceptions that would come from the creation
       ; of the assertion (like mismatched bv sizes) from being captured by verify
       (define assertion-body test-assertion)
       (define sol (verify (assert assertion-body)))
       (if (unsat? sol)
         (vprintf 1 "~a - verification succeeded\n" sol)
         (begin
           (vprintf 1 "found a model - verification failed\n")
           (vprintf 5 "~a\n" sol)
           (vprintf 2 "  registers: ~a\n" (evaluate r sol))
           (vprintf 2 "  memory: ~a\n" (evaluate m sol))))
       ; return the model so enclosing let statements can do something with it
       sol)]
    ; inductive case. create a symbolic operand in an enclosing let statement
    [(define-test r rn m mn op1 op2 ... test-body test-assertion)
     (let* ([r1 (symbolic-int)]
            [x1 (symbolic-bv mspx-bits)]
            [op1 (choose (reg r1) (abs x1) (idx r1 x1))]
            ; we've created the symbolic op. also bind the result of the inner call so we can look
            ; at the model and evaluate the op if verification failed
            [sol (define-test/ops r rn m mn op2 ... test-body test-assertion)])
       (unless (unsat? sol)
         (vprintf 2 "    ~a: ~a\n" (quote op1) (evaluate op1 sol)))
       sol)]))
                      
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

  ; a 16-bit load zeros the 4 high bits, and an 8-bit load zeros the high 12 bits
  (check-unsat?
   (define-test/ops r test-rn m test-mn op1
     (begin
       (define-symbolic v1/immx mspx-bv?)
       (define op1/imm (choose op1 (imm v1/immx))))
     (bveq (bv 0 4) (extract 19 16 (load16 op1/imm r m)))))

  (check-unsat?
   (define-test/ops r test-rn m test-mn op1
     (begin
       (define-symbolic v1/immx mspx-bv?)
       (define op1/imm (choose op1 (imm v1/immx))))
     (bveq (bv 0 12) (extract 19 8 (load8 op1/imm r m)))))
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
