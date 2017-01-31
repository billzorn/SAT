#lang rosette/safe

(require rosette/lib/synthax "../lib/util.rkt" "../lib/bv.rkt" "test-base.rkt" "lang-base.rkt" "lang-simple.rkt")
(require rackunit rackunit/text-ui)

; op creation specific to lang-simple
(define (mkop)
  (let ([r (symbolic-int)]
        [x (symbolic-bv mspx-bits)])
    (choose (reg r) (abs x) (idx r x))))

; tests

(define-test-suite ts-store/load

  ; if you store, then load at the same width, you always load what you stored
  (check-unsat?
   (define-test/ops r test-rn m test-mn mkop op1
     (begin
       (define-symbolic v1 word?)
       (store16 (word->mspx v1) op1 r m))
     (bveq (word->mspx v1) (load16 op1 r m))))
  
  (check-unsat?
   (define-test/ops r test-rn m test-mn mkop op1
     (begin
       (define-symbolic v1 byte?)
       (store8 (byte->mspx v1) op1 r m))
     (bveq (byte->mspx v1) (load8 op1 r m))))

  ; if you don't use the same width, then you might not load what you stored
  (check-sat?
   (define-test/ops r test-rn m test-mn mkop op1
     (begin
       (define-symbolic v1 word?)
       (store16 (word->mspx v1) op1 r m))
     (bveq (word->mspx v1) (load8 op1 r m))))
  
  (check-sat?
   (define-test/ops r test-rn m test-mn mkop op1
     (begin
       (define-symbolic v1 byte?)
       (store8 (byte->mspx v1) op1 r m))
     (bveq (byte->mspx v1) (load16 op1 r m))))

  ; a 16-bit load zeros the 4 high bits, and an 8-bit load zeros the high 12 bits
  (check-unsat?
   (define-test/ops r test-rn m test-mn mkop op1
     (begin
       (define-symbolic v1/immx mspx-bv?)
       (define op1/imm (choose op1 (imm v1/immx))))
     (bveq (bv 0 4) (extract 19 16 (load16 op1/imm r m)))))

  (check-unsat?
   (define-test/ops r test-rn m test-mn mkop op1
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
    (define-test/ops r test-rn m test-mn mkop op1 op2
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
    (define-test/ops r test-rn m test-mn mkop op2
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
    (define-test/ops r test-rn m test-mn mkop op1 op2
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
    (define-test/ops r test-rn m test-mn mkop op1 op2
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
    (define-test/ops r test-rn m test-mn mkop op2
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
    (define-test/ops r test-rn m test-mn mkop op1 op2
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

(define (test-all)
  (run-tests ts-store/load)
  (run-tests ts-double-op))

;; experimental
(define eregs (symbolic-bv-vector mspx-bits 4))
(define emem (symbolic-bv-vector mspx-bits 4))
