#lang rosette/safe

(require rosette/lib/synthax "../lib/util.rkt" "../lib/bv.rkt" "test-base.rkt" "lang-base.rkt" "lang-precise.rkt")
(require rackunit rackunit/text-ui)

; op creation specific to lang-precise
(define (mkop-src)
  (let ([r (symbolic-int)]
        [x (symbolic-bv mspx-bits)])
    (choose (reg r) (idx r x) (sym x) (abs x) (cg r) (ind r) (imm2 x) (ai r) (imm x))))

(define (mkop-src-noai)
  (let ([r (symbolic-int)]
        [x (symbolic-bv mspx-bits)])
    (choose (reg r) (idx r x) (sym x) (abs x) (cg r) (ind r) (imm2 x) (imm x))))

(define (mkop-dst)
  (let ([r (symbolic-int)]
        [x (symbolic-bv mspx-bits)])
    (choose (reg r) (idx r x) (sym x) (abs x))))

(define-test-suite ts-store/load
    
  ; if you store, then load at the same width, you always load what you stored
  (check-unsat? 
    (define-test/ops r test-rn m test-mn [op1 mkop-dst]
      (begin
         (define-symbolic v1 word?)
         (store16 (word->mspx v1) op1 r m))
      (bveq (word->mspx v1) (load16 op1 r m))))

  (check-unsat? 
    (define-test/ops r test-rn m test-mn [op1 mkop-dst]
      (begin
         (define-symbolic v1 byte?)
         (store8 (byte->mspx v1) op1 r m))
      (bveq (byte->mspx v1) (load8 op1 r m))))

  ; if you don't use the same width, then you might not load what you stored
  (check-sat?
   (define-test/ops r test-rn m test-mn [op1 mkop-dst]
     (begin
       (define-symbolic v1 word?)
       (store16 (word->mspx v1) op1 r m))
     (bveq (word->mspx v1) (load8 op1 r m))))
  
  (check-sat?
   (define-test/ops r test-rn m test-mn [op1 mkop-dst]
     (begin
       (define-symbolic v1 byte?)
       (store8 (byte->mspx v1) op1 r m))
     (bveq (byte->mspx v1) (load16 op1 r m))))

  ; a 16-bit load zeros the 4 high bits, and an 8-bit load zeros the high 12 bits
  (check-unsat?
   (define-test/ops r test-rn m test-mn [op1 mkop-dst]
     (begin
       (define-symbolic v1/immx mspx-bv?)
       (define op1/imm (choose op1 (imm v1/immx))))
     (bveq (bv 0 4) (extract 19 16 (load16 op1/imm r m)))))

  (check-unsat?
   (define-test/ops r test-rn m test-mn [op1 mkop-dst]
     (begin
       (define-symbolic v1/immx mspx-bv?)
       (define op1/imm (choose op1 (imm v1/immx))))
     (bveq (bv 0 12) (extract 19 8 (load8 op1/imm r m)))))
  )

(define-test-suite ts-autoincr

  ; Autoincrement op should increment by the width of the load 
  ; The hardware doesn't constrain which values the address can take, so it
  ; should be +2 or +1 respectively in all cases.
  (check-unsat?
    (define-test r test-rn m test-mn
      (begin
         (define rn (symbolic-int))
         (define r/before (register-ref r rn))
         (load16 (ai rn) r m))
      (bveq (register-ref r rn) (bvadd r/before (mspx-bv 2)))))

  (check-unsat?
    (define-test r test-rn m test-mn
      (begin
         (define rn (symbolic-int))
         (define r/before (register-ref r rn))
         (load8 (ai rn) r m))
      (bveq (register-ref r rn) (bvadd r/before (mspx-bv 1)))))

  ; When you store a value to some address, then load with autoincr, the value
  ; should be the same
  (check-unsat?
    (define-test r test-rn m test-mn
       (begin
         (define-symbolic v1 word?)
         (define rn (symbolic-int))
         (define op1 (abs (register-ref r rn)))
         (store16 (word->mspx v1) op1 r m))
       (bveq (word->mspx v1) (load16 (ai rn) r m))))

  (check-unsat?
    (define-test r test-rn m test-mn
       (begin
         (define-symbolic v1 byte?)
         (define rn (symbolic-int))
         (define op1 (abs (register-ref r rn)))
         (store8 (byte->mspx v1) op1 r m))
       (bveq (byte->mspx v1) (load8 (ai rn) r m))))

  ; if you don't use the same width, then you might not load what you stored
  (check-sat?
   (define-test r test-rn m test-mn
     (begin
       (define-symbolic v1 word?)
       (define i (symbolic-int))
       (store16 (word->mspx v1) (idx i (mspx-bv 0)) r m))
     (bveq (word->mspx v1) (load8 (ai i) r m))))
  
  (check-sat?
   (define-test r test-rn m test-mn
     (begin
       (define-symbolic v1 byte?)
       (define i (symbolic-int))
       (store8 (byte->mspx v1) (idx i (mspx-bv 0)) r m))
     (bveq (byte->mspx v1) (load16 (ai i) r m))))
)

(define-syntax-rule (run-state-simple instr)
  (lambda (op1 op2 r m)
    ; temp: use a static pc
    (vector-set! r REG/PC (mspx-bv 0))
    (define running (box #t))
    (define test-state (state
                        (vector (instr op1 op2))
                        r m running))
    (stepn test-state 1)
    ; (assert (not (unbox running)))
    ))

(define (get-flags-simple r)
  (register-ref r REG/SR))

(define-syntax-rule (test-flags-fmt1-simple instr w reference mk-assert)
  (test-flags-fmt1 #:run-state (run-state-simple instr)
                   #:mk-assert mk-assert
                   #:width w
                   #:mkop1 mkop-src
                   #:mkop2 mkop-dst
                   #:load. (lambda (width src r m) (load. width src r m))
                   #:reference-computation reference
                   #:get-flags get-flags-simple))

(define-test-suite ts-fmt1-flags
  (test-case
   "cmp.w"
   (check-unsat? (test-flags-fmt1-simple
                  cmp.w 16
                  reference-computation-sub
                  mk-assert-sub-z)))
  (test-case
   "cmp.b"
   (check-unsat? (test-flags-fmt1-simple
                  cmp.b 8
                  reference-computation-sub
                  mk-assert-sub-z)))
  )

(run-tests ts-autoincr)
(run-tests ts-store/load)
(run-tests ts-fmt1-flags)
