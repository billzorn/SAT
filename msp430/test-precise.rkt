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

(define-test-suite ts-autoincr

  ;(test-case "ai different src/dst"
  ;  (check bveq
  ;         (let ([s (make-state (vector (cmp.w (ai 1) (abs (mspx-bv 2)))) 4 4 #t)])
  ;             (begin 
  ;               (stepn s 1) 
  ;               (register-ref (state-r s) 1))) 
  ;         (mspx-bv 2))) ; Check that word instruction incremented by 2 bytes

  ;(test-case "ai same src/dst"
  ;     (let ([s (make-state (vector (mov.w (imm (mspx-bv #xBEEF)) (idx 1 (mspx-bv 0)))
  ;                                  (mov.w (ai 1) (idx 1 (mspx-bv 0)))) 
  ;                          4 4 #t)])
  ;       (check bveq
  ;             (begin 
  ;               (stepn s 2) 
  ;               (memory-ref16 (state-m s) (mspx-bv 0)))
  ;             (memory-ref16 (state-m s) (mspx-bv 2))))) ; Check that increment happened before store

  ; Autoincrement op should increment by the width of the load 
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
)

(define-test-suite ts-store/load
    
  ; if you store, then load at the same width, you always load what you stored
  (check-unsat? 
    (define-test/ops r test-rn m test-mn mkop-dst op1
      (begin
         (define-symbolic v1 word?)
         (store16 (word->mspx v1) op1 r m))
      (bveq (word->mspx v1) (load16 op1 r m))))

  (check-unsat? 
    (define-test/ops r test-rn m test-mn mkop-dst op1
      (begin
         (define-symbolic v1 byte?)
         (store8 (byte->mspx v1) op1 r m))
      (bveq (byte->mspx v1) (load8 op1 r m))))

  ; if you don't use the same width, then you might not load what you stored
  (check-sat?
   (define-test/ops r test-rn m test-mn mkop-dst op1
     (begin
       (define-symbolic v1 word?)
       (store16 (word->mspx v1) op1 r m))
     (bveq (word->mspx v1) (load8 op1 r m))))
  
  (check-sat?
   (define-test/ops r test-rn m test-mn mkop-dst op1
     (begin
       (define-symbolic v1 byte?)
       (store8 (byte->mspx v1) op1 r m))
     (bveq (byte->mspx v1) (load16 op1 r m))))

  ; a 16-bit load zeros the 4 high bits, and an 8-bit load zeros the high 12 bits
  (check-unsat?
   (define-test/ops r test-rn m test-mn mkop-dst op1
     (begin
       (define-symbolic v1/immx mspx-bv?)
       (define op1/imm (choose op1 (imm v1/immx))))
     (bveq (bv 0 4) (extract 19 16 (load16 op1/imm r m)))))

  (check-unsat?
   (define-test/ops r test-rn m test-mn mkop-dst op1
     (begin
       (define-symbolic v1/immx mspx-bv?)
       (define op1/imm (choose op1 (imm v1/immx))))
     (bveq (bv 0 12) (extract 19 8 (load8 op1/imm r m)))))
  )

(run-tests ts-autoincr)
(run-tests ts-store/load)
