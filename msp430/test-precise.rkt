#lang rosette/safe

(require rosette/lib/synthax "../lib/bv.rkt" "lang-base.rkt" "lang-precise.rkt")
(require rackunit rackunit/text-ui)

; TODO: Ask bill about pulling out stuff into a test-common
(define test-verbosity (box 0))

(define-syntax-rule (vprintf v s args ...)
  (when (>= (unbox test-verbosity) v) (printf s args ...)))

; unnamed streams of symbolic values

(define (tabulate-list f length)
  (if (> length 0)
      (cons (f) (tabulate-list f (- length 1)))
      null))

(define (bv-vector n length)
  (define (bv-fixed) (bv 0 n))
  (list->vector (tabulate-list bv-fixed length)))

; Helper

(define (make-state instrs nreg nmem running)
  (state instrs (bv-vector mspx-bits nreg) (bv-vector mspx-bits nmem) (box running)))

; Temp; eventually this should be solver aided and use symbolic values
(define regs (vector (mspx-bv 0) (mspx-bv 0) (mspx-bv 0) (mspx-bv 0)))
(define mem (vector (mspx-bv 0) (mspx-bv 0) (mspx-bv 0) (mspx-bv 0)))
;(define instrs (vector (mov.w (imm (mspx-bv 37)) (idx 1 (mspx-bv 0)))
;                         (mov.w (imm (mspx-bv 40)) (reg 3))
;                         (sub.w (idx 1 (mspx-bv 0)) (reg 3))
;                         (cmp.w (reg 3) (imm (mspx-bv 3)))
;                         (sub.w (ai 3) (reg 1))))

; (define s (state instrs regs mem (box #t)))


(define-test-suite ts-autoincr

  (test-case "ai different src/dst"
    (check bveq
           (let ([s (make-state (vector (cmp.w (ai 1) (abs (mspx-bv 2)))) 4 4 #t)])
               (begin 
                 (stepn s 1) 
                 (register-ref (state-r s) 1))) 
           (mspx-bv 2))) ; Check that word instruction incremented by 2 bytes

  (test-case "ai same src/dst"
       (let ([s (make-state (vector (mov.w (imm (mspx-bv #xBEEF)) (idx 1 (mspx-bv 0)))
                                    (mov.w (ai 1) (idx 1 (mspx-bv 0)))) 
                            4 4 #t)])
         (check bveq
               (begin 
                 (stepn s 2) 
                 (memory-ref16 (state-m s) (mspx-bv 0)))
               (memory-ref16 (state-m s) (mspx-bv 2))))) ; Check that increment happened before store
)

(run-tests ts-autoincr)
