#lang rosette/safe

(require rosette/lib/synthax "../lib/util.rkt" "../lib/bv.rkt" "lang-base.rkt")
(require rackunit rackunit/text-ui)

(provide (all-defined-out))

; test suite infrastructure courtesy of rackunit

(define-check (check-sat? model)
  (when (unsat? model) (fail-check "unsat - expecting a model")))

(define-check (check-unsat? model)
  (unless (unsat? model) (fail-check "found a model - expecting unsat")))

(define test-verbosity (box 0))

(define-syntax-rule (vprintf v s args ...)
  (when (>= (unbox test-verbosity) v) (printf s args ...)))

; main test macros

; base case. create symbolic registers and memory, then run the test
(define-syntax-rule (define-test r rn m mn test-body test-assertion)
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
    sol))

(define-syntax define-test/ops
  (syntax-rules ()
    ; no ops are left, pass to base case
    [(define-test/ops r rn m mn mkop test-body test-assertion)
     (define-test r rn m mn test-body test-assertion)]
    ; inductive case. create a symbolic operand in an enclosing let statement
    [(define-test/ops r rn m mn mkop op1 op2 ... test-body test-assertion)
     (let* ([op1 (mkop)]
            ; we've created the symbolic op. also bind the result of the inner call so we can look
            ; at the model and evaluate the op if verification failed
            [sol (define-test/ops r rn m mn mkop op2 ... test-body test-assertion)])
       (unless (unsat? sol)
         (vprintf 2 "    ~a: ~a\n" (quote op1) (evaluate op1 sol)))
       sol)]))

; global test settings

(define test-rn 4)
(define test-mn 4)
(set-box! test-verbosity 0)

; test basic language features

(define-syntax-rule (memory-test-case testname testw memory-set! memory-ref mkx mkaddr)
  (test-case testname
             ; check that if we store and then load, we get the same thing back
             (check-unsat?
              (let ([mem (symbolic-bv-vector mspx-bits test-mn)]
                    [x mkx]
                    [addr mkaddr])
                (memory-set! mem addr x)
                (let ([f (bveq x (memory-ref mem addr))])
                  (verify (assert f)))))
             ; if the operation is less than mspx-bits, check that the high bits are 0 when we load
             (when (< testw mspx-bits)
               (check-unsat?
                (let* ([mem (symbolic-bv-vector mspx-bits test-mn)]
                       [addr mkaddr]
                       [f (bveq (extract (- mspx-bits 1) testw (memory-ref mem addr)) (bv 0 (- mspx-bits testw)))])
                  (verify (assert f)))))
             ))

(define-test-suite ts-base

  ; make sure current-bitwidth is 20 or more (something like that?) or this will time out
  (test-case "addr->integer"
             (check-unsat?
              (let* ([x (symbolic-bv mspx-bits)]
                     [f (bveq (mask1 x) (bvshl (integer->bitvector (addr->integer x) mspx-bv?) (mspx-bv 1)))])
                (verify (assert f)))))

  (memory-test-case "memory16" 16 memory-set16! memory-ref16
                    (word->mspx (symbolic-bv word-bits))
                    (symbolic-bv mspx-bits))
  
  (memory-test-case "memory8" 8 memory-set8! memory-ref8
                    (byte->mspx (symbolic-bv byte-bits))
                    (symbolic-bv mspx-bits))

  (memory-test-case "register20" 20 register-set! register-ref
                    (symbolic-bv mspx-bits)
                    (symbolic-int))

  (memory-test-case "register16" 16 register-set16! register-ref16
                    (word->mspx (symbolic-bv word-bits))
                    (symbolic-int))
  
  (memory-test-case "register8" 8 register-set8! register-ref8
                    (byte->mspx (symbolic-bv byte-bits))
                    (symbolic-int))            
  )