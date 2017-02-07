#lang rosette/safe

(require rosette/lib/synthax "../lib/util.rkt" "../lib/bv.rkt" "lang-simple.rkt")
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

; test settings

(define test-rn 4)
(define test-mn 4)
(set-box! test-verbosity 0)