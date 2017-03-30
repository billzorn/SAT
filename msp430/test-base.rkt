#lang rosette/safe

(require rosette/lib/synthax
         "../lib/rosette-utils.rkt"
         "../lib/bv.rkt"
         "../lib/mem_simple.rkt"
         "lang-base.rkt"
         "flags.rkt")
(require racket/vector rackunit rackunit/text-ui)

(provide (all-defined-out))

; global test settings

(define test-rn 4)
(define test-mn 4)
(define test-verbosity 0)

; test suite infrastructure courtesy of rackunit

(define-check (check-sat? model)
  (when (unsat? model) (fail-check "unsat - expecting a model")))

(define-check (check-unsat? model)
  (unless (unsat? model) (fail-check "found a model - expecting unsat")))

(define-syntax-rule (vprintf v s args ...)
  (when (>= test-verbosity v) (printf s args ...)))

; We presumably do not want a symbolic PC value for these tests.
; For test-precise, it actually makes it impossible to execute stepn, since we
;   need to reference the i_th element of the instructions vector.
; For test-simple, though it may be desirable to allow the framework to treat r0
;   like any other register, it simplifies things enormously if we do not have
;   to generalize the behavior and pass this around through all the test macros.
; TODO: If this test framework gets generalized out to other architectures, the
;   PC may not always be register 0.
(define-syntax-rule (make-register-file bits n)
  (let ([r (symbolic-bv-vector bits n)])
     (vector-set! r 0 (mspx-bv 0))
     r))

; main test macros

; base case. create symbolic registers and memory, then run the test
(define-syntax-rule (define-test r rn m mn test-body test-assertion)
  (let ([r (make-register-file mspx-bits rn)]
        [m (make-memory mn (mspx-bv 0))])
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
    [(define-test/ops r rn m mn test-body test-assertion)
     (define-test r rn m mn test-body test-assertion)]
    ; inductive case. create a symbolic operand in an enclosing let statement
    [(define-test/ops r rn m mn [op1 mkop1] [op2 mkop2] ... test-body test-assertion)
     (let* ([op1 (mkop1)]
            ; we've created the symbolic op. also bind the result of the inner call so we can look
            ; at the model and evaluate the op if verification failed
            [sol (define-test/ops r rn m mn [op2 mkop2] ... test-body test-assertion)])
       (unless (unsat? sol)
         (vprintf 2 "    ~a: ~a\n" (quote op1) (evaluate op1 sol)))
       sol)]))

; flags are hard
(define-syntax define-test/flags
  (syntax-rules ()
    ; fmt1
    [(define-test/flags r rn m mn [op1 mkop1] [op2 mkop2] load src dst res res-expr res. res.-expr test-body flag-assertion)
     (define-test/ops r rn m mn [op1 mkop1] [op2 mkop2]
       (begin
         ; copy state vectors so that the reference calculation won't interfere with test execution
         (define r-copy (vector-copy r))
         (define m-copy (vector-copy m))
         ; define input and result names
         (define src (load op1 r-copy m-copy))
         (define dst (load op2 r-copy m-copy))
         (define res res-expr)
         (define res. res.-expr)
         ; run test
         test-body)
       flag-assertion)]))

; try with a function
(define (test-flags-fmt1 #:run-state run-state
                         #:mk-assert mk-assert
                         #:width [w 16]
                         #:mkop1 mkop1
                         #:mkop2 mkop2
                         #:load. load.
                         #:reference-computation reference-computation
                         #:get-flags get-flags
                         #:rn [rn test-rn]
                         #:mn [mn test-mn])
  (define-test/ops r rn m mn [op1 mkop1] [op2 mkop2]
    (begin
      ; copy state vectors so that the reference calculation won't interfere with test execution
      (define r-copy (vector-copy r))
      (define m-copy (memory-copy m))
      ; define input and result names
      (define reference-flags (get-flags r-copy))
      (define src (load. w op1 r-copy m-copy))
      (define dst (load. w op2 r-copy m-copy))
      (define res (reference-computation src dst reference-flags))
      (define res. (trunc. w res))
      ; run the tests
      (run-state op1 op2 r m)
      ; something like this
;      (define running (box #t))
;      (define test-state (state
;                          (halt (list (instr op1 op2)))
;                          r m running))
;      (stepn test-state 2)
;      (assert (not (unbox running)))
      ; get the flags
      (define flags (get-flags r)))
    ; this assertion shouldn't actually be constructed until inside the body of the define-test/ops
    (mk-assert src dst res res. flags)))

; global flag rules
(define (reference-computation-sub src dst flags)
  (bvsub dst src))

(define (mk-assert-sub-z src dst res res. flags)
  (iff (bveq res. (mspx-bv 0)) (bits-set? flags FLAG/Z)))

  
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
