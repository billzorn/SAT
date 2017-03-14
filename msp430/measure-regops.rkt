#lang racket

(require "../lib/util.rkt"
         "../py-mspdebug/shim/mspdebug.rkt"
         "../lib/tools/bin-symbols.rkt"
         "variants/fr5969.rkt"
         "asm/assemble.rkt")

(provide
 measurement-kernel-compile
 measurement-kernel-symbols
 measurement-kernel-init
 measure-regops)

(define here (get-here))
(define regops-source (build-path here "asm/measure-ram-regops.s"))
(define regops-elf (build-path here "asm/measure-ram-regops.elf"))

(define regop-syms
  '(_start
    _rand
    _rand_end
    _results
    _results_end
    _max_iters
    _set_arg_r1
    _set_arg_r2
    _tmp_r1
    _tmp_r2
    _test_critical
    _test_critical_end
    _arg1
    _arg2
    _argsr
    _final_saved_pc
    _loop_done))

(define (measurement-kernel-compile
         #:name [name regops-elf])
  (msp430fr5969-as-nostdlib regops-source name))

(define (measurement-kernel-symbols
         #:name [name regops-elf])
  (let ([symtab (dump-msp430-symbols regops-elf)])
    (for/hasheq ([s regop-syms])
      (let ([addr (hash-ref symtab s (lambda () (raise-user-error "missing symbol:" s)))])
        (values s addr)))))

(define (measurement-kernel-init
         #:name [name regops-elf])
  (define mspd (mspdebug-init))
  (msp-prog mspd name)
  (msp-fill mspd ram-start ram-size '(0))
  mspd)

(define (measure-regops
         #:opcodes opcodes
         #:rsrc [rsrc 4]
         #:rdst [rdst 5]
         #:arguments arguments
         #:reginit reginit
         #:measurement-kernel [measurement-kernel regops-elf]
         #:runtime [runtime 0.05])
  (define syms (measurement-kernel-symbols #:name measurement-kernel))
  (define _start (hash-ref syms '_start))
  (define _rand (hash-ref syms '_rand))
  (define _rand_end (hash-ref syms '_rand_end))
  (define _results (hash-ref syms '_results))
  ;; (define _results_end (hash-ref syms '_results_end))
  (define _max_iters (hash-ref syms '_max_iters))
  (define _set_arg_r1 (hash-ref syms '_set_arg_r1))
  (define _set_arg_r2 (hash-ref syms '_set_arg_r2))
  (define _tmp_r1 (hash-ref syms '_tmp_r1))
  (define _tmp_r2 (hash-ref syms '_tmp_r2))
  (define _test_critical (hash-ref syms '_test_critical))
  (define _test_critical_end (hash-ref syms '_test_critical_end))
  (define _arg1 (hash-ref syms '_arg1))
  (define _arg2 (hash-ref syms '_arg2))
  (define _argsr (hash-ref syms '_argsr))
  (define _final_saved_pc (hash-ref syms '_final_saved_pc))
  (define _loop_done (hash-ref syms '_loop_done))

  (unless (= (- _rand_end _rand) 64)
    (raise-arguments-error 'measure-regops
                           "register initialization section has wrong size"
                           "expected" 64
                           "given" (- _rand_end _rand)))
  (unless (even? (length opcodes))
    (raise-arguments-error 'measure-regops
                           "measured opcode sequence must have even length"
                           "given sequence" opcodes
                           "given length" (length opcodes)))
  (unless (>= (- _test_critical_end _test_critical) (length opcodes))
    (raise-arguments-error 'measure-regops
                           "measured opcode sequence does not fit in measurement kernel"
                           "given sequence" opcodes
                           "given length" (length opcodes)
                           "available memory" (- _test_critical_end _test_critical)))
  (unless (and (or (= rsrc 1) (and (<= 4 rsrc) (< rsrc 16)))
               (or (= rdst 1) (and (<= 4 rdst) (< rdst 16))))
    (raise-arguments-error 'measure-regops
                           "source and destination registers must be 1 or 4-15"
                           "given rsrc" rsrc
                           "given rdst" rdst))

  (define mspd (measurement-kernel-init #:name measurement-kernel))
  (define max_iters (msp-read-dword mspd _max_iters))

  (unless (> max_iters 0)
    (mspdebug-close mspd)
    (raise-arguments-error 'measure-regops
                           "measurement kernel must run for at least one iteration"
                           "given iterations" max_iters))

  ; setup register argument logic ; mov.w &_tmp_rn, rn
  (define rsrc-logic (append (list (bitwise-ior #x10 rsrc) #x42)
                             (le-word->bytes _tmp_r1)))
  (define rdst-logic (append (list (bitwise-ior #x10 rdst) #x42)
                             (le-word->bytes _tmp_r2)))

  ; load logic and opcodes
  (msp-mw mspd _set_arg_r1 rsrc-logic)
  (msp-mw mspd _set_arg_r2 rdst-logic)
  (msp-mw mspd _test_critical opcodes)
  ; load initial register payload
  (msp-fill mspd _rand 64 reginit)

  ; read back initializations
  (define expected-registers-base
    (let ([r (for/vector ([addr (in-range _rand (+ _rand 64) 4)])
               (bitwise-and (msp-read-dword mspd addr) #xfffff))])
      (vector-set! r 0 _final_saved_pc)
      (vector-set! r 3 0)
      r))
  
  ; we will process the arguments imperatively, dispatching in batches as needed
  (define current-inputs (box '()))
  (define results (box '()))

  ; helpers
  (define (expected-registers rsrc-v rdst-v sr-v)
    (let ([r (vector-copy expected-registers-base)])
      (vector-set! r rsrc rsrc-v)
      (vector-set! r rdst rdst-v)
      (vector-set! r 2 sr-v)
      r))

  (define (expand-inputs inputs)
    (flatten (for/list ([i inputs]) (le-word->bytes i))))

  (define (get-outputs raw-output idx)
    (for/vector ([i (in-range idx (+ idx 64) 4)])
      (le-dword (list-tail raw-output i))))

  (define (run-to-completion)
    (let ([pc-v (msp-run mspd runtime)])
      (unless (= pc-v _loop_done)
        (let ([pc-opcodes (msp-md mspd pc-v 2)])
          (unless (and (= (first pc-opcodes) #xff)
                       (= (second pc-opcodes) #x3f))
            (run-to-completion))))))
  
  (define (measure)
    (define inputs (unbox current-inputs))
    (define iters (length inputs))
    (unless (and (<= 0 iters) (<= iters _max_iters))
      (raise-arguments-error 'measure-regops
                             "attempt at bad measurement"
                             "given iterations" iters
                             "max iterations" _max_iters))
    (define rsrc-inputs (map first inputs))
    (define rdst-inputs (map second inputs))
    (define sr-inputs (map third inputs))

    ; inputs is a list '(arg-n arg-n-1 ... arg-0)

    (msp-mw mspd _arg1 (expand-inputs rsrc-inputs))
    (msp-mw mspd _arg2 (expand-inputs rdst-inputs))
    (msp-mw mspd _argsr (expand-inputs sr-inputs))
    
    (msp-setreg mspd 0 _start)
    (run-to-completion)

    (define raw-output (msp-md mspd _results (* iters 64)))
    (define outputs
      (for/list ([rsrc-v rsrc-inputs]
                 [rdst-v rdst-inputs]
                 [sr-v sr-inputs]
                 [idx (in-range 0 (* iters 64) 64)])
        (cons (expected-registers rsrc-v rdst-v sr-v)
              (get-outputs raw-output idx))))

    ; outputs is a list '((exp-n . res-n) ... (exp-0 . res-0))

    (let ([previous-results (unbox results)])
      (set-box! results (append previous-results (reverse outputs)))
      (set-box! current-inputs '())))

  ; main loop
  (for ([arg arguments])
    (let ([new-inputs (cons arg (unbox current-inputs))])
      (set-box! current-inputs new-inputs)
      (when (>= (length new-inputs) max_iters)
        (measure))))
  (when (> (length (unbox current-inputs)) 0)
    (measure))

  (let ([retval (unbox results)])
    (mspdebug-close mspd)
    retval))
