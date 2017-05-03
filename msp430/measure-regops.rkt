#lang racket

(require "../lib/racket-utils.rkt"
         "../py-mspdebug/shim/mspdebug.rkt"
         "../lib/tools/bin-symbols.rkt"
         "../lib/tools/assemble.rkt"
         "variants/fr5969.rkt")

(provide
 ; information for msp430fr5969 reference implementation
 regops-sources regops-elf regops-syms
 ; setup
 measurement-kernel-compile
 measurement-kernel-symbols
 measurement-kernel-init
 ; main measurement method
 measure-regops
 ; parallel version
 measure-regops/par
 )

(define here (get-here))

; reference implementation
(define regops-sources (list (build-path here "asm/measure-ram-regops.s")
                            (build-path here "asm/catchall.s")))
(define regops-elf    (build-path here "asm/measure-ram-regops.elf"))
(define regops-syms  '(_start
                       _rand
                       _rand_end
                       _results
                       _results_end
                       _max_iters
                       _set_arg_r1
                       _set_arg_r2
                       _set_arg_sr
                       _tmp_r1
                       _tmp_r2
                       _test_critical
                       _test_critical_end
                       _arg1
                       _arg2
                       _argsr
                       _final_saved_pc
                       _loop_done))

; setup

; Compiles a binary kernel from assembly source for use in taking measurements.
; Depends on the msp430 C compiler as an assembler, and assumes that the source
; file is a stand-alone executable able to be compiled with -nostdlib.
;
; * source : (or/c path? string-no-nuls? bytes-no-nuls?) is the name of the source
; file to be compiled. Defaults to the provided msp430fr5969 reference.
;
; * elf : (or/c path? string-no-nuls? bytes-no-nuls?) is the name of the binary
; file to tell the compiler to output. Defaults to the provided msp430fr5969
; reference.
;
; Returns the output of the assembler - the return code, data on stdout, and data
; on stderr.
(define (measurement-kernel-compile
         #:source [source regops-sources]
         #:elf    [elf regops-elf])
  (msp430fr5969-as-nostdlib source elf))

; Extracts symbols from an assembled binary kernel using msp430 objdump.
;
; * syms : (listof symbol?) is the list of symbols to extract. Defaults to the
; provided msp430fr5969 reference.
;
; * elf : (or/c path? string-no-nuls? bytes-no-nuls?) is the name of the binary kernel
; file to extract symbols from. Defaults to the provided msp430fr5969 reference.
;
; Returns an immutable hasheq mapping symbols to numerical values. Fails if any requested
; symbol is missing from the binary.
(define (measurement-kernel-symbols
         #:syms [syms regops-syms]
         #:elf  [elf regops-elf])
  (let ([symtab (dump-msp430-symbols elf)])
    (for/hasheq ([s syms])
      (let ([addr (hash-ref symtab s (lambda () (raise-user-error "missing symbol:" s)))])
        (values s addr)))))

; Sets up an mspdebug instance for the given binary measurement kernel.
;
; * elf : (or/c path? string-no-nuls? bytes-no-nuls?) is the name of the binary kernel
; to run. Defaults to the provided msp430fr5969 reference.
;
; Returns an mspdebug struct that can be used with the py-mspdebug shim.
(define (measurement-kernel-init
         #:elf [elf regops-elf])
  (define mspd (mspdebug-init))
  (msp-prog mspd elf)
  (msp-fill mspd ram-start ram-size '(0))
  mspd)

; Measurement procedure for format I double-operand instructions in register mode.
; Depends on all of the symbols named in regops-syms, and will fail if not provided
; a kernel that exposes them.
;
; * opcodes : (listof number?) is the code segment to be measured. Will be directly
; written into memory as a sequence of bytes.
;
; * arguments : (listof (listof number?)) is the set of arguments to measure. Each
; list of arguments in the set should have three elements: the value to supply
; in rsrc, the value to supply in rdst, and the value to supply in the status
; register.
;
; * rsrc : number? is the register to use as the source register of the operation.
; This controls how argument setup is done, and assumes the provided opcode(s)
; actually use this register as the source. Must be 1 or 4-15.
;
; * rdst : number? is like rsrc, but identifies the destination register. Must
; aslo be 1 or 4-15.
;
; * reginit : (listof number?) is a bitpattern to use for base initializtion of
; registers during testing. The source and destination registers will be overwritten
; with the arguments during actual tests. A pattern of any length can be provided,
; and will be filled over a 64-byte area used to pre-initialize registers via POPM.A.
;
; * runtime : number? is the number of seconds to run each test batch for. If the test
; does not complete, it will be continued until it does. Extremely small values work
; well, such as 0.05.
;
; * meas-elf : (or/c path? string-no-nuls? bytes-no-nuls?) is the name of the binary kernel
; to use for measurement. Defaults to the provided msp430fr5969 reference.
;
; * meas-syms : (or/c void? hash-eq?) is an optional hasheq of symbols to use.
; If not void, overrides the default behavior of extracting symbols from meas-elf.
;
; * meas-mspd : mspdebug? is an optional mspdebug struct to use. If not void,
; overrides the default behavior of opening a new session with measurement-kernel-init.
;
; Returns a list measurement results. Each result is a pair of vectors, with the first
; representing the values of the registers before the provide opcodes were run, and the
; second the values of the registers after. The order is the same as the order of the
; arguments, but the values of the arguments can also be recovered from the first
; vector.
(define (measure-regops
         #:opcodes   opcodes
         #:arguments arguments
         #:rsrc      [rsrc 4]
         #:rdst      [rdst 5]
         #:reginit   [reginit '(#xab #xcd #x0f #x00)]
         #:runtime   [runtime 0.05]
         #:meas-elf  [meas-elf regops-elf]
         #:meas-syms [meas-syms (void)]
         #:meas-mspd [meas-mspd (void)])

  ; get symbols and check for validity
  (define syms (if (void? meas-syms)
                   (measurement-kernel-symbols #:elf meas-elf)
                   meas-syms))

  (define-syntax-rule (defsym id) (define id (hash-ref syms (quote id))))
  (defsym _start)
  (defsym _rand)
  (defsym _rand_end)
  (defsym _results)
  ;; (defsym _results_end)
  (defsym _max_iters)
  (defsym _set_arg_r1)
  (defsym _set_arg_r2)
  (defsym _set_arg_sr)
  (defsym _tmp_r1)
  (defsym _tmp_r2)
  (defsym _test_critical)
  (defsym _test_critical_end)
  (defsym _arg1)
  (defsym _arg2)
  (defsym _argsr)
  (defsym _final_saved_pc)
  (defsym _loop_done)

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

  ; get mspd connection and finish validity checks
  (define mspd (if (void? meas-mspd)
                   (measurement-kernel-init #:elf meas-elf)
                   meas-mspd))
  
  (define max_iters (msp-read-dword mspd _max_iters))

  (unless (> max_iters 0)
    (mspdebug-close mspd)
    (raise-arguments-error 'measure-regops
                           "measurement kernel must run for at least one iteration"
                           "given iterations" max_iters))

  ; setup register argument logic ; mov.w &_tmp_rn, rn
  (define rsrc-logic (append (list #x00 #x18 (bitwise-ior #x50 rsrc) #x42)
                             (le-word->bytes _tmp_r1)))
  (define rdst-logic (append (list #x00 #x18 (bitwise-ior #x50 rdst) #x42)
                             (le-word->bytes _tmp_r2)))

  (unless (and (= (length rsrc-logic) (- _set_arg_r2 _set_arg_r1))
               (= (length rdst-logic) (- _set_arg_sr _set_arg_r2))
               (< _set_arg_r1 #x10000)
               (< _set_arg_r2 #x10000)
               (< _set_arg_sr #x10000))
    (raise-arguments-error 'measure-regops
                           "invalid argument setup logic"
                           "rsrc opcodes" rsrc-logic
                           "rdst opcodes" rdst-logic
                           "_set_arg_r1" (format "0x~x" _set_arg_r1)
                           "_set_arg_r2" (format "0x~x" _set_arg_r2)
                           "_set_arg_sr" (format "0x~x" _set_arg_sr)))

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

  ; batched measurement for all currently collected inputs
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

; Parallel version of measure-regops.
;
; * nprocs : number? is the number of devices to use. Don't try to use more
; devices than are available at one time, or measurement will probably fail
; rather than waiting for them to become available.
;
; Behavior is otherwise identical to measure-regops.
(define (measure-regops/par
         #:nprocs    nprocs
         #:opcodes   opcodes
         #:arguments arguments
         #:rsrc      [rsrc 4]
         #:rdst      [rdst 5]
         #:reginit   [reginit '(#xab #xcd #x0f #x00)]
         #:runtime   [runtime 0.05]
         #:meas-elf  [meas-elf regops-elf])

  (define argument-chunks (chunks arguments nprocs))
  (define places (make-vector nprocs (void)))

  (define syms (measurement-kernel-symbols #:elf meas-elf))

  (for ([i (in-range nprocs)])
    (let ([p (place pch
                    (let* ([opcodes (place-channel-get pch)]
                           [arguments (place-channel-get pch)]
                           [rsrc (place-channel-get pch)]
                           [rdst (place-channel-get pch)]
                           [reginit (place-channel-get pch)]
                           [runtime (place-channel-get pch)]
                           [meas-elf (place-channel-get pch)]
                           [meas-syms (place-channel-get pch)]
                           [meas (measure-regops
                                  #:opcodes opcodes
                                  #:arguments arguments
                                  #:rsrc rsrc
                                  #:rdst rdst
                                  #:reginit reginit
                                  #:runtime runtime
                                  #:meas-elf meas-elf
                                  #:meas-syms meas-syms
                                  #:meas-mspd (void))])
                      (place-channel-put pch meas)))])
      (place-channel-put p opcodes)
      (place-channel-put p (list-ref argument-chunks i))
      (place-channel-put p rsrc)
      (place-channel-put p rdst)
      (place-channel-put p reginit)
      (place-channel-put p runtime)
      (place-channel-put p meas-elf)
      (place-channel-put p syms)
      (vector-set! places i p)))

  (apply append
         (for/list ([p places])
           (unless (= (place-wait p) 0)
             (raise-arguments-error 'measure/par
                                    "measurement process terminated abnormally"))
           (place-channel-get p))))
