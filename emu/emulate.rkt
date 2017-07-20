#lang rosette ; Requires a Racket 6.8 / Rosette 2.2 environment

(require racket/cmdline
         racket/format
         readline/readline
         "../py-mspdebug/shim/mspdebug.rkt"
         "framework.rkt"
         "framework-addr.rkt"
         "cpu/msp430.rkt"
         "../lib/mem_ivmap.rkt"
         "../lib/bv.rkt")

(define interactive-mode (make-parameter #f))
(define emulated-cpu (make-parameter "msp430"))
(define elf-file (make-parameter "msp430"))
(define machine-state (make-parameter (void)))

(define (regs) (msp430-state-registers (machine-state)))
(define (mem) (msp430-state-memory (machine-state)))

(define (msp430-load elf-file)
  (letrec ([f (λ (addr vals)
               (unless (<= (length vals) 1)
                 (memory-set! (mem) (arithmetic-shift addr -1)
                   (if (= (length vals) 1) (mspx-bv (first vals))
                     (bvor (mspx-bv (first vals)) (bvshl (mspx-bv (second vals)) (mspx-bv 8)))))
                 (f (+ addr 2) (drop vals 2))))])
    (for ([interval (in-list (msp-loadelf elf-file))])
         (f (first interval) (rest interval))))
  (let ([entrypt (memory-ref (mem) (arithmetic-shift #xfffe -1))])
    (printf "Entry point ~a\n" (number->string (bitvector->integer entrypt) 16))
    (vector-set! (regs) 0 entrypt)))

(elf-file
  (command-line 
    #:once-each 
    [("-c" "--cpu") cpu ("Which CPU to emulate." 
                         "Currently supported values: 'msp430' (default)")
                    (emulated-cpu cpu)]
    
    [("-i" "--interactive") ("Run the emulator in interactive mode." 
                             "In non-interactive mode, the emulator will load the program" 
                             "specified by <elf-file>, run it, and then print the resulting"
                             "registers.")
                            (interactive-mode #t)]
    #:ps ""
    #:args ([elf-file ""]) rest
    elf-file))

(machine-state (msp430-state 16 #xfffe))

(define (run state)
  (let ([pc-init (vector-ref (regs) 0)])
    (step state)
    (unless (bveq (vector-ref (regs) 0) pc-init) (run state))))

(define (trunc8 x)
  (bvand x (mspx-bv #x000ff)))
(define (high8->low x)
  (trunc8 (bvlshr x (mspx-bv 8))))

(define (membyte->string addr)
   (~a #:min-width 2 #:align 'right #:pad-string "0"
       (number->string 
         (bitvector->natural 
           ((if (= (modulo addr 2) 0) trunc8 high8->low) (memory-ref (mem) (arithmetic-shift addr -1))))
         16)))

(define (printmem addr #:length [len 16] #:group [group 'byte])
  (for ([b (in-range addr (+ addr len) 16)])
    (printf "~a: " (number->string b 16))
    (for ([a (in-range b (+ b 16) 2)])
       (printf "~a ~a " (membyte->string a) (membyte->string (+ a 1))))
    (printf "\n")))

(define (printreg r) 
  (printf "register ~a: ~a\n" r 
    (number->string 
      (bitvector->natural (vector-ref (regs) r)) 
      16)))

(define (printregs)
  (for ([r (in-range (vector-length (regs)))])
       (printreg r)))

(define (repl)
  (let* ([line (readline (string-append (emulated-cpu) " > "))]
         [condition (λ (s) (if (equal? s eof) "q" (if (equal? (string-trim s) "") "_" s)))]
         [words (string-split (condition line))]
         [cmd (first words)]
         [params (rest words)]
         [quit #f])
    (case cmd
      [("r" "reg" "regs" "register" "registers") 
       (if (null? params)
         (printregs)
         (for ([r (in-list params)])
           (let ([reg (string->number r)])
             (if (and reg (< reg (vector-length (regs))))
               (printreg reg)
               (printf "unknown register ~a\n" r)))))]
      [("m" "mem" "memory")
       (if (null? params) (printf "usage: mem <addr> [<# bytes>]\n")
         (let ([addr (string->number (first params) 16)]
               [len (if (> (length params) 1) (string->number (second params)) 16)])
           (when (equal? addr #f) (printf "invalid address ~a\n" (first params)))
           (when (equal? len #f) (printf "invalid length ~a\n" (second params)))
           (unless (equal? addr #f)
             (printmem addr #:length len))))]
      [("s" "step")
       (step (machine-state))]
      [("r" "run")
       (run (machine-state))]
      [("l" "load") 
       (elf-file (first params))
       (msp430-load (elf-file))
       (printf "loaded ~a\n" (elf-file))]
      [("q" "quit") (set! quit #t)]
      [("h" "help")
       (printf "Available commands:\n")
       (printf "  reg [#]: display contents of register # (or all if no argument provided)\n")
       (printf "  mem addr: display contents of memory at addr\n")
       (printf "  load <elf-file>: load an elf file from the filesystem\n")
       (printf "  step: execute one instruction\n")
       (printf "  run: execute instructions until the instruction pointer does not change\n")
       (printf "  help: display this message\n")]
      [else (printf "unknown command ~a\n" cmd)])
    (unless quit (repl))))

(if (interactive-mode)
  (repl)
  (if (equal? (elf-file) "")
    (printf "Elf file needed when running in noninteractive mode (see --help)\n")
    (begin (msp430-load (elf-file))
           (run (machine-state)) 
           (printregs))))
