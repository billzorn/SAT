#lang rosette ; Requires a Racket 6.8 / Rosette 2.2 environment

(require racket/cmdline
         racket/format
         readline/readline
         "../py-mspdebug/shim/mspdebug.rkt"
         "framework.rkt"
         "framework-addr.rkt"
         "cpu/msp430/impl.rkt"
         "../lib/mem_ivmap.rkt"
         "../lib/bv.rkt")

; Command line parameters
(define interactive-mode (make-parameter #f))
(define cosimulate (make-parameter #f))
(define emulated-cpu (make-parameter "msp430"))
(define elf-file (make-parameter "msp430"))

; Program state
;   emulator-state: the set of memory maps that make up the emulated CPU
;   machine-state: the handle to the debug interface controlling the reference hardware
(define emulator-state (make-parameter (void)))
(define machine-state (make-parameter (void)))

(define (regs) (msp430-state-registers (emulator-state)))
(define (mem) (msp430-state-memory (emulator-state)))

; Utility functions
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

; Command line parsing
(command-line 
  #:once-each 
  [("-m" "--cpu") cpu ("Which CPU to emulate." 
                       "Currently supported values: 'msp430' (default)")
                  (emulated-cpu cpu)]
  
  [("-i" "--interactive") ("Run the emulator in interactive mode." 
                           "In non-interactive mode, the emulator will load the program" 
                           "specified by <elf-file>, run it, and then print the resulting"
                           "registers.")
                          (interactive-mode #t)]
  [("-c" "--cosim") ("Run the emulator in cosimulation mode." 
                     "In cosimulation mode, the emulator will launch a debugger and run"
                     "the program on the real hardware for comparison purposes.")
                     (cosimulate #t)]
  #:ps ""
  #:args ([elf-file-path ""]) rest
  (elf-file elf-file-path)))

(emulator-state (msp430-state 16 #xfffe))
(when (cosimulate) (machine-state (mspdebug-init)))

(define (trunc8 x)
  (bvand x (mspx-bv #x000ff)))
(define (high8->low x)
  (trunc8 (bvlshr x (mspx-bv 8))))

(define (membyte->natural addr)
  (bitvector->natural 
    ((if (= (modulo addr 2) 0) trunc8 high8->low) 
     (memory-ref (mem) (arithmetic-shift addr -1)))))

(define (membyte->string addr #:ref [ref membyte->natural])
   (~a #:min-width 2 #:align 'right #:pad-string "0"
       (number->string (ref addr) 16)))

(define (printmem addr #:length [len 16] #:ref [ref membyte->natural])
  (for ([b (in-range addr (+ addr len) 16)])
    (printf "~a: " (number->string b 16))
    (for ([a (in-range b (+ b 16))])
       (if (< a (+ addr len))
         (printf "~a " (membyte->string a #:ref ref))
         (printf "   ")))
    (printf "|")
    (for ([a (in-range b (+ b 16))])
       (if (< a (+ addr len))
         (printf "~a" 
            (let* ([i (ref a)]
                   [c (integer->char i)])
              (if (and (< i 128) 
                       (or (char-alphabetic? c) (char-numeric? c) (char-punctuation? c)))
                  c ".")))
         (printf " ")))
    (printf "|\n")))

(define (printreg regs r) 
  (printf "(~a: ~a) " 
    (case r
      [(0) " PC"]
      [(1) " SP"]
      [(2) " SR"]
      [(3 4 5 6 7 8 9) (format " R~a" r)]
      [else (format "R~a" r)])
    (~r
      (bitvector->natural (vector-ref regs r))
      #:base 16 #:min-width 5 #:pad-string "0")))

(define (printregs regs)
  (for ([g (in-range (quotient (vector-length regs) 4))])
       (for ([r (in-range 4)])
            (printreg regs (+ (* 4 r) g)))
       (printf "\n")))

(define (sync)
  (for ([r (in-range (vector-length (regs)))])
    (msp-setreg (machine-state) r (bitvector->natural (vector-ref (regs) r)))))

(define (run state)
  (let ([pc-init (vector-ref (regs) 0)])
    (step state)
    (if (bveq (vector-ref (regs) 0) pc-init) (printregs (regs)) (run state))))

(define (repl)
  (let* ([line (readline (string-append (emulated-cpu) " > "))]
         [condition (λ (s) (if (equal? s eof) "q" (if (equal? (string-trim s) "") "_" s)))]
         [words (string-split (condition line))]
         [cmd (first words)]
         [params (rest words)]
         [quit #f])
    (case cmd
      [("r" "reg" "regs" "register" "registers") 
       (printf "Emulator:\n")
       (printregs (regs))
       (when (cosimulate) 
         (printf "-------------\n")
         (printf "Hardware:\n")
         (printregs 
           (list->vector (map (λ (x) (bv x 20)) 
                              (msp-regs (machine-state))))))]
      [("m" "md" "mem" "memory")
       (if (null? params) (printf "usage: md <addr> [<# bytes>]\n")
         (let ([addr (string->number (first params) 16)]
               [len (if (> (length params) 1) (string->number (second params)) 16)])
           (when (equal? addr #f) (printf "invalid address ~a\n" (first params)))
           (when (equal? len #f) (printf "invalid length ~a\n" (second params)))
           (unless (equal? addr #f)
             (printf "Emulator:\n")
             (printmem addr #:length len))
           (when (cosimulate)
             (printf "-------------\n")
             (printf "Hardware:\n")
             (let ([mem (msp-md (machine-state) addr len)])
               (printmem addr #:length len #:ref (λ (a) (list-ref mem (- a addr))))))))]
      [("s" "step")
       (step (emulator-state))
       (when (cosimulate) (msp-step (machine-state)))]
      [("sync") (sync)]
      [("r" "run")
       (run (emulator-state))
       (when (cosimulate) (msp-run (machine-state) 1))]
      [("l" "load" "prog") 
       (elf-file (first params))
       (msp430-load (elf-file))
       (when (cosimulate) 
         (msp-prog (machine-state) (elf-file))
         (sync))
       (printf "loaded ~a\n" (elf-file))]
      [("q" "quit") (set! quit #t) (when (cosimulate) (mspdebug-close (machine-state)))]
      [("h" "help")
       (printf "Available commands:\n")
       (printf "  reg [#]: display contents of register # (or all if no argument provided)\n")
       (printf "  md addr: display contents of memory at addr\n")
       (printf "  prog <elf-file>: load an elf file from the filesystem into device RAM\n")
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
           (run (emulator-state)) 
           (printregs (regs)))))
