#lang rosette ; Requires a Racket 6.8 / Rosette 2.2 environment

(require racket/cmdline
         racket/format
         readline/readline
         "py-mspdebug/shim/mspdebug.rkt"
         "engine/framework.rkt"
         "engine/framework-addr.rkt"
         "engine/msp430.rkt"
         "lib/mem_ivmap.rkt"
         "lib/bv.rkt")

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
         (f (first interval) (rest interval)))))

(elf-file
  (command-line 
    #:once-each 
    [("-c" "--cpu") cpu ("Which CPU to emulate." 
                         "Currently supported values: 'msp430' (default)")
                    (emulated-cpu cpu)]
    
    [("-i" "--interactive") ("Run the emulator in interactive mode." 
                             "In non-interactive mode, the emulator will load the program" 
                             "specified by <elf-file>, run it, and then print the resulting"
                             "register file.")
                            (interactive-mode #t)]
    #:ps ""
    #:args ([elf-file ""]) rest
    elf-file))

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

(define (repl)
  (let* ([line (readline (string-append (emulated-cpu) " > "))]
         [condition (λ (s) (if (equal? s eof) "q" (if (equal? (string-trim s) "") "_" s)))]
         [words (string-split (condition line))]
         [cmd (first words)]
         [params (rest words)]
         [quit #f])
    (case cmd
      [("r" "reg" "regs") 
       (if (not (null? params))
         (for ([r (in-list params)])
           (let ([reg (string->number r)])
             (if (and reg (< reg (vector-length (regs))))
               (printreg reg)
               (printf "unknown register ~a\n" r))))
         (for ([r (in-range (vector-length (regs)))])
              (printreg r)))]
      [("m" "mem")
       (if (null? params) (printf "usage: mem <addr> [<# bytes>]\n")
         (let ([addr (string->number (first params) 16)]
               [len (if (> (length params) 1) (string->number (second params)) 16)])
           (when (equal? addr #f) (printf "invalid address ~a\n" (first params)))
           (when (equal? len #f) (printf "invalid length ~a\n" (second params)))
           (unless (equal? addr #f)
             (printmem addr #:length len))))]
      [("l" "load") 
       (elf-file (first params))
       (msp430-load (elf-file))
       (printf "loaded ~a\n" (elf-file))]
      [("q" "quit") (set! quit #t)]
      [else (printf "unknown command ~a\n" cmd)])
    (unless quit (repl))))

(machine-state (msp430-state 16 #xfffe))

(if (interactive-mode)
  (repl)
  (begin (msp430-load (elf-file)) (memory-ref (mem) #x2200)))
