#lang rosette ; Requires a Racket 6.8 / Rosette 2.2 environment

(require racket/cmdline
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

(define (msp430-load state elf-file)
  (for ([interval (in-list (msp-loadelf elf-file))])
    (for ([i (in-range (length (rest interval)))])
      (memory-set! (msp430-state-memory state) (+ i (first interval)) (mspx-bv (list-ref interval (+ i 1))))))
  state)

(define (regs) (msp430-state-registers (machine-state)))
(define (mem) (msp430-state-memory (machine-state)))

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

(define (repl)
  (let* ([line (readline (string-append (emulated-cpu) " > "))]
         [condition (λ (s) (if (equal? s eof) "q" (if (equal? (string-trim s) "") "_" s)))]
         [words (string-split (condition line))]
         [cmd (first words)]
         [params (rest words)]
         [printreg (λ (r) 
                    (printf "register ~a: ~a\n" r 
                      (number->string 
                        (bitvector->natural (vector-ref (regs) r)) 
                        16)))]
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
      [("l" "load") 
       (elf-file (first params))
       (machine-state (msp430-load (machine-state) (elf-file)))
       (printf "loaded ~a\n" (elf-file))]
      [("q" "quit") (set! quit #t)]
      [else (printf "unknown command ~a\n" cmd)])
    (unless quit (repl))))

(machine-state (msp430-state 16 #xfffe))

(if (interactive-mode)
  (repl)
  (msp430-load (machine-state) (elf-file)))
