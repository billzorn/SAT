#lang racket ; Requires a Racket 6.8+ / Rosette 2.2 environment

(require racket/cmdline
         "emulate.rkt")

; Command line parameters
(define interactive-mode (make-parameter #f))
(define cosimulate (make-parameter #f))
(define elf-file (make-parameter (void)))

; Command line parsing
(command-line 
  #:once-each 
  [("-i" "--interactive") ("Run the emulator in interactive mode." 
                           "In non-interactive mode, the emulator will load the program" 
                           "specified by <elf-file>, run it, and then print the resulting"
                           "registers.")
                          (set! interactive-mode #t)]
  [("-c" "--cosim") ("Run the emulator in cosimulation mode." 
                     "In cosimulation mode, the emulator will launch a debugger and run"
                     "the program on the real hardware for comparison purposes.")
                     (set! cosimulate #t)]
  #:ps ""
  #:args ([elf-file-path ""]) rest
  (elf-file elf-file-path))

(run #:interactive-mode interactive-mode
     #:cosimulate cosimulate
     #:elf-file elf-file)
