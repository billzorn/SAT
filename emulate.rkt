#lang rosette ; Requires a Racket 6.8 / Rosette 2.2 environment

(require racket/cmdline
         "py-mspdebug/shim/mspdebug.rkt"
         "engine/framework.rkt"
         "engine/framework-addr.rkt"
         "engine/msp430.rkt"
         "lib/mem_ivmap.rkt"
         "lib/bv.rkt")

(define (msp430-load state elf-file)
  (for ([interval (in-list (msp-loadelf elf-file))])
    (for ([i (in-range (length (rest interval)))])
      (memory-set! (msp430-state-memory state) (+ i (first interval)) (mspx-bv (list-ref interval (+ i 1))))))
  state)


(define elf-file
  (command-line 
    #:args (elf-file)
    elf-file))

(msp430-load (msp430-state 16 #xfffe) elf-file)
