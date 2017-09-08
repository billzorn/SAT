#!/bin/sh
#|
exec racket -tm $0 -- $*
|#

#lang racket

(require "emulate.rkt")

(define (main . args)
  ; Command line parameters
  (define interactive-mode #f)
  (define cosimulate #f)
  (define elf-file (void))
  
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
    (set! elf-file elf-file-path))
  
  (run #:interactive-mode interactive-mode
       #:cosimulate cosimulate
       #:elf-file elf-file))

(define-namespace-anchor emu)
(define (place-main ch)
  (eval (place-channel-get ch) (namespace-anchor->namespace meas)))
