#lang racket

(require "../lib/util.rkt"
         "../py-mspdebug/shim/mspdebug.rkt"
         "../lib/tools/bin-symbols.rkt"
         "asm/assemble.rkt")

(define here (get-here))

(dump-msp430-symbols (build-path here "asm/test-regs.elf"))
