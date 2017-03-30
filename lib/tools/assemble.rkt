#lang racket

(provide msp430fr5969-as-nostdlib)

;; tool paths
(define mspgcc (find-executable-path "msp430-elf-gcc"))
(define-values (msptools-bin mspgcc-name mspgcc-isdir) (split-path mspgcc))
(define-values (msptools-base msptools-bin-name msptools-bin-isdir) (split-path msptools-bin))
(define msptools-include (build-path msptools-base "include/"))

;; linker files
(define msp430fr5969.ld (build-path msptools-include "msp430fr5969.ld"))

(define (msp430fr5969-as-nostdlib infile outfile)
  (unless (file-exists? infile)
    (raise-argument-error 'msp430fr5969-as-nostdlib "path to assemblable file" infile))
  (define-values (sp sp-stdout sp-stdin sp-stderr)
    (subprocess
     #f #f #f
     mspgcc "-nostdlib" "-L" msptools-include "-T" msp430fr5969.ld infile "-o" outfile))
  (close-output-port sp-stdin)
  (subprocess-wait sp)
  (define stdout-data (port->string sp-stdout))
  (define stderr-data (port->string sp-stderr))
  (close-input-port sp-stdout)
  (close-input-port sp-stderr)
  (define status (subprocess-status sp))
  (unless (= status 0)
    (raise-user-error 'msp430fr5969-as-nostdlib "mspgcc returned ~a\nstdout: ~a\nstderr: ~a"
                      status stdout-data stderr-data))
  (values status stdout-data stderr-data))
