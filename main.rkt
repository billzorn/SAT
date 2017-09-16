#!/bin/sh
#|
exec racket -tm $0 -- $*
|#

#lang racket

(require "mmcu/msp430/ops.rkt"
         "lib/racket-utils.rkt")

(provide main)

(define measure? (make-parameter #f))
(define synthesize? (make-parameter #f))
(define emulate? (make-parameter #f))

(define data-path (make-parameter "data/"))
(define output-file (make-parameter "emu/msp430/synthesized.rkt"))
(define racket62 (make-parameter 'racket62-assumed))

(define op-list (make-parameter all-ops))

(define (run-measure)
  (define p (dynamic-place "meas/run.rkt" 'place-main))
  (place-channel-put p `(run #:data-prefix ,(data-path) 
                             #:nprocs 1 
                             #:nsamples 64
                             #:op-list (quote ,(op-list))))
  (place-wait p))

(define (run-synthesis)
  (when (file-exists? (output-file)) 
    (rename-file-or-directory 
      (output-file)
      (string-append (output-file) ".old")))

  (define p (dynamic-place "synth/run.rkt" 'place-main))
  (place-channel-put p `(run #:racket62 ,(racket62)
                             #:data-prefix ,(data-path)
                             #:out-file (open-output-file ,(output-file))
                             #:op-list (quote ,(op-list))))
  (place-wait p))

(define (run-emulator)
  (define p (dynamic-place "emu/run.rkt" 'place-main))
  (place-channel-put p `(run #:interactive-mode #t))
  (place-wait p))

(define (main . args)
  (command-line
    #:once-each
    [("-m" "--measure") "If present, perform measurement using hardware connected over a debug interface."
              (measure? #t)]
    [("-s" "--synthesize") "If present, perform synthesis using data files generated during measurement."
              (synthesize? #t)]
    [("-e" "--emulate") "If present, launch the emulator at the end of synthesis."
              (emulate? #t)]
    [("-d" "--data-path") d "Alternate path for data files generated during measurement (default: data/)"
           (data-path d)]
    [("-o" "--output-file") o "Alternate path for synthesis results (default: emu/msp430/synthesized.rkt)"
           (output-file o)]
    [("--ops") ops "List of operations to measure/synthesize. Default is all ops."
           (op-list (sread ops))]
    [("-r" "--racket-6.2-path") racketpath "Path to a racket 6.2 `racket` executable."
           (racket62 racketpath)])
  
  (when (measure?) (run-measure))
  (when (synthesize?) (run-synthesis))
  (when (emulate?) (run-emulator)))
