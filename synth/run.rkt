#!/bin/sh
#|
exec racket -tm $0 -- "$@"
|#

#lang racket

(require racket/cmdline
         "../lib/racket-utils.rkt"
         "synthesize.rkt")

(provide main place-main)

(define all-ops '(mov.b mov.w add.b add.w addc.b addc.w sub.b sub.w subc.b subc.w cmp.b cmp.w dadd.b dadd.w bit.b bit.w bic.b bic.w bis.b bis.w xor.b xor.w and.b and.w))

(define racket62-assumed (build-path (find-system-path 'home-dir) "racket-6.2/bin/racket"))

(define (main . args)
  (define racket62 racket62-assumed)
  (define data-prefix "data/")
  (define out-file (current-output-port))
  (define op-list all-ops)
  
  (command-line
    #:once-each
    [("-r" "--racket-6.2-path") racketpath "Path to a racket 6.2 `racket` executable."
                           (set! racket62 racketpath)]
    [("-d" "--data-path") datapath "Path to the collected data (e.g. data/)"
                           (set! data-prefix datapath)]
    [("-o" "--output-file") outfile "Path to the output file"
                           (set! out-file (open-output-file outfile))]
    [("--ops") ops "List of operations to synthesize"
                            (printf "~a\n" ops)
                           (set! op-list (sread ops))]
    #:args rest
    (void))
  
  (run #:racket62 racket62
       #:data-prefix data-prefix
       #:out-file out-file
       #:op-list op-list))

(define-namespace-anchor synth)
(define (place-main ch)
  (eval (place-channel-get ch) (namespace-anchor->namespace synth)))
