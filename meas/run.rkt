#!/bin/sh
#|
exec racket -tm $0 -- $*
|#

#lang racket

(provide main place-main)

(require racket/cmdline
         "../lib/racket-utils.rkt"
         "measure.rkt")

(define data-prefix "data/")
(define nsamples 65536)
(define nprocs 1)
(define width 'a)

(define (main . args)
  (printf "running main with args ~a\n" (current-command-line-arguments))
  (command-line
    #:once-each
    [("-d" "--data-path") datapath "Path where the collected data should be stored (e.g. data/)"
                          (set! data-prefix datapath)]
    [("-n" "--num-samples") n "Number of samples to take (only for non-thorough sampling)"
                          (set! nsamples (sread n))]
    [("-j" "--num-procs") j "Number of processes to run for data collection"
                          (set! nprocs (sread j))]
    [("-w" "--width") w "Operation width to collect data for. 'b or 8, 'w or 16, or 'a or 20."
                          (set! width (sread w))]
    #:args rest
    (void))
  
  (run #:data-prefix data-prefix 
       #:nsamples nsamples 
       #:nprocs nprocs 
       #:width width))

(define-namespace-anchor meas)
(define (place-main ch)
  (eval (place-channel-get ch) (namespace-anchor->namespace meas)))
