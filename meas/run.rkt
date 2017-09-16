#!/bin/sh
#|
exec racket -tm $0 -- "$@"
|#

#lang racket

(provide main place-main)

(require racket/cmdline
         "../lib/racket-utils.rkt"
         "../mmcu/msp430/ops.rkt"
         "measure.rkt")

(define data-prefix "data/")
(define nsamples 65536)
(define nprocs 1)
(define ops all-ops)

(define (main . args)
  (command-line
    #:once-each
    [("-d" "--data-path") datapath "Path where the collected data should be stored (e.g. data/)"
                          (set! data-prefix datapath)]
    [("-n" "--num-samples") n "Number of samples to take (only for non-thorough sampling)"
                          (set! nsamples (sread n))]
    [("-j" "--num-procs") j "Number of processes to run for data collection"
                          (set! nprocs (sread j))]
    [("-o" "--ops") o "Operations to synthesize. Omit for all ops"
                          (set! ops (sread o))]
    #:args rest
    (void))

  (run #:data-prefix data-prefix 
       #:nsamples nsamples 
       #:nprocs nprocs 
       #:op-list ops))

(define-namespace-anchor meas)
(define (place-main ch)
  (eval (place-channel-get ch) (namespace-anchor->namespace meas)))
