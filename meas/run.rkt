#lang racket

(require racket/cmdline
         "../lib/racket-utils.rkt"
         "measure.rkt")

(define data-prefix "data/")
(define nsamples 65536)
(define nprocs 1)
(define width 'a)

(command-line
  #:once-each
  [("-d" "--data-path") datapath "Path where the collected data should be stored (e.g. data/)"
                        (data-prefix datapath)]
  [("-n" "--num-samples") n "Number of samples to take (only for non-thorough sampling)"
                        (nsamples n)]
  [("-j" "--num-procs") j "Number of processes to run for data collection"
                        (nprocs j)]
  [("-w" "--width") w "Operation width to collect data for. 'b or 8, 'w or 16, or 'a or 20."
                        (width (sread w))]
  #:args rest
  (void))

(run #:data-prefix data-prefix 
     #:nsamples nsamples 
     #:nprocs nprocs 
     #:width width)
