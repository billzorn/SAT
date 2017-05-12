#lang s-exp rosette

(require "../../synapse/opsyn/engine/search.rkt"
         "../../synapse/opsyn/bv/lang.rkt"  
         "../../synapse/opsyn/metasketches/superoptimization.rkt"  
         "../../synapse/opsyn/metasketches/cost.rkt")

(require "../data/iotabs.rkt"
         "../msp430/process-measurements.rkt"
         "../../synapse/benchmarks/msp430/regops.rkt")

(provide (all-defined-out))

(define timeout 1800)
(define threads 4)

(define-syntax-rule (synthesize-operation precond postcond arity)
  (search #:metasketch `(msp-simpleop postcond arity #:pre precond)
          #:threads threads
          #:timeout timeout
          #:bitwidth 8
          #:exchange-samples #t
          #:exchange-costs #t
          #:use-structure #t
          #:incremental #t
          #:widening #f
          #:synthesizer 'kodkod-incremental%
          #:verifier 'kodkod%
          #:verbose #t))

(synthesize-operation valid-inputs-bt dadd.b-sample-v/post 3)
