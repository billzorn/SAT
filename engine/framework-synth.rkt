#lang s-exp rosette

(require "../synapse/opsyn/engine/search.rkt"
         "../synapse/opsyn/bv/lang.rkt"  
         "../synapse/opsyn/metasketches/superoptimization.rkt"  
         "../synapse/opsyn/metasketches/cost.rkt")

(require "../data/iotabs.rkt"
         "../msp430/process-measurements.rkt"
         "../synapse/benchmarks/msp430/regops.rkt")

(provide (all-defined-out))

(define timeout 1800)
(define threads 4)

(define-syntax-rule (synthesize-operation precond postcond arity)
  (search #:metasketch `(msp-simpleop postcond arity #:pre precond)
          #:threads threads
          #:timeout timeout
          #:bitwidth 9
          #:exchange-samples #t
          #:exchange-costs #t
          #:use-structure #t
          #:incremental #t
          #:widening #f
          #:synthesizer 'kodkod-incremental%
          #:verifier 'kodkod%
          #:verbose #t))

(displayln "add.b:")
(synthesize-operation valid-inputs-b16 add.b-sample-val/post 3)
(displayln " - flag c:")
(synthesize-operation valid-inputs-b16 add.b-sample-c/post 4)
(displayln " - flag z:")
(synthesize-operation valid-inputs-b16 add.b-sample-z/post 4)
(displayln " - flag n:")
(synthesize-operation valid-inputs-b16 add.b-sample-n/post 4)
(displayln " - flag v:")
(synthesize-operation valid-inputs-b16 add.b-sample-v/post 4)

;(displayln "xor.b:")
;(synthesize-operation valid-inputs-b16 xor.b-sample-val/post 3)
;(displayln " - flag c:")
;(synthesize-operation valid-inputs-b16 xor.b-sample-c/post 4)
;(displayln " - flag z:")
;(synthesize-operation valid-inputs-b16 xor.b-sample-z/post 4)
;(displayln " - flag n:")
;(synthesize-operation valid-inputs-b16 xor.b-sample-n/post 4)
;(displayln " - flag v:")
;(synthesize-operation valid-inputs-b16 xor.b-sample-v/post 4)
