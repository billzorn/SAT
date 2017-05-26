#lang s-exp rosette ; Requires a Racket 6.2 / Rosette 1.0 environment

(require "synapse/opsyn/engine/search.rkt"
         "synapse/opsyn/bv/lang.rkt")

(require "data/iotabs.rkt"
         "msp430/process-measurements.rkt"
         "engine/framework-synth.rkt")

(provide (all-defined-out))

(define timeout 1800)
(define threads 4)

; Notes:
; - there are two things which determine the parameters of a synthesis search:
;    1. The assertions used (n4 carry-left, n4 carry-right, n8/16 sample)
;    2. The parameters passed to superopt (maxlength [?], timeout, pre/post
;    conditions (see above)

(define-syntax-rule (synthesize-op width precond postcond arity)
  (search #:metasketch `(bvop.b-simple ,postcond ,arity #:pre ,precond)
          #:threads threads
          #:timeout timeout
          #:bitwidth (+ width 1)
          #:exchange-samples #t
          #:exchange-costs #t
          #:use-structure #t
          #:incremental #t
          #:widening #f
          #:synthesizer 'kodkod-incremental%
          #:verifier 'kodkod%
          #:verbose #f))

(define (synthesize-and-check iotab
                     #:width [width 8]
                     #:arity [arity 3]
                     #:index [index 0])
  (let* ([pre (case width
                    [(8) `valid-inputs.b]
                    [(16) `valid-inputs.w])]
    [post `((hash-ref sample/posts (quote ,iotab)) ,arity ,(+ arity index))]
    [sat #t])
    (define (check)
      (set! sat #t)
      (let ([p (synthesize-op width pre post arity)])
        (for* ([c (in-range 2)]
               [a (in-range (arithmetic-shift 1 width))]
               [b (in-range (arithmetic-shift 1 width))])
          #:break (not sat)
          (define sample (iotab-sample (hash-ref iotabs iotab) c a b))
          (define result (list-ref sample arity))
          (define inputs (list c a b result))
          (define val (list-ref sample (+ arity index)))
          (set! sat (= (bitwise-and (interpret p (take inputs arity)) #xff) val))
          (unless sat (begin (printf "Program ~a doesn't satisfy iotab for ~a, ~a (was ~a, should be ~a), adding ~a to sample set\n" p a b (interpret p (take inputs arity)) val inputs)
                             (hash-set! samples iotab (vector-append (hash-ref samples iotab) (vector sample))))))
        (if sat p (check))))
    (parameterize ([current-bitwidth (+ width 1)])
      (check))))

; Bug (?): even though samples are added to the sample table, it continues to
; generate programs that don't work for the same arguments...

(define msp430-add.b (synthesize-and-check `add.b))
(printf "add.b: ~a\n" msp430-add.b)
(printf "samples: ~a\n" (hash-ref samples `add.b))
(define msp430-add.b/c (synthesize-and-check `add.b #:arity 4 #:index 0))
(printf "add.b/c: ~a\n" msp430-add.b/c)
(printf "samples: ~a\n" (hash-ref samples `add.b))
(define msp430-add.b/z (synthesize-and-check `add.b #:arity 4 #:index 1))
(printf "add.b/z: ~a\n" msp430-add.b/z)
(printf "samples: ~a\n" (hash-ref samples `add.b))
(define msp430-add.b/n (synthesize-and-check `add.b #:arity 4 #:index 2))
(printf "add.b/n: ~a\n" msp430-add.b/n)
(define msp430-add.b/v (synthesize-and-check `add.b #:arity 4 #:index 3))
(printf "add.b/v: ~a\n" msp430-add.b/v)

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
