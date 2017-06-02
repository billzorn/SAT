#lang s-exp rosette ; Requires a Racket 6.2 / Rosette 1.0 environment

(require "synapse/opsyn/engine/search.rkt"
         "synapse/opsyn/bv/lang.rkt")

(require "data/iotabs.rkt"
         "msp430/process-measurements.rkt"
         "engine/framework-synth.rkt")

(provide (all-defined-out))

(define timeout 1800)
(define threads 2)

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
         [iotab-sample (case width
                    [(8) iotab-sample.b]
                    [(16) iotab-sample.w])]
    [post `(iotab-sample->post (quote ,iotab) #:arity ,arity #:index ,(+ arity index) #:width ,width)]
    [sat #t])
    (define (check)
      (set! sat #t)
      (let ([p (synthesize-op width pre post arity)])
        (if (equal? p #f) #f
          (for* ([c (in-range 2)]
                 [a (in-range (arithmetic-shift 1 width))]
                 [b (in-range (arithmetic-shift 1 width))])
            #:break (not sat)
            (define sample (iotab-sample (hash-ref iotabs iotab) c a b))
            (define result (list-ref sample 3))
            (define inputs (list c a b result))
            (define val (list-ref sample (+ arity index)))
            (set! sat (eq-under-width width (interpret p (take inputs arity)) val))
            (unless sat (begin (printf "Program ~v doesn't satisfy iotab for ~a, ~a (was ~a, should be ~a), adding ~a to sample set\n" p a b (interpret p (take inputs arity)) val inputs)
                               (iotab-add-sample iotab sample)))))
        (if sat p (check))))
    (parameterize ([current-bitwidth (+ width 1)])
      (check))))


(define-syntax-rule (synthesize/flags width tab)
  (let ([iotab (quote tab)])
    (hash-set! iotabs iotab tab)
    (iotab-generate-samples iotab #:width width)
    (printf "~a: ~v\n" (quote->string iotab) (synthesize-and-check iotab #:width width))
    (printf "~a/c: ~v\n" (quote->string iotab) (synthesize-and-check iotab #:width width #:arity 4 #:index 0))
    (printf "~a/z: ~v\n" (quote->string iotab) (synthesize-and-check iotab #:width width #:arity 4 #:index 1))
    (printf "~a/n: ~v\n" (quote->string iotab) (synthesize-and-check iotab #:width width #:arity 4 #:index 2))
    (printf "~a/v: ~v\n" (quote->string iotab) (synthesize-and-check iotab #:width width #:arity 4 #:index 3))))

(require "data/io/add.w.rkt")

(synthesize/flags 16 add.w)

;(synthesize/flags mov.b)
;(synthesize/flags add.b)
;(synthesize/flags addc.b)
;(synthesize/flags sub.b)
;(synthesize/flags subc.b)
;(synthesize/flags cmp.b)
;(synthesize/flags dadd.b)
;(synthesize/flags bit.b)
;(synthesize/flags bic.b)
;(synthesize/flags bis.b)
;(synthesize/flags xor.b)
;(synthesize/flags and.b)
