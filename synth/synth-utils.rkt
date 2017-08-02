#lang s-exp rosette ; Requires a Racket 6.2 / Rosette 1.0 environment

(require (for-syntax racket/syntax))

(require "../synapse/opsyn/bv/lang.rkt"  
         "../synapse/opsyn/metasketches/superoptimization.rkt"  
         "../synapse/opsyn/metasketches/cost.rkt"
         "../synapse/benchmarks/msp430/regops.rkt")

(require "../meas/process-measurements.rkt"
         "data/iotabs.rkt")

(provide (all-defined-out))

(define (bvop.b-simple post arity
               #:finite? [finite? #t]
               #:maxlength [maxlength 4]
               #:cost-model [cost-model constant-cost-model]
               #:pre (pre void))
  (superopt∑ #:instructions bvops.b
             #:maxlength (if finite? maxlength +inf.0)
             #:arity arity
             #:pre   pre
             #:post  post
             #:cost-model cost-model))

(define (bvop.w-simple post arity
               #:finite? [finite? #t]
               #:maxlength [maxlength 4]
               #:cost-model [cost-model constant-cost-model]
               #:pre (pre void))
  (superopt∑ #:instructions bvops.w
             #:maxlength (if finite? maxlength +inf.0)
             #:arity arity
             #:pre   pre
             #:post  post
             #:cost-model cost-model))

(define bvops.b
  (list (bv 0) (bv 1) (bv #xf) bit8 bit9
        bvadd bvsub bvand bvor bvnot bvneg bvxor bveq
        pass eq0 samesign8 diffsign8 shr4 msp_dcarry))

(define bvops.w
  (list (bv 0) (bv 1) bit16 bit17
        bvadd bvsub bvand bvor bvnot bvneg bvxor bveq
        pass eq0 samesign16 diffsign16))

(define (valid-inputs.b inputs)
  (assert (and (= (first inputs) (bitwise-and (first inputs) #x1))
               (= (second inputs) (bitwise-and (second inputs) #xff))
               (= (third inputs) (bitwise-and (third inputs) #xff)))))
               ;(= (fourth inputs) (bitwise-and (fourth inputs) #xff)))))

(define (valid-inputs.w inputs)
  (assert (and (= (first inputs) (bitwise-and (first inputs) #x1))
               (= (second inputs) (bitwise-and (second inputs) #xffff))
               (= (third inputs) (bitwise-and (third inputs) #xffff)))))

(define (quote->string q)
  (define o (open-output-string))
  (write q o)
  (get-output-string o))

(define (iotab-samples.rkt iotab)
  (string-append (quote->string iotab) "-samples.rkt.tmp"))

(define iotabs (make-hash))

(define (eq-under-width width a b)
  (define mask (- (arithmetic-shift 1 width) 1))
  (= (bitwise-and a mask) (bitwise-and b mask)))

; Turn an iotab sample into a postcondition function
(define (iotab-sample->post s #:arity (arity 3) #:index (result-ind 0) #:width (width 8))
  (λ (P inputs)
    (let ([samples (with-input-from-file (iotab-samples.rkt s) read)])
      (for* ([sample (in-vector samples )])
        (let* ([inputs (take sample arity)]
               [x (list-ref sample result-ind)]
               [assertion (eq-under-width width (interpret P inputs) x)])
          (assert assertion))))))

(define (iotab-generate-samples iotab #:nsamples (nsamples 64) #:width (width 8))
  (let ([sample-fn (if (= width 8) iotab-fmt1.b-sample iotab-fmt1.w-sample)])
    (with-output-to-file #:exists 'replace
      (iotab-samples.rkt iotab)
      (thunk (write (sample-fn (hash-ref iotabs iotab) nsamples))))))

(define (iotab-add-sample iotab sample)
  (let ([samples (with-input-from-file (iotab-samples.rkt iotab) read)])
    (with-output-to-file #:exists 'replace
      (iotab-samples.rkt iotab) 
      (thunk (write (vector-append samples (vector sample)))))))


