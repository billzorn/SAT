#lang s-exp rosette ; Requires a Racket 6.2 / Rosette 1.0 environment

(require (for-syntax racket/syntax))

(require "../synapse/opsyn/bv/lang.rkt"  
         "../synapse/opsyn/metasketches/superoptimization.rkt"  
         "../synapse/opsyn/metasketches/cost.rkt"
         "../synapse/benchmarks/msp430/regops.rkt")

(require "../msp430/process-measurements.rkt"
         "../data/iotabs.rkt")

(provide (all-defined-out))

(define (bvop.b-simple post arity
               #:finite? [finite? #t]
               #:maxlength [maxlength 3]
               #:cost-model [cost-model constant-cost-model]
               #:pre (pre void))
  (superopt∑ #:instructions bvops.b
             #:maxlength (if finite? maxlength +inf.0)
             #:arity arity
             #:pre   pre
             #:post  post
             #:cost-model cost-model))

(define bvops-cmp.b
  (list (bv 0) (bv 1) (bv 7) (bv 8) (bv #x80) (bv #x100)
        bvadd bvsub bvand bvor bvnot bvshl bvashr bvlshr 
        bvneg bvredor bvxor bvsle bvslt bveq bvule bvult))

(define bvops.b
  (list (bv 0) (bv 1) (bv 7) (bv 8) (bv #x80) (bv #x100)
        bvadd bvsub bvand bvor bvnot bvshl bvashr bvlshr 
        bvneg bvredor bvxor bveq))

(define bvops.w
  (list (bv 0) (bv 1) (bv 15) (bv 16) (bv #x8000) (bv #x10000)
        bvadd bvsub bvand bvor bvnot bvshl bvashr bvlshr 
        bvneg bvredor bvxor bveq))

(define (valid-inputs.b inputs)
  (assert (and (= (first inputs) (bitwise-and (first inputs) #x1))
               (= (second inputs) (bitwise-and (second inputs) #xff))
               (= (third inputs) (bitwise-and (third inputs) #xff)))))

(define (valid-inputs.w inputs)
  (assert (and (= (first inputs) (bitwise-and (first inputs) #x1))
               (= (second inputs) (bitwise-and (second inputs) #xffff))
               (= (third inputs) (bitwise-and (third inputs) #xffff)))))

(define iotabs (make-hash))
(define samples (make-hash))
(define sample/posts (make-hash))

; Turn an iotab sample into a postcondition function
(define (iotab-sample->post s)
  (λ (arity result-ind)
    (λ (P inputs)
      (for* ([sample (in-vector (hash-ref samples s))])
        (let* ([inputs (take sample arity)]
               [x (list-ref sample result-ind)]
               [assertion (= (bitwise-and (interpret P inputs) #xff) x)])
          (assert assertion))))))


(hash-set! iotabs `mov.b mov.b)
(hash-set! samples `mov.b (iotab-fmt1-sample mov.b 512))
(hash-set! sample/posts `mov.b (iotab-sample->post `mov.b))

(hash-set! iotabs `add.b add.b)
(hash-set! samples `add.b (iotab-fmt1-sample add.b 512))
(hash-set! sample/posts `add.b (iotab-sample->post `add.b))

(define addc.b-samples (iotab-fmt1-sample addc.b 512))
(define addc.b-sample/post (iotab-sample->post addc.b-samples))
(define sub.b-samples (iotab-fmt1-sample sub.b 512))
(define sub.b-sample/post (iotab-sample->post sub.b-samples))
(define subc.b-samples (iotab-fmt1-sample subc.b 512))
(define subc.b-sample/post (iotab-sample->post subc.b-samples))
(define cmp.b-samples (iotab-fmt1-sample cmp.b 512))
(define cmp.b-sample/post (iotab-sample->post cmp.b-samples))
(define dadd.b-samples (iotab-fmt1-sample dadd.b 512))
(define dadd.b-sample/post (iotab-sample->post dadd.b-samples))
(define bit.b-samples (iotab-fmt1-sample bit.b 512))
(define bit.b-sample/post (iotab-sample->post bit.b-samples))
(define bic.b-samples (iotab-fmt1-sample bic.b 512))
(define bic.b-sample/post (iotab-sample->post bic.b-samples))
(define bis.b-samples (iotab-fmt1-sample bis.b 512))
(define bis.b-sample/post (iotab-sample->post bis.b-samples))
(define xor.b-samples (iotab-fmt1-sample xor.b 512))
(define xor.b-sample/post (iotab-sample->post xor.b-samples))
(define and.b-samples (iotab-fmt1-sample and.b 512))
(define and.b-sample/post (iotab-sample->post and.b-samples))

