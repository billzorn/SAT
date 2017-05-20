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

; simple ops (likely defined by 1-3 instruction bv programs)

; sample some number of values from the data table and hope that the resulting assertions
; are sufficient to more or less uniquely define the operation

(define (iotab-sample->post samples arity result-ind)
  (define (post P inputs)
    (for* ([sample (in-vector samples)])
      (let* ([inputs (take sample arity)]
             [x (list-ref sample result-ind)]
             [assertion (= (bitwise-and (interpret P inputs) #xff) x)])
        (assert assertion))))
  post)

(define-syntax (define-sample-post stx)
  (syntax-case stx ()
    [(_ iotab type value)
     (with-syntax ([name (format-id #'iotab #:source #'iotab "~a-sample-~a/post" (syntax-e #'iotab) (syntax-e #'type))])
       #'(define name value))]))

; defines the following values (given e.g. an iotab called add.b):
;  - add.b-sample-val/post
;  - add.b-sample-c/post
;  - add.b-sample-z/post
;  - add.b-sample-v/post
;  - add.b-sample-f/post
(define-syntax-rule (define-iotab-post/sample iotab)
  (begin (define samples (iotab-fmt1-sample iotab 512))
    (define-sample-post iotab val (iotab-sample->post samples 3 3))
    (define-sample-post iotab c (iotab-sample->post samples 4 4))
    (define-sample-post iotab z (iotab-sample->post samples 4 5))
    (define-sample-post iotab n (iotab-sample->post samples 4 6))
    (define-sample-post iotab v (iotab-sample->post samples 4 7))))

; Note: the postconditions for the flags have an arity of 4, because they can
; also use the result of the computation as part of the computation of the flag
; value.

; these should find a valid result in (less than) 4 instructions for most values
; and flags. notable exceptions: overflow flag for add.b
  
(define-iotab-post/sample xor.b)
(define-iotab-post/sample add.b)
(define-iotab-post/sample sub.b)
(define-iotab-post/sample and.b)
(define-iotab-post/sample cmp.b)
(define-iotab-post/sample addc.b)
(define-iotab-post/sample subc.b)
(define-iotab-post/sample bic.b)
(define-iotab-post/sample bis.b)
(define-iotab-post/sample bit.b)

; these should probably _not_ find a valid result in 4 instructions

(define-iotab-post/sample dadd.b)

