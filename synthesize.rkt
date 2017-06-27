#lang s-exp rosette ; Requires a Racket 6.2 / Rosette 1.0 environment

(require "synapse/opsyn/engine/search.rkt"
         "synapse/opsyn/bv/lang.rkt")

(require "data/iotabs.rkt"
         "msp430/process-measurements.rkt"
         "engine/framework-synth.rkt")

(define timeout 1800)
(define threads 2)

; Notes:
; - there are two things which determine the parameters of a synthesis search:
;    1. The assertions used (n4 carry-left, n4 carry-right, n8/16 sample)
;    2. The parameters passed to superopt (maxlength [?], timeout, pre/post
;    conditions (see above)

(define-syntax-rule (synthesize-op width precond postcond arity)
  (let ([sketch (case width
                  [(8) `bvop.b-simple]
                  [(16) `bvop.w-simple])])
    (search #:metasketch `(,sketch ,postcond ,arity #:pre ,precond)
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
            #:verbose #f)))

(define (synthesize-and-check iotab
                     #:width [width 8]
                     #:arity [arity 3]
                     #:index [index 0])
  (let* ([pre (case width
                    [(8) `valid-inputs.b]
                    [(16) `valid-inputs.w])]
         [post `(iotab-sample->post (quote ,iotab) #:arity ,arity #:index ,(+ arity index) #:width ,width)]
         [iotab-sample (case width
                    [(8) iotab-sample.b]
                    [(16) iotab-sample.w])]
         [tab (hash-ref iotabs iotab)]
         [sat #t])
    (define (check)
      (set! sat #t)
      (let ([p (synthesize-op width pre post arity)])
        (if (equal? p #f) #f
          ; todo need a general way to iterate over possible inputs
          ;(for* ([c (in-range 2)]
          ;       [a (in-range (arithmetic-shift 1 width))]
          ;       [b (in-range (arithmetic-shift 1 width))])
          ;  #:break (not sat)
          ;  (define sample (iotab-sample tab c a b))
          (for ([kv (in-list (hash->list tab))])
            #:break (not sat)
            (define-values (key value) (values (first kv) (list-tail kv 1)))
            (define-values (c a b) (values (sr-carry (first key)) (second key) (third key)))
            (define sample (append key (iotab-entry-separate value)))
            (define result (list-ref sample 3))
            (define inputs (list c a b result))
            (define val (list-ref sample (+ arity index)))
            (set! sat (eq-under-width width (interpret p (take inputs arity)) val))
            (unless sat (begin ;(printf "Program ~v doesn't satisfy iotab for ~a, ~a (was ~a, should be ~a), adding ~a to sample set\n" p a b (interpret p (take inputs arity)) val inputs)
                               (iotab-add-sample iotab sample)))))
        (if sat p (check))))
   (parameterize ([current-bitwidth (+ width 1)])
      (check))))

(define (program->string p default)
  (if (false? p) 
    default
    (let* ([args '("(sr-carry sr)" "op1" "op2" "dst")]
           [arity (program-inputs p)]
           [statements (make-vector (+ arity (length (program-instructions p))))])
      (for ([i (in-range (vector-length statements))])
        (if (< i arity)
          (vector-set! statements i (list-ref args i))
          (let ([s (format "~v" (list-ref (program-instructions p) (- i arity)))])
            (for ([j (in-range i)])
              (set! s (string-replace s (format " ~a" j) (string-append " " (vector-ref statements j)))))
            (vector-set! statements i s))))
      (vector-ref statements (- (vector-length statements) 1)))))

(define-syntax-rule (synthesize/flags width tab)
  (let ([iotab (quote tab)])
    (hash-set! iotabs iotab tab)
    (iotab-generate-samples iotab #:width width #:nsamples 64)
    (printf "(define (msp-~a sr op1 op2) ~a)\n" (quote->string iotab) (program->string (synthesize-and-check iotab #:width width) "(mspx-bv 0)"))
    (printf "(define (msp-sr-~a sr op1 op2 dst) \n" (quote->string iotab))
    (printf "  (concat\n")
    (printf "    ~a\n" (program->string (synthesize-and-check iotab #:width width #:arity 4 #:index 0) "(sr-carry sr)"))
    (printf "    ~a\n" (program->string (synthesize-and-check iotab #:width width #:arity 4 #:index 1) "(sr-zero sr)"))
    (printf "    ~a\n" (program->string (synthesize-and-check iotab #:width width #:arity 4 #:index 2) "(sr-negative sr)"))
    (printf "    (bv 0 5)\n")
    (printf "    ~a))\n" (program->string (synthesize-and-check iotab #:width width #:arity 4 #:index 3) "(sr-overflow sr)"))))

(define (widthshim tab)
  (printf 
"(define (msp-~a bw sr op1 op2)
  (case bw
    [(8) msp-~a.b sr op1 op2]
    ;[(16) msp-~a.w sr op1 op2]
    [else (mspx-bv 0)]))\n" tab tab tab)
  (printf 
"(define (msp-sr-~a bw sr op1 op2 dst)
  (case bw
    [(8) msp-sr-~a.b sr op1 op2 dst]
    ;[(16) msp-sr-~a.w sr op1 op2 dst]
    [else (mspx-bv 0)]))\n" tab tab tab))

;(require "data/io/mov.w.rkt")
;(synthesize/flags 16 mov.w)
;(require "data/io/add.w.rkt")
;(synthesize/flags 16 add.w)
;(require "data/io/addc.w.rkt")
;(synthesize/flags 16 addc.w)
;(require "data/io/sub.w.rkt")
;(synthesize/flags 16 sub.w)
;(require "data/io/subc.w.rkt")
;;(synthesize/flags 16 subc.w)
;(require "data/io/cmp.w.rkt")
;(synthesize/flags 16 cmp.w)
;(require "data/io/bit.w.rkt")
;(synthesize/flags 16 bit.w)
;(require "data/io/bic.w.rkt")
;(synthesize/flags 16 bic.w)
;(require "data/io/bis.w.rkt")
;(synthesize/flags 16 bis.w)
(require "data/io/and.w.rkt")
(synthesize/flags 16 and.w)
(require "data/io/xor.w.rkt")
(synthesize/flags 16 xor.w)


;(printf "#lang rosette\n")
;(printf "(require \"../lib/bv-operations.rkt\")\n")
;(printf "(provide (all-defined-out))\n\n")
;
;(synthesize/flags 8 mov.b)
;(synthesize/flags 8 add.b)
;(synthesize/flags 8 addc.b)
;(synthesize/flags 8 sub.b)
;(synthesize/flags 8 subc.b)
;(synthesize/flags 8 cmp.b)
;(synthesize/flags 8 dadd.b)
;(synthesize/flags 8 bit.b)
;(synthesize/flags 8 bic.b)
;(synthesize/flags 8 bis.b)
;(synthesize/flags 8 xor.b)
;(synthesize/flags 8 and.b)
;
;(widthshim "mov")
;(widthshim "add")
;(widthshim "addc")
;(widthshim "sub")
;(widthshim "subc")
;(widthshim "cmp")
;(widthshim "dadd")
;(widthshim "bit")
;(widthshim "bic")
;(widthshim "bis")
;(widthshim "xor")
;(widthshim "and")

; unanswered questions:
; - how can we get it synthesize 8bit and 16bit ops with the same code? 
