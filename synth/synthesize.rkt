#lang racket

(require racket/cmdline
         "../mmcu/msp430/hints.rkt")

(define racket62 (make-parameter (build-path (find-system-path 'home-dir) "racket-6.2/bin/racket")))
(define data-prefix (make-parameter "data/"))
(define out-file (make-parameter (current-output-port)))
(define op-list (make-parameter '(mov.b mov.w add.b add.w addc.b addc.w sub.b sub.w cmp.b cmp.w dadd.b dadd.w bit.b bit.w bic.b bic.w bis.b bis.w xor.b xor.w and.b and.w)))

(define (sread s)
  (define i (open-input-string s))
  (read i))

(define (sprintf fmt ...)
  (define o (open-output-string))
  (fprintf o fmt ...)
  (get-output-string o))

(command-line
  #:once-each
  [("-r" "--racket-6.2-path") racketpath "Path to a racket 6.2 `racket` executable."
                         (racket62 racketpath)]
  [("-d" "--data-path") datapath "Path to the collected data (e.g. data/)"
                         (data-prefix datapath)]
  [("-o" "--output-file") outfile "Path to the output file"
                         (out-file (open-output-file outfile))]
  [("--ops") ops "List of operations to synthesize"
                         (op-list (sread ops))]
  #:args rest
  (void))

(struct synthesis (iotab sp stdout stderr fallback strategy) #:transparent)

(define (data-file file)
  (build-path (data-prefix) "io/" file))

(define (begin-synthesis iotab 
                         #:width [width 8] 
                         #:arity [arity 3] 
                         #:index [index 0] 
                         #:maxlength [maxlength 4]
                         #:threads [threads 4] 
                         #:strategy [strategy "full"] 
                         #:fallback [fallback "#f"])
  (let*-values
   ([(sp sp-stdout sp-stdin sp-stderr) 
     (subprocess #f #f #f 
       (racket62)
       "synapse/cpumodel/synthesize.rkt" 
       "--width" (sprintf "~a" width)
       "--arity" (sprintf "~a" arity)
       "--index" (sprintf "~a" index)
       "--threads" (number->string threads)
       "--strategy" (sprintf "~a" strategy)
       "--maxlength" (sprintf "~a" maxlength)
       (data-file iotab))])
   (close-output-port sp-stdin)
   (synthesis iotab sp sp-stdout sp-stderr fallback (first strategy))))

(define (condition-result r)
  (string-replace r "carry-in" "(sr-carry sr)"))

(define (end-synthesis synth)
   (subprocess-wait (synthesis-sp synth))
   (let ([stdout-data (port->string (synthesis-stdout synth))]
         [stderr-data (port->string (synthesis-stderr synth))])
     (close-input-port (synthesis-stdout synth))
     (close-input-port (synthesis-stderr synth))
     (unless (equal? stderr-data "")
       (fprintf (current-error-port) "Synthesis internal error: ~a\n" stderr-data))
     (if (equal? stdout-data "")
       (synthesis-fallback synth)
       (let ([results (sread stdout-data)])
         ;(printf "[~a]\n" results)
         (map (λ (result fallback)
                 (if (equal? result "#f") fallback (condition-result result)))
              results (synthesis-fallback synth))))))

(define (begin-operation-synthesis iotab #:width [width 8] #:maxlengths [maxlengths (list 4 4 4 4 4)] #:strategy [strategy "full"])
  (begin-synthesis iotab 
                   #:width width 
                   #:strategy strategy 
                   #:arity '(3 4 4 4 4)
                   #:index '(0 0 1 2 3)
                   #:maxlength maxlengths
                   #:fallback '("(bv 0 20)"
                                "(sr-carry sr)"
                                "(sr-zero sr)"
                                "(sr-negative sr)"
                                "(sr-overflow sr)")))

(define (end-operation-synthesis synth-handle)
  (define ops (end-synthesis synth-handle))
  (define iotab (synthesis-iotab synth-handle))
  (define strategy (synthesis-strategy synth-handle))
  (case strategy
    [(full)
       (fprintf (out-file) #<<END
(define (msp-~a sr op1 op2) ~a)
(define (msp-sr-~a sr op1 op2 dst)
  (concat
    ~a
    ~a
    ~a
    (bv 0 5)
    ~a))

END
          iotab (first ops)
          iotab (second ops) (third ops) (fourth ops) (fifth ops))]
    [(n4-up)
       (fprintf (out-file) #<<END
(define (msp-~a sr op1 op2) (n4-up.~a (lambda (sr op1 op2) ~a) (lambda (sr op1 op2 dst) ~a)))
(define (msp-sr-~a sr op1 op2 dst)
  (concat
    (n4-up.~a/c (lambda (sr op1 op2) ~a) (lambda (sr op1 op2 dst) ~a))
    ~a
    ~a
    (bv 0 5)
    ~a))

END
          iotab (second (string-split iotab "."))
          (first ops) (second ops)
          iotab (second (string-split iotab "."))
          (first ops) (second ops) (third ops) (fourth ops) (fifth ops))]
    [else (error (sprintf "Invalid synthesis strategy ~a" strategy))]))

(define (synthesize-operation op)
  (end-operation-synthesis 
    (begin-operation-synthesis op #:width (bitwidth-hint op) #:maxlengths (maxlength-hint op) #:strategy (strategy-hint op))))

(define (quote->string q)
  (let ([o (open-output-string)])
    (display q o)
    (get-output-string o)))

(define (synthesize-operations ops)
  (let ([handles 
         (for/list ([op (in-list (map quote->string ops))]) 
           (begin-operation-synthesis op #:width (bitwidth-hint op) #:maxlengths (maxlength-hint op) #:strategy (strategy-hint op)))])
    (for ([handle (in-list handles)]) 
      (end-operation-synthesis handle))))

(define (widthshim tab)
  (fprintf (out-file)
"(define (msp-~a bw sr op1 op2)
  (case bw
    [(8) msp-~a.b sr op1 op2]
    [(16) msp-~a.w sr op1 op2]
    [else (mspx-bv 0)]))\n" tab tab tab)
  (fprintf (out-file)
"(define (msp-sr-~a bw sr op1 op2 dst)
  (case bw
    [(8) msp-sr-~a.b sr op1 op2 dst]
    [(16) msp-sr-~a.w sr op1 op2 dst]
    [else (mspx-bv 0)]))\n" tab tab tab))

(fprintf (out-file) "#lang rosette\n")
(fprintf (out-file) "(require \"../../lib/bv-operations.rkt\")\n")
(fprintf (out-file) "(provide (all-defined-out))\n\n")

(for ([op (in-list (op-list))])
  (synthesize-operations (list op)))

(close-output-port (out-file))
