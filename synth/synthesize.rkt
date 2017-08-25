#lang racket

(require racket/cmdline)

(define racket62 (make-parameter "TODO"))
(define data-prefix (make-parameter "data/io/"))

(command-line
  #:once-each
  [("-r" "--racket-6.2-path") racketpath "Path to a racket 6.2 `racket` executable."
                         (racket62 racketpath)]
  [("-d" "--data-path") datapath "Path to the collected operation i/o data (e.g. data/io/"
                         (data-prefix datapath)]
  #:args rest
  (void))

(struct synthesis (iotab sp stdout stderr fallback) #:transparent)

(define (sread s)
  (define i (open-input-string s))
  (read i))

(define (sprintf fmt ...)
  (define o (open-output-string))
  (fprintf o fmt ...)
  (get-output-string o))

(define (data-file file)
  (string-append (data-prefix) file ".rkt"))

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
       (find-executable-path (racket62))
       "synapse/cpumodel/synthesize.rkt" 
       "--width" (number->string width)
       "--arity" (sprintf "~a" arity)
       "--index" (sprintf "~a" arity)
       "--threads" (number->string threads)
       "--strategy" strategy
       "--maxlength" (sprintf "~a" arity)
       (data-file iotab))])
   (close-output-port sp-stdin)
   (synthesis iotab sp sp-stdout sp-stderr fallback)))

(define (end-synthesis synth)
   (subprocess-wait (synthesis-sp synth))
   (let ([stdout-data (port->string (synthesis-stdout synth))]
         [stderr-data (port->string (synthesis-stderr synth))])
     (close-input-port (synthesis-stdout synth))
     (close-input-port (synthesis-stderr synth))
     (if (equal? stdout-data "")
       (begin (printf "Synthesis internal error: ~a\n" stderr-data) (synthesis-fallback synth))
       (let ([results (sread stdout-data)])
         (map (λ (result fallback)
                 (if (equal? result "#f") fallback result))
              results (synthesis-fallback synth))))))

(define (begin-operation-synthesis iotab #:width [width 8] #:maxlengths [maxlengths (list 4 4 4 4 4)] #:strategy [strategy "full"])
  (begin-synthesis iotab 
                   #:width width 
                   #:strategy strategy 
                   #:arity '(3 4 4 4 4)
                   #:index '(0 0 1 2 3)
                   #:maxlength maxlengths
                   #:fallback '("(mspx-bv 0)"
                                "(sr-carry sr)"
                                "(sr-zero sr)"
                                "(sr-negative sr)"
                                "(sr-overflow sr)")))

(define (end-operation-synthesis synth-handle)
  (define ops (end-synthesis synth-handle))
  (printf "(define (msp-~a sr op1 op2) ~a)\n" (synthesis-iotab synth-handle) (first ops))
  (printf "(define (msp-sr-~a sr op1 op2 dst) \n" (synthesis-iotab synth-handle))
  (printf "  (concat\n")
  (printf "    ~a\n" (second ops))
  (printf "    ~a\n" (third ops))
  (printf "    ~a\n" (fourth ops))
  (printf "    (bv 0 5)\n")
  (printf "    ~a))\n" (fifth ops)))

(define (widthshim tab)
  (printf 
"(define (msp-~a bw sr op1 op2)
  (case bw
    [(8) msp-~a.b sr op1 op2]
    [(16) msp-~a.w sr op1 op2]
    [else (mspx-bv 0)]))\n" tab tab tab)
  (printf 
"(define (msp-sr-~a bw sr op1 op2 dst)
  (case bw
    [(8) msp-sr-~a.b sr op1 op2 dst]
    [(16) msp-sr-~a.w sr op1 op2 dst]
    [else (mspx-bv 0)]))\n" tab tab tab))

(printf "#lang rosette\n")
(printf "(require \"../lib/bv-operations.rkt\")\n")
(printf "(provide (all-defined-out))\n\n")

(define handles (list
  (begin-operation-synthesis "mov.b"  #:width 8 #:maxlengths '(1 1 1 1 1))
  (begin-operation-synthesis "add.b"  #:width 8 #:maxlengths '(1 4 4 4 4))
  ;(begin-operation-synthesis (data-file "addc.b.rkt") #:width 8 #:maxlengths '(4 4 4 4 4))
  ;(begin-operation-synthesis (data-file "sub.b.rkt")  #:width 8 #:maxlengths '(1 4 4 4 4))
  ;(begin-operation-synthesis (data-file "subc.b.rkt") #:width 8 #:maxlengths '(4 4 4 4 4))
  ;(begin-operation-synthesis (data-file "cmp.b.rkt")  #:width 8 #:maxlengths '(1 4 4 4 4))
  ;(begin-operation-synthesis (data-file "bit.b.rkt")  #:width 8 #:maxlengths '(4 4 4 4 4))
  ;(begin-operation-synthesis (data-file "bic.b.rkt")  #:width 8 #:maxlengths '(4 4 4 4 4))
  ;(begin-operation-synthesis (data-file "bis.b.rkt")  #:width 8 #:maxlengths '(4 4 4 4 4))
  ;(begin-operation-synthesis (data-file "xor.b.rkt")  #:width 8 #:maxlengths '(1 4 4 4 4))
  ;(begin-operation-synthesis (data-file "and.b.rkt")  #:width 8 #:maxlengths '(1 4 4 4 4))
  ;(begin-operation-synthesis (data-file "dadd.b.rkt") #:width 8 #:maxlengths '(8 8 8 8 8) #:strategy 'n4-up))
  ;
  ;(begin-operation-synthesis (data-file "mov.w.rkt")  #:width 16 #:maxlengths '(1 1 1 1 1))
  ;(begin-operation-synthesis (data-file "add.w.rkt")  #:width 16 #:maxlengths '(1 4 4 4 4))
  ;(begin-operation-synthesis (data-file "addc.w.rkt") #:width 16 #:maxlengths '(4 4 4 4 4))
  ;(begin-operation-synthesis (data-file "sub.w.rkt")  #:width 16 #:maxlengths '(1 4 4 4 4))
  ;(begin-operation-synthesis (data-file "subc.w.rkt") #:width 16 #:maxlengths '(4 4 4 4 4))
  ;(begin-operation-synthesis (data-file "cmp.w.rkt")  #:width 16 #:maxlengths '(1 4 4 4 4))
  ;(begin-operation-synthesis (data-file "bit.w.rkt")  #:width 16 #:maxlengths '(4 4 4 4 4))
  ;(begin-operation-synthesis (data-file "bic.w.rkt")  #:width 16 #:maxlengths '(4 4 4 4 4))
  ;(begin-operation-synthesis (data-file "bis.w.rkt")  #:width 16 #:maxlengths '(4 4 4 4 4))
  ;(begin-operation-synthesis (data-file "xor.w.rkt")  #:width 16 #:maxlengths '(1 4 4 4 4))
  ;(begin-operation-synthesis (data-file "and.w.rkt")  #:width 16 #:maxlengths '(1 4 4 4 4))
  ;(begin-operation-synthesis (data-file "dadd.w.rkt") #:width 16 #:maxlengths '(8 8 8 8 8) #:strategy `n4-up)
  ))

(for ([handle handles])
  (end-operation-synthesis handle))

(widthshim "mov")
(widthshim "add")
(widthshim "addc")
(widthshim "sub")
(widthshim "subc")
(widthshim "cmp")
(widthshim "dadd")
(widthshim "bit")
(widthshim "bic")
(widthshim "bis")
(widthshim "xor")
(widthshim "and")

