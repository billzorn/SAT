#lang racket

(require racket/cmdline)

(define racket62 (make-parameter "TODO"))

(command-line
  #:once-each
  [("--racket-6.2-path") racketpath ("Path to a racket 6.2 `racket` executable.")
                         (racket62 racketpath)]
  #:args rest
  (void))

(struct synthesis (sp stdout stderr fallback) #:transparent)

(define (begin-synthesis iotab 
                         #:width [width 8] 
                         #:arity [arity 3] 
                         #:index [index 0] 
                         #:maxlength [maxlength 4]
                         #:threads [threads 4] 
                         #:fallback [fallback "#f"])
  (let*-values
   ([(sp sp-stdout sp-stdin sp-stderr) 
     (subprocess #f #f #f 
       (find-executable-path (racket62))
       "synapse/cpumodel/synthesize.rkt" 
       "--width" (number->string width)
       "--arity" (number->string arity)
       "--index" (number->string index)
       "--threads" (number->string threads)
       "--maxlength" (number->string maxlength)
       iotab)])
   (close-output-port sp-stdin)
   (synthesis sp sp-stdout sp-stderr fallback)))

(define (end-synthesis synth)
   (subprocess-wait (synthesis-sp synth))
   (let ([stdout-data (port->string (synthesis-stdout synth))]
         [stderr-data (port->string (synthesis-stderr synth))])
     (close-input-port (synthesis-stdout synth))
     (close-input-port (synthesis-stderr synth))
     (cond
       [(equal? stdout-data "") (printf "Synthesis internal error: ~a\n" stderr-data) (synthesis-fallback synth)]
       [(equal? stdout-data "#f") (synthesis-fallback synth)]
       [else stdout-data])))

(define (begin-operation-synthesis iotab #:width [width 8] #:maxlengths [maxlengths (list 4 4 4 4 4)])
  (list
    (begin-synthesis iotab #:width width #:maxlength (first maxlengths) #:fallback "(mspx-bv 0) ; no soln found")
    (begin-synthesis iotab #:width width #:arity 4 #:index 0 #:maxlength (second maxlengths) #:fallback "(sr-carry sr) ; no soln found")
    (begin-synthesis iotab #:width width #:arity 4 #:index 1 #:maxlength (third maxlengths) #:fallback "(sr-zero sr) ; no soln found")
    (begin-synthesis iotab #:width width #:arity 4 #:index 2 #:maxlength (fourth maxlengths) #:fallback "(sr-negative sr) ; no soln found")
    (begin-synthesis iotab #:width width #:arity 4 #:index 3 #:maxlength (fifth maxlengths) #:fallback "(sr-overflow sr) ; no soln found")))

(define (end-operation-synthesis synth-handles)
  (printf "(define (msp-~a sr op1 op2) ~a)\n" (end-synthesis (first synth-handles)))
  (printf "(define (msp-sr-~a sr op1 op2 dst) \n")
  (printf "  (concat\n")
  (printf "    ~a\n" (end-synthesis (second synth-handles)))
  (printf "    ~a\n" (end-synthesis (third synth-handles)))
  (printf "    ~a\n" (end-synthesis (fourth synth-handles)))
  (printf "    (bv 0 5)\n")
  (printf "    ~a))\n" (end-synthesis (fifth synth-handles))))

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

(define synthesis-handles
  (list
    (begin-operation-synthesis "synth/data/test/mov.b.rkt"  #:width 8 #:maxlengths '(1 1 1 1 1))
    (begin-operation-synthesis "synth/data/test/add.b.rkt"  #:width 8 #:maxlengths '(1 4 4 4 4))
    (begin-operation-synthesis "synth/data/test/addc.b.rkt" #:width 8 #:maxlengths '(4 4 4 4 4))
    (begin-operation-synthesis "synth/data/test/sub.b.rkt"  #:width 8 #:maxlengths '(1 4 4 4 4))
    (begin-operation-synthesis "synth/data/test/subc.b.rkt" #:width 8 #:maxlengths '(4 4 4 4 4))
    (begin-operation-synthesis "synth/data/test/cmp.b.rkt"  #:width 8 #:maxlengths '(1 4 4 4 4))
    (begin-operation-synthesis "synth/data/test/bit.b.rkt"  #:width 8 #:maxlengths '(4 4 4 4 4))
    (begin-operation-synthesis "synth/data/test/bic.b.rkt"  #:width 8 #:maxlengths '(4 4 4 4 4))
    (begin-operation-synthesis "synth/data/test/bis.b.rkt"  #:width 8 #:maxlengths '(4 4 4 4 4))
    (begin-operation-synthesis "synth/data/test/xor.b.rkt"  #:width 8 #:maxlengths '(1 4 4 4 4))
    (begin-operation-synthesis "synth/data/test/and.b.rkt"  #:width 8 #:maxlengths '(1 4 4 4 4))
    
    (begin-operation-synthesis "synth/data/test/mov.w.rkt"  #:width 16 #:maxlengths '(1 1 1 1 1))
    (begin-operation-synthesis "synth/data/test/add.w.rkt"  #:width 16 #:maxlengths '(1 4 4 4 4))
    (begin-operation-synthesis "synth/data/test/addc.w.rkt" #:width 16 #:maxlengths '(4 4 4 4 4))
    (begin-operation-synthesis "synth/data/test/sub.w.rkt"  #:width 16 #:maxlengths '(1 4 4 4 4))
    (begin-operation-synthesis "synth/data/test/subc.w.rkt" #:width 16 #:maxlengths '(4 4 4 4 4))
    (begin-operation-synthesis "synth/data/test/cmp.w.rkt"  #:width 16 #:maxlengths '(1 4 4 4 4))
    (begin-operation-synthesis "synth/data/test/bit.w.rkt"  #:width 16 #:maxlengths '(4 4 4 4 4))
    (begin-operation-synthesis "synth/data/test/bic.w.rkt"  #:width 16 #:maxlengths '(4 4 4 4 4))
    (begin-operation-synthesis "synth/data/test/bis.w.rkt"  #:width 16 #:maxlengths '(4 4 4 4 4))
    (begin-operation-synthesis "synth/data/test/xor.w.rkt"  #:width 16 #:maxlengths '(1 4 4 4 4))
    (begin-operation-synthesis "synth/data/test/and.w.rkt"  #:width 16 #:maxlengths '(1 4 4 4 4))
    (begin-operation-synthesis "synth/data/test/dadd.w.rkt" #:width 16 #:maxlengths '(8 8 8 8 8))))

(for* ([handles (in-list synthesis-handles)])
  (end-operation-synthesis handles))

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

