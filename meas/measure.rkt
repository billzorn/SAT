#lang racket

(require racket/cmdline
         "../lib/racket-utils.rkt" 
         "measure-regops.rkt" 
         "process-measurements.rkt")

(define-syntax-rule (defopc/regop.b id opc)
  (define (id rsrc rdst) (list (bitwise-ior (bitwise-and rdst #xf) #x40)
                               (bitwise-ior (bitwise-and rsrc #xf) opc))))
(defopc/regop.b regopc/mov.b  #x40)
(defopc/regop.b regopc/add.b  #x50)
(defopc/regop.b regopc/addc.b #x60)
(defopc/regop.b regopc/subc.b #x70)
(defopc/regop.b regopc/sub.b  #x80)
(defopc/regop.b regopc/cmp.b  #x90)
(defopc/regop.b regopc/dadd.b #xa0)
(defopc/regop.b regopc/bit.b  #xb0)
(defopc/regop.b regopc/bic.b  #xc0)
(defopc/regop.b regopc/bis.b  #xd0)
(defopc/regop.b regopc/xor.b  #xe0)
(defopc/regop.b regopc/and.b  #xf0)

(define-syntax-rule (defopc/regop.w id opc)
  (define (id rsrc rdst) (list (bitwise-ior (bitwise-and rdst #xf) #x00)
                               (bitwise-ior (bitwise-and rsrc #xf) opc))))
(defopc/regop.w regopc/mov.w  #x40)
(defopc/regop.w regopc/add.w  #x50)
(defopc/regop.w regopc/addc.w #x60)
(defopc/regop.w regopc/subc.w #x70)
(defopc/regop.w regopc/sub.w  #x80)
(defopc/regop.w regopc/cmp.w  #x90)
(defopc/regop.w regopc/dadd.w #xa0)
(defopc/regop.w regopc/bit.w  #xb0)
(defopc/regop.w regopc/bic.w  #xc0)
(defopc/regop.w regopc/bis.w  #xd0)
(defopc/regop.w regopc/xor.w  #xe0)
(defopc/regop.w regopc/and.w  #xf0)

(define-syntax-rule (defopc/regop.a id opc)
  (define (id rsrc rdst) (list #x00 #x18
                               (bitwise-ior (bitwise-and rdst #xf) #x40)
                               (bitwise-ior (bitwise-and rsrc #xf) opc))))
(defopc/regop.a regopc/movx.a  #x40)
(defopc/regop.a regopc/addx.a  #x50)
(defopc/regop.a regopc/addcx.a #x60)
(defopc/regop.a regopc/subcx.a #x70)
(defopc/regop.a regopc/subx.a  #x80)
(defopc/regop.a regopc/cmpx.a  #x90)
(defopc/regop.a regopc/daddx.a #xa0)
(defopc/regop.a regopc/bitx.a  #xb0)
(defopc/regop.a regopc/bicx.a  #xc0)
(defopc/regop.a regopc/bisx.a  #xd0)
(defopc/regop.a regopc/xorx.a  #xe0)
(defopc/regop.a regopc/andx.a  #xf0)

(define (rmeasure-regop.b/par
         #:nprocs nprocs
         #:opc opc
         #:fname [fname (void)]
         #:rsrc [rsrc 4]
         #:rdst [rdst 5])
  (let ([opcodes (opc rsrc rdst)]
        [arguments (for*/list ([rsrc-v (in-range 256)]
                               [rdst-v (in-range 256)]
                               [c (in-range 2)]
                               [z (in-range 2)]
                               [n (in-range 2)]
                               [v (in-range 2)])
                     (list rsrc-v rdst-v (bitwise-ior c (arithmetic-shift z 1) (arithmetic-shift n 2)
                                                      (arithmetic-shift v 8))))]
        [reginit (build-list 64 (lambda (_) (random 256)))]
        [outfile (if (void? fname) (void) (open-output-file fname #:exists 'truncate))])
    
    (measurement-kernel-compile)
    (let ([data (measure-regops/par
                 #:nprocs nprocs
                 #:opcodes opcodes
                 #:arguments arguments
                 #:rsrc rsrc
                 #:rdst rdst
                 #:reginit reginit)])
      (if (void? fname)
          data
          (begin
            (write data outfile)
            (close-output-port outfile))))))

(define (rmeasure-regop/par
         #:nprocs nprocs
         #:opc opc
         #:n n
         #:mininput [mininput 0]
         #:maxinput maxinput
         #:fname [fname (void)]
         #:rsrc [rsrc 4]
         #:rdst [rdst 5])
  (let ([opcodes (opc rsrc rdst)]
        [arguments (for*/list ([i (in-range n)])
                     (let ([rsrc-v (if (= 0 mininput) (random maxinput) (random mininput maxinput))]
                           [rdst-v (if (= 0 mininput) (random maxinput) (random mininput maxinput))]
                           [c (random 2)]
                           [z (random 2)]
                           [n (random 2)]
                           [v (random 2)])
                     (list rsrc-v rdst-v (bitwise-ior c (arithmetic-shift z 1) (arithmetic-shift n 2)
                                                      (arithmetic-shift v 8)))))]
        [reginit (build-list 64 (lambda (_) (random 256)))]
        [outfile (if (void? fname) (void) (open-output-file fname #:exists 'truncate))])
    
    (measurement-kernel-compile)
    (let ([data (measure-regops/par
                 #:nprocs nprocs
                 #:opcodes opcodes
                 #:arguments arguments
                 #:rsrc rsrc
                 #:rdst rdst
                 #:reginit reginit)])
      (if (void? fname)
          data
          (begin
            (write data outfile)
            (close-output-port outfile))))))

(define-syntax-rule (defrelpath id)
  (define id (build-path (data-prefix) (symbol->string (quote id)))))
(define-syntax-rule (deftextpath id)
  (define id (build-path (data-prefix) (symbol->string (quote id)) ".txt")))
(defrelpath data/)
(defrelpath data/regops/)
(deftextpath data/regops/mov.b)
(deftextpath data/regops/add.b)
(deftextpath data/regops/addc.b)
(deftextpath data/regops/subc.b)
(deftextpath data/regops/sub.b)
(deftextpath data/regops/cmp.b)
(deftextpath data/regops/dadd.b)
(deftextpath data/regops/bit.b)
(deftextpath data/regops/bic.b)
(deftextpath data/regops/bis.b)
(deftextpath data/regops/xor.b)
(deftextpath data/regops/and.b)
(deftextpath data/regops/mov.w)
(deftextpath data/regops/add.w)
(deftextpath data/regops/addc.w)
(deftextpath data/regops/subc.w)
(deftextpath data/regops/sub.w)
(deftextpath data/regops/cmp.w)
(deftextpath data/regops/dadd.w)
(deftextpath data/regops/bit.w)
(deftextpath data/regops/bic.w)
(deftextpath data/regops/bis.w)
(deftextpath data/regops/xor.w)
(deftextpath data/regops/and.w)
(deftextpath data/regops/movx.a)
(deftextpath data/regops/addx.a)
(deftextpath data/regops/addcx.a)
(deftextpath data/regops/subcx.a)
(deftextpath data/regops/subx.a)
(deftextpath data/regops/cmpx.a)
(deftextpath data/regops/daddx.a)
(deftextpath data/regops/bitx.a)
(deftextpath data/regops/bicx.a)
(deftextpath data/regops/bisx.a)
(deftextpath data/regops/xorx.a)
(deftextpath data/regops/andx.a)

(define-syntax-rule (defiopath id)
  (define id (build-path (data-prefix) (symbol->string (quote id)) ".io")))
(defrelpath data/io/)
(defiopath data/io/mov.b)
(defiopath data/io/add.b)
(defiopath data/io/addc.b)
(defiopath data/io/subc.b)
(defiopath data/io/sub.b)
(defiopath data/io/cmp.b)
(defiopath data/io/dadd.b)
(defiopath data/io/bit.b)
(defiopath data/io/bic.b)
(defiopath data/io/bis.b)
(defiopath data/io/xor.b)
(defiopath data/io/and.b)
(defiopath data/io/mov.w)
(defiopath data/io/add.w)
(defiopath data/io/addc.w)
(defiopath data/io/subc.w)
(defiopath data/io/sub.w)
(defiopath data/io/cmp.w)
(defiopath data/io/dadd.w)
(defiopath data/io/bit.w)
(defiopath data/io/bic.w)
(defiopath data/io/bis.w)
(defiopath data/io/xor.w)
(defiopath data/io/and.w)
(defiopath data/io/movx.a)
(defiopath data/io/addx.a)
(defiopath data/io/addcx.a)
(defiopath data/io/subcx.a)
(defiopath data/io/subx.a)
(defiopath data/io/cmpx.a)
(defiopath data/io/daddx.a)
(defiopath data/io/bitx.a)
(defiopath data/io/bicx.a)
(defiopath data/io/bisx.a)
(defiopath data/io/xorx.a)
(defiopath data/io/andx.a)

(define regops.b
  (list
   (list 'mov.b  regopc/mov.b  data/regops/mov.b  data/io/mov.b)
   (list 'add.b  regopc/add.b  data/regops/add.b  data/io/add.b)
   (list 'addc.b regopc/addc.b data/regops/addc.b data/io/addc.b)
   (list 'subc.b regopc/subc.b data/regops/subc.b data/io/subc.b)
   (list 'sub.b  regopc/sub.b  data/regops/sub.b  data/io/sub.b)
   (list 'cmp.b  regopc/cmp.b  data/regops/cmp.b  data/io/cmp.b)
   (list 'dadd.b regopc/dadd.b data/regops/dadd.b data/io/dadd.b)
   (list 'bit.b  regopc/bit.b  data/regops/bit.b  data/io/bit.b)
   (list 'bic.b  regopc/bic.b  data/regops/bic.b  data/io/bic.b)
   (list 'bis.b  regopc/bis.b  data/regops/bis.b  data/io/bis.b)
   (list 'xor.b  regopc/xor.b  data/regops/xor.b  data/io/xor.b)
   (list 'and.b  regopc/and.b  data/regops/and.b  data/io/and.b)))
(define regops.w
  (list
   (list 'mov.w  regopc/mov.w  data/regops/mov.w  data/io/mov.w)
   (list 'add.w  regopc/add.w  data/regops/add.w  data/io/add.w)
   (list 'addc.w regopc/addc.w data/regops/addc.w data/io/addc.w)
   (list 'subc.w regopc/subc.w data/regops/subc.w data/io/subc.w)
   (list 'sub.w  regopc/sub.w  data/regops/sub.w  data/io/sub.w)
   (list 'cmp.w  regopc/cmp.w  data/regops/cmp.w  data/io/cmp.w)
   (list 'dadd.w regopc/dadd.w data/regops/dadd.w data/io/dadd.w)
   (list 'bit.w  regopc/bit.w  data/regops/bit.w  data/io/bit.w)
   (list 'bic.w  regopc/bic.w  data/regops/bic.w  data/io/bic.w)
   (list 'bis.w  regopc/bis.w  data/regops/bis.w  data/io/bis.w)
   (list 'xor.w  regopc/xor.w  data/regops/xor.w  data/io/xor.w)
   (list 'and.w  regopc/and.w  data/regops/and.w  data/io/and.w)))
(define regops.a
  (list
   (list 'movx.a  regopc/movx.a  data/regops/movx.a  data/io/movx.a)
   (list 'addx.a  regopc/addx.a  data/regops/addx.a  data/io/addx.a)
   (list 'addcx.a regopc/addcx.a data/regops/addcx.a data/io/addcx.a)
   (list 'subcx.a regopc/subcx.a data/regops/subcx.a data/io/subcx.a)
   (list 'subx.a  regopc/subx.a  data/regops/subx.a  data/io/subx.a)
   (list 'cmpx.a  regopc/cmpx.a  data/regops/cmpx.a  data/io/cmpx.a)
   (list 'daddx.a regopc/daddx.a data/regops/daddx.a data/io/daddx.a)
   (list 'bitx.a  regopc/bitx.a  data/regops/bitx.a  data/io/bitx.a)
   (list 'bicx.a  regopc/bicx.a  data/regops/bicx.a  data/io/bicx.a)
   (list 'bisx.a  regopc/bisx.a  data/regops/bisx.a  data/io/bisx.a)
   (list 'xorx.a  regopc/xorx.a  data/regops/xorx.a  data/io/xorx.a)
   (list 'andx.a  regopc/andx.a  data/regops/andx.a  data/io/andx.a)))


;; exhaustive over all 8-bit operands and flag values
(define (rmeasure-regop.b/all optable nprocs)
  (unless (directory-exists? data/) (make-directory data/))
  (unless (directory-exists? data/regops/) (make-directory data/regops/))

  (for ([fields optable])
    (let ([opc (second fields)]
          [path (third fields)])
      (rmeasure-regop.b/par
       #:nprocs nprocs
       #:opc opc
       #:fname path))))

;; n randomly chosen inputs
(define (rmeasure-regop/all optable n maxinput nprocs)
  (unless (directory-exists? data/) (make-directory data/))
  (unless (directory-exists? data/regops/) (make-directory data/regops/))

  (for ([fields optable])
    (let ([opc (second fields)]
          [path (third fields)])
      (rmeasure-regop/par
       #:nprocs nprocs
       #:opc opc
       #:n n
       #:maxinput maxinput
       #:fname path))))

(define (process-regop.b inpath outpath sym)
  (let* ([fin (open-input-file inpath)]
         [raw-data (read fin)]
         [data (io-diffs raw-data '(2 4 5) '(2 5))]
         [fout (open-output-file outpath)])
    (close-input-port fin)
    (for ([diff data])
      (fprintf fout "(~a . ~a)\n" (car diff) (cdr diff)))
    (close-output-port fout)))

(define (process-regop.b/all optable)
  (unless (directory-exists? data/io/) (make-directory data/io/))
  (for ([fields optable])
    (let ([inpath (third fields)]
          [outpath (fourth fields)]
          [sym (first fields)])
      (process-regop.b inpath outpath sym))))

(define (run #:data-prefix [datapath "data/"]
             #:nsamples [nsamples 65536]
             #:nprocs [nprocs 1]
             #:width [width 'a])
  (data-prefix datapath)
  (case width
    [('b 8)  (rmeasure-regop.b/all regops.b nprocs)]
    [('w 16) (rmeasure-regop/all regops.w nsamples 65536 nprocs)]
    [('a 20) (rmeasure-regop/all regops.a nsamples 2097152 nprocs)]
    [('all) 
      (rmeasure-regop.b/all regops.b nprocs) 
      (rmeasure-regop/all regops.w nsamples 65536 nprocs)
      (rmeasure-regop/all regops.a nsamples 2097152 nprocs)]))
