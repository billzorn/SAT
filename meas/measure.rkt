#lang racket

(require racket/cmdline
         "../lib/racket-utils.rkt" 
         "measure-regops.rkt" 
         "process-measurements.rkt")

(provide run)

(define data-prefix (make-parameter "data/"))

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
  (define id (build-path (data-prefix) (string-append (symbol->string (quote id)) ".txt"))))
(defrelpath regops/)
(deftextpath regops/mov.b)
(deftextpath regops/add.b)
(deftextpath regops/addc.b)
(deftextpath regops/subc.b)
(deftextpath regops/sub.b)
(deftextpath regops/cmp.b)
(deftextpath regops/dadd.b)
(deftextpath regops/bit.b)
(deftextpath regops/bic.b)
(deftextpath regops/bis.b)
(deftextpath regops/xor.b)
(deftextpath regops/and.b)
(deftextpath regops/mov.w)
(deftextpath regops/add.w)
(deftextpath regops/addc.w)
(deftextpath regops/subc.w)
(deftextpath regops/sub.w)
(deftextpath regops/cmp.w)
(deftextpath regops/dadd.w)
(deftextpath regops/bit.w)
(deftextpath regops/bic.w)
(deftextpath regops/bis.w)
(deftextpath regops/xor.w)
(deftextpath regops/and.w)
(deftextpath regops/movx.a)
(deftextpath regops/addx.a)
(deftextpath regops/addcx.a)
(deftextpath regops/subcx.a)
(deftextpath regops/subx.a)
(deftextpath regops/cmpx.a)
(deftextpath regops/daddx.a)
(deftextpath regops/bitx.a)
(deftextpath regops/bicx.a)
(deftextpath regops/bisx.a)
(deftextpath regops/xorx.a)
(deftextpath regops/andx.a)

(define-syntax-rule (defiopath id)
  (define id (build-path (data-prefix) (symbol->string (quote id)) ".io")))
(defrelpath io/)
(defiopath io/mov.b)
(defiopath io/add.b)
(defiopath io/addc.b)
(defiopath io/subc.b)
(defiopath io/sub.b)
(defiopath io/cmp.b)
(defiopath io/dadd.b)
(defiopath io/bit.b)
(defiopath io/bic.b)
(defiopath io/bis.b)
(defiopath io/xor.b)
(defiopath io/and.b)
(defiopath io/mov.w)
(defiopath io/add.w)
(defiopath io/addc.w)
(defiopath io/subc.w)
(defiopath io/sub.w)
(defiopath io/cmp.w)
(defiopath io/dadd.w)
(defiopath io/bit.w)
(defiopath io/bic.w)
(defiopath io/bis.w)
(defiopath io/xor.w)
(defiopath io/and.w)
(defiopath io/movx.a)
(defiopath io/addx.a)
(defiopath io/addcx.a)
(defiopath io/subcx.a)
(defiopath io/subx.a)
(defiopath io/cmpx.a)
(defiopath io/daddx.a)
(defiopath io/bitx.a)
(defiopath io/bicx.a)
(defiopath io/bisx.a)
(defiopath io/xorx.a)
(defiopath io/andx.a)

(define regops.b
  (list
   (list 'mov.b  regopc/mov.b  regops/mov.b  io/mov.b)
   (list 'add.b  regopc/add.b  regops/add.b  io/add.b)
   (list 'addc.b regopc/addc.b regops/addc.b io/addc.b)
   (list 'subc.b regopc/subc.b regops/subc.b io/subc.b)
   (list 'sub.b  regopc/sub.b  regops/sub.b  io/sub.b)
   (list 'cmp.b  regopc/cmp.b  regops/cmp.b  io/cmp.b)
   (list 'dadd.b regopc/dadd.b regops/dadd.b io/dadd.b)
   (list 'bit.b  regopc/bit.b  regops/bit.b  io/bit.b)
   (list 'bic.b  regopc/bic.b  regops/bic.b  io/bic.b)
   (list 'bis.b  regopc/bis.b  regops/bis.b  io/bis.b)
   (list 'xor.b  regopc/xor.b  regops/xor.b  io/xor.b)
   (list 'and.b  regopc/and.b  regops/and.b  io/and.b)))
(define regops.w
  (list
   (list 'mov.w  regopc/mov.w  regops/mov.w  io/mov.w)
   (list 'add.w  regopc/add.w  regops/add.w  io/add.w)
   (list 'addc.w regopc/addc.w regops/addc.w io/addc.w)
   (list 'subc.w regopc/subc.w regops/subc.w io/subc.w)
   (list 'sub.w  regopc/sub.w  regops/sub.w  io/sub.w)
   (list 'cmp.w  regopc/cmp.w  regops/cmp.w  io/cmp.w)
   (list 'dadd.w regopc/dadd.w regops/dadd.w io/dadd.w)
   (list 'bit.w  regopc/bit.w  regops/bit.w  io/bit.w)
   (list 'bic.w  regopc/bic.w  regops/bic.w  io/bic.w)
   (list 'bis.w  regopc/bis.w  regops/bis.w  io/bis.w)
   (list 'xor.w  regopc/xor.w  regops/xor.w  io/xor.w)
   (list 'and.w  regopc/and.w  regops/and.w  io/and.w)))
(define regops.a
  (list
   (list 'movx.a  regopc/movx.a  regops/movx.a  io/movx.a)
   (list 'addx.a  regopc/addx.a  regops/addx.a  io/addx.a)
   (list 'addcx.a regopc/addcx.a regops/addcx.a io/addcx.a)
   (list 'subcx.a regopc/subcx.a regops/subcx.a io/subcx.a)
   (list 'subx.a  regopc/subx.a  regops/subx.a  io/subx.a)
   (list 'cmpx.a  regopc/cmpx.a  regops/cmpx.a  io/cmpx.a)
   (list 'daddx.a regopc/daddx.a regops/daddx.a io/daddx.a)
   (list 'bitx.a  regopc/bitx.a  regops/bitx.a  io/bitx.a)
   (list 'bicx.a  regopc/bicx.a  regops/bicx.a  io/bicx.a)
   (list 'bisx.a  regopc/bisx.a  regops/bisx.a  io/bisx.a)
   (list 'xorx.a  regopc/xorx.a  regops/xorx.a  io/xorx.a)
   (list 'andx.a  regopc/andx.a  regops/andx.a  io/andx.a)))


;; exhaustive over all 8-bit operands and flag values
(define (rmeasure-regop.b/all optable nprocs)
  (unless (directory-exists? (data-prefix)) (make-directory (data-prefix)))
  (unless (directory-exists? regops/) (make-directory regops/))

  (for ([fields optable])
    (let ([opc (second fields)]
          [path (third fields)])
      (rmeasure-regop.b/par
       #:nprocs nprocs
       #:opc opc
       #:fname path))))

;; n randomly chosen inputs
(define (rmeasure-regop/all optable n maxinput nprocs)
  (unless (directory-exists? (data-prefix)) (make-directory (data-prefix)))
  (unless (directory-exists? regops/) (make-directory regops/))

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
  (unless (directory-exists? (data-prefix)) (make-directory (data-prefix)))
  (unless (directory-exists? io/) (make-directory io/))
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
    [(b 8)  (rmeasure-regop.b/all regops.b nprocs)]
    [(w 16) (rmeasure-regop/all regops.w nsamples 65536 nprocs)]
    [(a 20) (rmeasure-regop/all regops.a nsamples 2097152 nprocs)]
    [(all) 
      (rmeasure-regop.b/all regops.b nprocs) 
      (rmeasure-regop/all regops.w nsamples 65536 nprocs)
      (rmeasure-regop/all regops.a nsamples 2097152 nprocs)]
    [else (error (sprintf "Invalid width ~a specified" width))]))
