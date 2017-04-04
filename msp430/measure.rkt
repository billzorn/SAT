#lang racket

(require "../lib/racket-utils.rkt" "measure-regops.rkt" "process-measurements.rkt")



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
        [outfile (if (void? fname) (void) (open-output-file fname))])
    
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

(define here (get-here))
(define-syntax-rule (defrelpath id)
  (define id (build-path here (string-append "../" (symbol->string (quote id))))))
(define-syntax-rule (deftextpath id)
  (define id (build-path here (string-append "../" (symbol->string (quote id)) ".txt"))))
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
(define-syntax-rule (defrktpath id)
  (define id (build-path here (string-append "../" (symbol->string (quote id)) ".rkt"))))
(defrelpath data/io/)
(defrktpath data/io/mov.b)
(defrktpath data/io/add.b)
(defrktpath data/io/addc.b)
(defrktpath data/io/subc.b)
(defrktpath data/io/sub.b)
(defrktpath data/io/cmp.b)
(defrktpath data/io/dadd.b)
(defrktpath data/io/bit.b)
(defrktpath data/io/bic.b)
(defrktpath data/io/bis.b)
(defrktpath data/io/xor.b)
(defrktpath data/io/and.b)

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

(define (rmeasure-regop.b/all nprocs)
  (unless (directory-exists? data/) (make-directory data/))
  (unless (directory-exists? data/regops/) (make-directory data/regops/))

  (for ([fields regops.b])
    (let ([opc (second fields)]
          [path (third fields)])
      (rmeasure-regop.b/par
       #:nprocs nprocs
       #:opc opc
       #:fname path))))

(define (process-regop inpath outpath sym)
  (let* ([fin (open-input-file inpath)]
         [raw-data (read fin)]
         [data (iotab-fmt1/sr (io-diffs raw-data '(2 4 5) '(2 5)))]
         [fout (open-output-file outpath)])
    (close-input-port fin)
    (fprintf fout
             "#lang racket~n(provide ~a)~n(define ~a~n~v)~n"
             sym   ; (provide sym)
             sym   ; (define sym
             data) ; data...)
    (close-output-port fout)))

(define (process-regop.b/all)
  (unless (directory-exists? data/io/) (make-directory data/io/))
  (for ([fields regops.b])
    (let ([inpath (third fields)]
          [outpath (fourth fields)]
          [sym (first fields)])
      (process-regop inpath outpath sym))))
