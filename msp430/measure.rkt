#lang racket

(require "../lib/racket-utils.rkt" "measure-regops.rkt")



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

(define regops.b
  (vector
   (vector 'mov.b  regopc/mov.b  data/regops/mov.b)
   (vector 'add.b  regopc/add.b  data/regops/add.b)
   (vector 'addc.b regopc/addc.b data/regops/addc.b)
   (vector 'subc.b regopc/subc.b data/regops/subc.b)
   (vector 'sub.b  regopc/sub.b  data/regops/sub.b)
   (vector 'cmp.b  regopc/cmp.b  data/regops/cmp.b)
   (vector 'dadd.b regopc/dadd.b data/regops/dadd.b)
   (vector 'bit.b  regopc/bit.b  data/regops/bit.b)
   (vector 'bic.b  regopc/bic.b  data/regops/bic.b)
   (vector 'bis.b  regopc/bis.b  data/regops/bis.b)
   (vector 'xor.b  regopc/xor.b  data/regops/xor.b)
   (vector 'and.b  regopc/and.b  data/regops/and.b)))

(define (rmeasure-regop.b/all nprocs)
  (unless (directory-exists? data/) (make-directory data/))
  (unless (directory-exists? data/regops/) (make-directory data/regops/))

  (for ([fields regops.b])
    (let ([opc (vector-ref fields 1)]
          [path (vector-ref fields 2)])
      (rmeasure-regop.b/par
       #:nprocs nprocs
       #:opc opc
       #:fname path))))
