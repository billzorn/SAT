#lang rosette/safe

(require rosette/lib/match "../lib/bv.rkt" "lang-base.rkt" "flags.rkt")

(provide (except-out (all-defined-out) define-load-syntax define-store-syntax))

; instruction set representation

; operands
(struct op () #:transparent)
(struct reg op (r) #:transparent)    ; "Rn"
(struct idx op (r i) #:transparent)  ; "X(Rn)"
(struct sym op (addr) #:transparent) ; "ADDR"
(struct abs op (addr) #:transparent) ; "&ADDR"
(struct cg op (i) #:transparent)     ; constant generator including "#1"
(struct ind op (r) #:transparent)    ; "@Rn"
(struct imm2 op (i) #:transparent)   ; "#@N"
(struct ai op (r) #:transparent)     ; "@Rn+"
(struct imm op (i) #:transparent)    ; "#N"

; executable instructions
(struct instruction () #:transparent)
(struct fmt1 instruction (op1 op2) #:transparent)
(struct fmt2 instruction (op1) #:transparent)
; For these two, 'regop' is an integer (the register number to work with)
; 'immop' is also an integer
(struct fmt2r instruction (regop) #:transparent)
(struct fmt2i instruction (immop regop) #:transparent)

(struct jump instruction (target) #:transparent)

; MSP430 instructions
(define-instruction
  [mov.w fmt1] [mov.b fmt1]
  [add.w fmt1] [add.b fmt1]
  [addc.w fmt1] [addc.b fmt1]
  [sub.w fmt1] [sub.b fmt1]
  [subc.w fmt1] [subc.b fmt1]
  [cmp.w fmt1] [cmp.b fmt1]
  [dadd.w fmt1] [dadd.b fmt1]
  [bit.w fmt1] [bit.b fmt1]
  [bic.w fmt1] [bic.b fmt1]
  [bis.w fmt1] [bis.b fmt1]
  [xor.w fmt1] [xor.b fmt1]
  [and.w fmt1] [and.b fmt1]

  [push.w fmt2] [push.b fmt2]
  [rrc.w fmt2] [rrc.b fmt2]
  [rra.w fmt2] [rra.b fmt2]
  [swpb fmt2]
  [sxt fmt2]
  [call fmt2]

  [reti instruction]
  [jmp jump]
  [jc jump] [jnc jump]
  [jz jump] [jnz jump]
  [jn jump] 
  [jge jump]
  [jl jump])

; MSP430x extended instructions
(define-instruction
  [movx.a fmt1] [movx.w fmt1] [movx.b fmt1]
  [addx.a fmt1] [addx.w fmt1] [addx.b fmt1]
  [addcx.a fmt1] [addcx.w fmt1] [addcx.b fmt1]
  [subx.a fmt1] [subx.w fmt1] [subx.b fmt1]
  [subcx.a fmt1] [subcx.w fmt1] [subcx.b fmt1]
  [cmpx.a fmt1] [cmpx.w fmt1] [cmpx.b fmt1]
  [daddx.a fmt1] [daddx.w fmt1] [daddx.b fmt1]
  [bitx.a fmt1] [bitx.w fmt1] [bitx.b fmt1]
  [bicx.a fmt1] [bicx.w fmt1] [bicx.b fmt1]
  [bisx.a fmt1] [bisx.w fmt1] [bisx.b fmt1]
  [xorx.a fmt1] [xorx.w fmt1] [xorx.b fmt1]
  [andx.a fmt1] [andx.w fmt1] [andx.b fmt1]

  [calla fmt2]
  [popm.a fmt2i] [popm.w fmt2i]
  [pushm.a fmt2i] [pushm.w fmt2i]
  [pushx.a fmt2] [pushx.w fmt2] [pushx.b fmt2]
  [rrcm.a fmt2i] [rrcm.w fmt2i]
  [rrum.a fmt2i] [rrum.w fmt2i]
  [rram.a fmt2i] [rram.w fmt2i]
  [rlam.a fmt2i] [rlam.w fmt2i]
  [rrcx.a fmt2] [rrcx.w fmt2] [rrcx.b fmt2]
  [rrux.a fmt2r] [rrux.w fmt2r] [rrux.b fmt2r]
  [rrax.a fmt2] [rrax.w fmt2] [rrax.b fmt2]
  [swpbx.a fmt2] [swpbx.w fmt2]
  [sxtx.a fmt2] [sxtx.w fmt2])


; representation of complete state
; instrs is a vector of instructions representing the program.
(struct state (instrs r m running) #:transparent)

; Some magic number constants
; Register alternate names
(define REG/PC 0)
(define REG/SP 1)
(define REG/SR 2)
(define REG/CG 3)
(define REG/CG1 2)
(define REG/CG2 3)

; Memory Accuracy TODOs:
; (mostly awaiting HW-accurate memory model)
; - PC (r0) needs to be 2-word aligned, AI by 2 only
; - Writes to r2 should be dropped

; master load macro
(define-syntax (define-load-syntax stx)
  (syntax-case stx ()
    [(_ [id register-refx register-setx! memory-refx truncx width])
     #'(begin (define-syntax-rule (id op registers memory)
                (match op
                  [(reg r)    (register-refx registers r)]

                  [(cg i)     (truncx i)]
                  [(imm i)    (truncx i)]
                  [(imm2 i)   (truncx i)]

                  [(abs addr) (memory-refx memory addr)]
                  ; sym treated like abs (difference handled by decoder)
                  [(sym addr) (memory-refx memory addr)]

                  [(ind r)    (memory-refx memory (register-ref registers r))]
                  ; Same as above but step needs to also inc val referenced by r
                  [(ai r)     (begin0 (memory-refx memory (register-ref registers r)) (register-set! registers r (bvadd (register-ref registers r) width)))]

                  [(idx r i)  (memory-refx memory (bvadd (register-ref registers r) i))])))]
    [(_ [id register-refx register-setx! memory-refx truncx width] more ...)
     #'(begin
         (define-load-syntax [id register-refx register-setx! memory-refx truncx width])
         (define-load-syntax more ...))]))

; define 16-bit and 8-bit load macros
(define-load-syntax
  [load20 register-ref register-set! memory-ref20 trunc20 (mspx-bv 4)]
  [load16 register-ref16 register-set16! memory-ref16 trunc16 (mspx-bv 2)]
  [load8 register-ref8 register-set8! memory-ref8 trunc8 (mspx-bv 1)])

; master store macro
(define-syntax (define-store-syntax stx)
  (syntax-case stx ()
    [(_ [id register-setx! memory-setx!])
     #'(begin (define-syntax-rule (id x op registers memory)
                (match op
                  [(reg r)    (register-setx! registers r x)]
                  [(abs addr) (memory-setx! memory addr x)]
                  [(idx r i)  (memory-setx! memory (bvadd (register-ref registers r) i) x)])))]
    [(_ [id register-setx! memory-setx!] more ...)
     #'(begin
         (define-store-syntax [id register-setx! memory-setx!])
         (define-store-syntax more ...))]))

; define 16-bit and 8-bit store macros
(define-store-syntax
  [store20 register-set! memory-set20!]
  [store16 register-set16! memory-set16!]
  [store8 register-set8! memory-set8!])

(define-syntax-rule (flag-set? r f)
  (not (bveq (bvand (register-ref r REG/SR) f) (mspx-bv 0))))

; Macro for updating the status (flags) register based on the results of some
; computation
(define-syntax-rule (update-flags c z n v regs)
  (register-set! regs REG/SR
    (compute-flags c z n v (register-ref regs REG/SR))))

; Parameterizing these things on width, so that we can dispatch to the right one
; from other macros.
; Widths given in bits

; some implementations moved to lang-base

(define-syntax-rule (load. width src r m)
  (case width
    [(8) (load8 src r m)]
    [(16) (load16 src r m)]
    [(20) (load20 src r m)]))

; Binary Coded Decimal arithmetic
; Add: a and b are mspx-bvs
; TODO: this may not handle invalid BCD operands the same way as the hardware!
; tests are needed
(define (bvdadd a b)
  (let* ([t1 (bvadd a (mspx-bv #x06666))]
         [t2 (bvxor t1 b)]
         [t1 (bvadd t1 b)]
         [t2 (bvxor t1 t2)]
         [t2 (bvand (bvnot t2) (mspx-bv #x11110))]
         [t2 (bvor (bvashr t2 (mspx-bv 2)) (bvashr t2 (mspx-bv 3)))])
         (bvsub t1 t2)))

; Identity macro to get around the fact that the most generic form of the
; op/flags macro needs a parameter used to convert from mspx bitvectors to
; the working bitvector size
(define-syntax-rule (mspx->mspx x) x)

; Macro for doing an operation and then updating the flags (returns the result
; of the expression for possible use)
; Flag order is N Z C V
; tmp is the working bitvector size. Usually 20 (mspx-bv) but 32 for the
; extended ops
(define-syntax op/flags!
  (syntax-rules ()
    ; fmt1 with default N and Z flags
    [(op/flags! width src dst r m [tmp-bv mspx->tmp tmp->.] [sv sv. dv dv. x x.] expr c-expr v-expr)
     (op/flags! width src dst r m [tmp-bv mspx->tmp tmp->.] [sv sv. dv dv. x x.] expr (bvslt x (tmp-bv 0)) (bveq x (tmp-bv 0)) c-expr v-expr)]
    ; fmt1
    [(op/flags! width src dst r m [tmp-bv mspx->tmp tmp->.] [sv sv. dv dv. x x.] expr n-expr z-expr c-expr v-expr)
      (let* ([sv (mspx->tmp (load. width src r m))]
             [dv (mspx->tmp (load. width dst r m))]
             [x expr]
             [sv. (tmp->. width sv)]
             [dv. (tmp->. width dv)]
             [x. (tmp->. width x)]
             [n n-expr]
             [z z-expr]
             [c c-expr]
             [v v-expr])
           (begin (update-flags c z n v r) x))]
    ; fmt2 with default N and Z flags
    [(op/flags! width dst r m [tmp-bv mspx->tmp tmp->.] [dv dv. x x.] expr c-expr v-expr)
     (op/flags! width dst r m [tmp-bv mspx->tmp tmp->.] [dv dv. x x.] expr (bvslt x (tmp-bv 0)) (bveq x (tmp-bv 0)) c-expr v-expr)]
    ; fmt2
    [(op/flags! width dst r m [tmp-bv mspx->tmp tmp->.] [dv dv. x x.] expr n-expr z-expr c-expr v-expr)
      (let* ([dv (mspx->tmp (load. width dst r m))]
             [x expr]
             [dv. (tmp->. width dv)]
             [x. (tmp->. width x)]
             [n n-expr]
             [z z-expr]
             [c c-expr]
             [v v-expr])
           (begin (update-flags c z n v r) x))]

    ; Shorthand versions that default to mspx bitvectors as the working size
    [(op/flags! width src dst r m [sv sv. dv dv. x x.] expr c-expr v-expr)
     (op/flags! width src dst r m [mspx-bv mspx->mspx mspx->.] [sv sv. dv dv. x x.] expr c-expr v-expr)]
    [(op/flags! width src dst r m [sv sv. dv dv. x x.] expr n-expr z-expr c-expr v-expr)
     (op/flags! width src dst r m [mspx-bv mspx->mspx mspx->.] [sv sv. dv dv. x x.] expr n-expr z-expr c-expr v-expr)]
    [(op/flags! width dst r m [dv dv. x x.] expr c-expr v-expr)
     (op/flags! width dst r m [mspx-bv mspx->mspx mspx->.] [dv dv. x x.] expr c-expr v-expr)]
    [(op/flags! width dst r m [dv dv. x x.] expr n-expr z-expr c-expr v-expr)
     (op/flags! width dst r m [mspx-bv mspx->mspx mspx->.] [dv dv. x x.] expr n-expr z-expr c-expr v-expr)]
    ))

; Shorthand versions that default to 32bit extended bitvectors as the working size
(define-syntax opx/flags!
  (syntax-rules ()
    [(opx/flags! width src dst r m [sv sv. dv dv. x x.] expr c-expr v-expr)
     (op/flags! width src dst r m [ext-bv mspx->ext ext->.] [sv sv. dv dv. x x.] expr c-expr v-expr)]
    [(opx/flags! width src dst r m [sv sv. dv dv. x x.] expr n-expr z-expr c-expr v-expr)
     (op/flags! width src dst r m [ext-bv mspx->ext ext->.] [sv sv. dv dv. x x.] expr n-expr z-expr c-expr v-expr)]
    [(opx/flags! width dst r m [dv dv. x x.] expr c-expr v-expr)
     (op/flags! width dst r m [ext-bv mspx->ext ext->.] [dv dv. x x.] expr c-expr v-expr)]
    [(opx/flags! width dst r m [dv dv. x x.] expr n-expr z-expr c-expr v-expr)
     (op/flags! width dst r m [ext-bv mspx->ext ext->.] [dv dv. x x.] expr n-expr z-expr c-expr v-expr)]))


(define-syntax-rule (add/flags! width src dst r m)
  (op/flags! width src dst r m [srcval srcval. dstval dstval. x x.] 
             (bvadd dstval srcval)
             (bit-set? x width)
             (or (and (bvsgt srcval. (bv 0 width)) (bvsgt dstval. (bv 0 width)) (bvslt x. (bv 0 width)))
                 (and (bvslt srcval. (bv 0 width)) (bvsgt dstval. (bv 0 width)) (bvsgt x. (bv 0 width))))))


(define-syntax-rule (addc/flags! width src dst r m)
  (op/flags! width src dst r m [srcval srcval. dstval dstval. x x.] 
             (bvadd dstval srcval (mspx-bv (if (flag-set? r FLAG/C) 1 0)))
             (bit-set? x width)
             ; TODO double check ↓
             (or (and (bvsgt srcval. (bv 0 width)) (bvsgt dstval. (bv 0 width)) (bvslt x. (bv 0 width)))
                 (and (bvslt srcval. (bv 0 width)) (bvsgt dstval. (bv 0 width)) (bvsgt x. (bv 0 width))))))

(define-syntax-rule (sub/flags! width src dst r m)
  (op/flags! width src dst r m [srcval srcval. dstval dstval. x x.]
             (bvsub dstval srcval) 
             (bvslt x (mspx-bv 0)) 
             (bveq x (mspx-bv 0)) 
             (bit-set? x width)
             (or (and (bvslt srcval. (bv 0 width)) (bvsge dstval. (bv 0 width)) (bvslt x. (bv 0 width)))
                 (and (bvsge srcval. (bv 0 width)) (bvslt dstval. (bv 0 width)) (bvsge x. (bv 0 width))))))

(define-syntax-rule (subc/flags! width src dst r m)
  (op/flags! width src dst r m [srcval srcval. dstval dstval. x x.]
             (bvadd dstval (bvnot srcval) (mspx-bv (if (flag-set? r FLAG/C) 1 0))) 
             (bvslt x (mspx-bv 0)) 
             (bveq x (mspx-bv 0)) 
             (bit-set? x width)
             ; TODO what is hardware behavior for these flags? Manual is unclear
             ; how overflow is calculated, is the carry bit factored in? Is it
             ; logically part of src or dst in the behavior description?
             (or (and (bvslt srcval. (bv 0 width)) (bvsge dstval. (bv 0 width)) (bvslt x. (bv 0 width)))
                 (and (bvsge srcval. (bv 0 width)) (bvslt dstval. (bv 0 width)) (bvsge x. (bv 0 width))))))

(define-syntax-rule (dadd/flags! width src dst r m)
   (op/flags! width src dst r m [srcval srcval. dstval dstval. x x.]
             (bvdadd srcval dstval)
             ((bit-set? x (- width 1)))
             (bveq x. (mspx-bv 0))
             ; Manual defines carry flag as being set when "BCD result is too large",
             ; so we'll check if there are any bits set above the width of the operation
             (bvsgt (bvand (trunc. width (mspx-bv -1)) x) (mspx-bv 0))
             ; V flag is formally undefined in manual. Need to do some HW tests to find actual behavior.
             #f)) 

(define-syntax-rule (and/flags! width src dst r m)
  (op/flags! width src dst r m [srcval srcval. dstval dstval. x x.]
             (bvand dstval srcval) 
             (not (bveq x (mspx-bv 0)))
             #f))

(define-syntax-rule (xor/flags! width src dst r m)
  (op/flags! width src dst r m [srcval srcval. dstval dstval. x x.]
             (bvxor dstval srcval) 
             (not (bveq x (mspx-bv 0)))
             (and (bvslt srcval. (bv 0 width)) (bvslt dstval. (bv 0 width)))))

(define-syntax-rule (rra/flags! width dst r m)
  (op/flags! width dst r m [dstval dstval. x x.]
             (bvashr dstval (mspx-bv 1)) 
             (bveq (extract 0 0 dstval) (bv -1 1))
             #f))

(define-syntax-rule (rrc/flags! width dst r m)
  (op/flags! width dst r m [dstval dstval. x x.]
             (bvor (bvashr dstval (mspx-bv 1)) 
                   (mspx-bv (if (flag-set? r FLAG/C) (bvshl (mspx-bv 1) (- width 1)) 0)))
             (bveq (extract 0 0 dstval) (bv -1 1))
             #f))

(define-syntax-rule (sxt/flags! dst r m)
  (op/flags! 16 dst r m [dstval dstval. x x.]
             (sign-extend (extract 7 0) (bitvector mspx-bits))
             (not (bveq x (mspx-bv 0)))
             #f))

(define-syntax-rule (push. width src r m)
    (let ([sp (bvsub (register-ref r REG/SP) (mspx-bv (if (> width 16) 4 2)))])
      (register-set! r REG/SP sp)
      (memory-set8! m sp (load8 src r m))))

; Extended operations

(define-syntax-rule (addx/flags! width src dst r m)
  (opx/flags! width src dst r m [srcval srcval. dstval dstval. x x.]
             (bvadd dstval srcval)
             (bit-set? x width)
             (or (and (bvsgt srcval. (bv 0 width)) (bvsgt dstval. (bv 0 width)) (bvslt x. (bv 0 width)))
                 (and (bvslt srcval. (bv 0 width)) (bvsgt dstval. (bv 0 width)) (bvsgt x. (bv 0 width))))))

(define-syntax-rule (addcx/flags! width src dst r m)
  (opx/flags! width src dst r m [srcval srcval. dstval dstval. x x.]
             (bvadd dstval srcval (ext-bv (if (flag-set? r FLAG/C) 1 0)))
             (bit-set? x width)
             ; TODO double check ↓
             (or (and (bvsgt srcval. (bv 0 width)) (bvsgt dstval. (bv 0 width)) (bvslt x. (bv 0 width)))
                 (and (bvslt srcval. (bv 0 width)) (bvsgt dstval. (bv 0 width)) (bvsgt x. (bv 0 width))))))

(define-syntax-rule (subx/flags! width src dst r m)
  (opx/flags! width src dst r m [srcval srcval. dstval dstval. x x.]
             (bvsub dstval srcval) 
             (bvslt x (ext-bv 0)) 
             (bveq x (ext-bv 0)) 
             (bit-set? x width)
             (or (and (bvslt srcval. (bv 0 width)) (bvsge dstval. (bv 0 width)) (bvslt x. (bv 0 width)))
                 (and (bvsge srcval. (bv 0 width)) (bvslt dstval. (bv 0 width)) (bvsge x. (bv 0 width))))))

(define-syntax-rule (subcx/flags! width src dst r m)
  (opx/flags! width src dst r m [srcval srcval. dstval dstval. x x.]
             (bvadd dstval (bvnot srcval) (ext-bv (if (flag-set? r FLAG/C) 1 0))) 
             (bvslt x (ext-bv 0)) 
             (bveq x (ext-bv 0))
             (bit-set? x width)
             ; TODO what is hardware behavior for these flags? Manual is unclear
             ; how overflow is calculated, is the carry bit factored in? Is it
             ; logically part of src or dst in the behavior description?
             (or (and (bvslt srcval. (bv 0 width)) (bvsge dstval. (bv 0 width)) (bvslt x. (bv 0 width)))
                 (and (bvsge srcval. (bv 0 width)) (bvslt dstval. (bv 0 width)) (bvsge x. (bv 0 width))))))

(define-syntax-rule (daddx/flags! width src dst r m)
  (opx/flags! width src dst r m [srcval srcval. dstval dstval. x x.]
         (bvdadd srcval dstval)
         ((bit-set? x (- width 1)))
         (bveq x. (ext-bv 0))
         ; Manual defines carry flag as being set when "BCD result is too large",
         ; so we'll check if there are any bits set above the width of the operation
         (bvsgt (bvand (trunc. width (ext-bv -1)) x) (ext-bv 0))
         ; V flag is formally undefined in manual. Need to do some HW tests to find actual behavior.
         #f))

(define-syntax-rule (andx/flags! width src dst r m)
  (opx/flags! width src dst r m [srcval srcval. dstval dstval. x x.]
         (bvand dstval srcval) 
         (not (bveq x (ext-bv 0)))
         #f))

(define-syntax-rule (xorx/flags! width src dst r m)
  (opx/flags! width src dst r m [srcval srcval. dstval dstval. x x.]
         (bvxor dstval srcval) 
         (not (bveq x (ext-bv 0)))
         (and (bvslt srcval. (bv 0 width)) (bvslt dstval. (bv 0 width)))))

(define-syntax-rule (popm. width n rdst r m)
    ; Define a recursive function and run it n times
    (letrec ([f (lambda (i)
                   (if (>= i 0)
                     (let* ([sp (register-ref. width r REG/SP)]
                            [val (memory-ref. width m sp)])
                       (begin 
                         (register-set! r (- rdst i) val)
                         (register-set! r REG/SP (bvadd sp (mspx-bv (bytewidth. width))))
                         (f (- i 1))))
                     (void)))])
      (f (- n 1))))

(define-syntax-rule (pushm. width n rdst r m)
    (letrec ([f (lambda (i)
                   (if (> i 0)
                     (let* ([sp (bvsub (register-ref. width r REG/SP) (mspx-bv (bytewidth. width)))])
                       (begin 
                         (register-set! r REG/SP sp)
                         (memory-set.! width m sp (register-ref. width r (+ (- rdst n) i)))
                         (f (- i 1))))
                     (void)))])
      (f n)))

(define-syntax-rule (rrcm/flags! width n rdst r m)
  (op/flags! width (reg rdst) r m [dstval dstval. x x.]
             (bvor (bvlshr dstval (mspx-bv n))
                   (bvshl dstval (mspx-bv (+ width 1 (- n))))
                   (if (flag-set? r FLAG/C) (bvshl (mspx-bv 1) (- width n)) (mspx-bv 0)))
             (bveq (extract (- n 1) (- n 1) dstval) (bv -1 1))
             #f))

(define-syntax-rule (rrum/flags! width n rdst r m)
  (op/flags! width (reg rdst) r m [dstval dstval. x x.]
             (bvlshr dstval (mspx-bv n))
             (bveq (extract (- n 1) (- n 1) dstval) (bv -1 1))
             #f))

(define-syntax-rule (rram/flags! width n rdst r m)
  (op/flags! width (reg rdst) r m [dstval dstval. x x.]
             (bvashr dstval (mspx-bv n))
             (bveq (extract (- n 1) (- n 1) dstval) (bv -1 1))
             #f))

(define-syntax-rule (rlam/flags! width n rdst r m)
  (op/flags! width (reg rdst) r m [dstval dstval. x x.]
             (bvshl dstval (mspx-bv n))
             (bveq (extract (- width n) (- width n) dstval) (bv -1 1))
             #f))

(define-syntax-rule (rrcx/flags! width dst r m)
  (op/flags! width dst r m [dstval dstval. x x.]
             (bvor (bvashr dstval (mspx-bv 1)) 
                   (mspx-bv (if (flag-set? r FLAG/C) (bvshl (mspx-bv 1) (- width 1)) 0)))
             (bveq (extract 0 0 dstval) (bv -1 1))
             #f))

(define-syntax-rule (rrux/flags! width rdst r m)
  (op/flags! width (reg rdst) r m [dstval dstval. x x.]
             (bvlshr dstval (mspx-bv 1))
             (bveq (extract 0 0 dstval) (bv -1 1))
             #f))

; TODO: this one is supposed to work slightly differently for non-register operands.
; Specifically, The high 12/4 bits of .b and .w are not cleared, but retain
; their original values.
(define-syntax-rule (rrax/flags! width dst r m)
  (op/flags! width dst r m [dstval dstval. x x.]
             (bvashr dstval (mspx-bv 1))
             (bveq (extract 0 0 dstval) (bv -1 1))
             #f))

(define-syntax-rule (sxtx/flags! width dst r m)
  (op/flags! width dst r m [dstval dstval. x x.]
             (sign-extend (extract 7 0) (bitvector mspx-bits))
             (not (bveq x (mspx-bv 0)))
             #f))


; interpreter step function
; this macro expands into a huge mess which inlines the entire logic of the step function
(define (step instr r m)
  (match instr
    ; Workaround: because load and store are macros, and store basically
    ;  evaluates to (memory-set dst val), we need to make sure that src is
    ;  evaluated first because it might have side effects (autoincrement).
    ; So we have to wrap with a let expression here, and anywhere that stores a
    ; value in a destination.
    ;  The alternate way to solve this problem is to have either load or store
    ;  be a function, so that racket will evaluate the arguments before
    ;  "expanding" (calling) the function.
    [(mov.w src dst) (let ([val (load16 src r m)]) (store16 val dst r m))]
    [(mov.b src dst) (let ([val (load8 src r m)]) (store8 val dst r m))]

    [(add.w src dst) (let ([val (add/flags! 16 src dst r m)]) (store16 val dst r m))]
    [(add.b src dst) (let ([val (add/flags!  8 src dst r m)]) (store8 val dst r m))]

    [(addc.w src dst) (let ([val (addc/flags! 16 src dst r m)]) (store16 val dst r m))]
    [(addc.b src dst) (let ([val (addc/flags!  8 src dst r m)]) (store8 val dst r m))]

    [(sub.w src dst) (let ([val (sub/flags! 16 src dst r m)]) (store16 val dst r m))]
    [(sub.b src dst) (let ([val (sub/flags!  8 src dst r m)]) (store8 val dst r m))]

    [(subc.w src dst) (let ([val (subc/flags! 16 src dst r m)]) (store16 val dst r m))]
    [(subc.b src dst) (let ([val (subc/flags!  8 src dst r m)]) (store8 val dst r m))]

    [(cmp.w src dst) (sub/flags! 16 src dst r m)]
    [(cmp.b src dst) (sub/flags!  8 src dst r m)]

    [(dadd.w src dst) (let ([val (dadd/flags! 16 src dst r m)]) (store16 val dst r m))]
    [(dadd.b src dst) (let ([val (dadd/flags!  8 src dst r m)]) (store8 val dst r m))]

    [(bit.w src dst) (and/flags! 16 src dst r m)]
    [(bit.b src dst) (and/flags! 8 src dst r m)]

    [(bic.w src dst) (store16 (bvand (bvnot (load16 src r m)) (load16 dst r m)) dst r m)]
    [(bic.b src dst) (store8  (bvand (bvnot (load8  src r m)) (load8  dst r m)) dst r m)]

    [(bis.w src dst) (store16 (bvor (load16 src r m) (load16 dst r m)) dst r m)]
    [(bis.b src dst) (store8  (bvor (load8  src r m) (load8  dst r m)) dst r m)]

    [(xor.w src dst) (let ([val (xor/flags! 16 src dst r m)]) (store16 val dst r m))]
    [(xor.b src dst) (let ([val (xor/flags!  8 src dst r m)]) (store8 val dst r m))]

    [(and.w src dst) (let ([val (and/flags! 16 src dst r m)]) (store16 val dst r m))]
    [(and.b src dst) (let ([val (and/flags!  8 src dst r m)]) (store8 val dst r m))]

    [(push.w src) (push. 16 src r m)]
    [(push.b src) (push. 8  src r m)]

    [(rra.w dst) (let ([val (rra/flags! 16 dst r m)]) (store16 val dst r m))]
    [(rra.b dst) (let ([val (rra/flags!  8 dst r m)]) (store8 val dst r m))]

    [(rrc.w dst) (let ([val (rrc/flags! 16 dst r m)]) (store16 val dst r m))]
    [(rrc.b dst) (let ([val (rrc/flags!  8 dst r m)]) (store8 val dst r m))]

    [(swpb dst) (let ([val (load16 dst r m)]) (store16 (bvor (high8->low val) (low8->high val)) dst r m))]
    
    [(sxt dst) (let ([val (sxt/flags! dst r m)]) (store16 val dst r m))]

    [(call dst) (let ([tmp (load16 dst r m)]
                      [sp (bvsub (register-ref16 r REG/SP) 2)]
                      [pc (register-ref16 r REG/PC)])
                  (register-set16! r REG/SP sp)
                  (memory-set16! m sp pc)
                  (register-set16! r REG/PC tmp))]

    [(reti) (let* ([sp1 (register-ref r REG/SP)]
                   [val1 (memory-ref16 m sp1)]
                   [sp2 (bvadd sp1 (mspx-bv 2))]
                   [val2 (memory-ref16 m sp2)])
              (register-set16! r REG/SR (bvand val1 (mspx-bv #x00fff)))
              (register-set16! r REG/PC (bvor val2 (bvshl (bvand val1 (mspx-bv #x0f000)) 4)))
              (register-set16! r REG/SP (bvadd sp2 (mspx-bv 2))))]

    [(jmp target) (register-set! r REG/PC target )]

    [(jc target) (if (flag-set? r FLAG/C) (register-set! r REG/PC target) (void))]
    [(jnc target) (if (not (flag-set? r FLAG/C)) (register-set! r REG/PC target) (void))]

    [(jz target) (if (flag-set? r FLAG/Z) (register-set! r REG/PC target) (void))]
    [(jnz target) (if (not (flag-set? r FLAG/Z)) (register-set! r REG/PC target) (void))]

    [(jn target) (if (flag-set? r FLAG/N) (register-set! r REG/PC target) (void))]

    [(jl target) (if (xor (flag-set? r FLAG/N) (flag-set? r FLAG/V)) (register-set! r REG/PC target) (void))]
    [(jge target) (if (not (xor (flag-set? r FLAG/N) (flag-set? r FLAG/V))) (register-set! r REG/PC target) (void))]

    ; Extended instructions

    [(movx.a src dst) (let ([val (load20 src r m)]) (store20 val dst r m))]
    [(movx.w src dst) (let ([val (load16 src r m)]) (store16 val dst r m))]
    [(movx.b src dst) (let ([val (load8  src r m)]) (store8  val dst r m))]

    [(addx.a src dst) (let ([val (ext->mspx (addx/flags! 20 src dst r m))]) (store20 val dst r m))]
    [(addx.w src dst) (let ([val (ext->mspx (addx/flags! 16 src dst r m))]) (store16 val dst r m))]
    [(addx.b src dst) (let ([val (ext->mspx (addx/flags! 8  src dst r m))]) (store8  val dst r m))]

    [(addcx.a src dst) (let ([val (ext->mspx (addcx/flags! 20 src dst r m))]) (store20 val dst r m))]
    [(addcx.w src dst) (let ([val (ext->mspx (addcx/flags! 16 src dst r m))]) (store16 val dst r m))]
    [(addcx.b src dst) (let ([val (ext->mspx (addcx/flags! 8  src dst r m))]) (store8  val dst r m))]

    [(subx.a src dst) (let ([val (ext->mspx (subx/flags! 20 src dst r m))]) (store20 val dst r m))]
    [(subx.w src dst) (let ([val (ext->mspx (subx/flags! 16 src dst r m))]) (store16 val dst r m))]
    [(subx.b src dst) (let ([val (ext->mspx (subx/flags! 8  src dst r m))]) (store8  val dst r m))]

    [(subcx.a src dst) (let ([val (ext->mspx (subcx/flags! 20 src dst r m))]) (store20 val dst r m))]
    [(subcx.w src dst) (let ([val (ext->mspx (subcx/flags! 16 src dst r m))]) (store16 val dst r m))]
    [(subcx.b src dst) (let ([val (ext->mspx (subcx/flags! 8  src dst r m))]) (store8  val dst r m))]

    [(cmpx.a src dst) (subx/flags! 20 src dst r m)]
    [(cmpx.w src dst) (subx/flags! 16 src dst r m)]
    [(cmpx.b src dst) (subx/flags!  8 src dst r m)]

    [(daddx.a src dst) (let ([val (ext->mspx (daddx/flags! 20 src dst r m))]) (store20 val dst r m))]
    [(daddx.w src dst) (let ([val (ext->mspx (daddx/flags! 16 src dst r m))]) (store16 val dst r m))]
    [(daddx.b src dst) (let ([val (ext->mspx (daddx/flags! 8  src dst r m))]) (store8  val dst r m))]

    [(bitx.a src dst) (andx/flags! 20 src dst r m)]
    [(bitx.w src dst) (andx/flags! 16 src dst r m)]
    [(bitx.b src dst) (andx/flags! 8 src dst r m)]

    [(bicx.a src dst) (store20 (bvand (bvnot (load20 src r m)) (load20 dst r m)) dst r m)]
    [(bicx.w src dst) (store16 (bvand (bvnot (load16 src r m)) (load16 dst r m)) dst r m)]
    [(bicx.b src dst) (store8  (bvand (bvnot (load8  src r m)) (load8  dst r m)) dst r m)]

    [(bisx.w src dst) (store20 (bvor (load16 src r m) (load20 dst r m)) dst r m)]
    [(bisx.w src dst) (store16 (bvor (load16 src r m) (load16 dst r m)) dst r m)]
    [(bisx.b src dst) (store8  (bvor (load8  src r m) (load8  dst r m)) dst r m)]

    [(xorx.a src dst) (let ([val (ext->mspx (xorx/flags! 20 src dst r m))]) (store20 val dst r m))]
    [(xorx.w src dst) (let ([val (ext->mspx (xorx/flags! 16 src dst r m))]) (store16 val dst r m))]
    [(xorx.b src dst) (let ([val (ext->mspx (xorx/flags! 8  src dst r m))]) (store8  val dst r m))]

    [(andx.a src dst) (let ([val (ext->mspx (andx/flags! 20 src dst r m))]) (store20 val dst r m))]
    [(andx.w src dst) (let ([val (ext->mspx (andx/flags! 16 src dst r m))]) (store16 val dst r m))]
    [(andx.b src dst) (let ([val (ext->mspx (andx/flags! 8  src dst r m))]) (store8  val dst r m))]

    [(calla dst) (let ([tmp (load20 dst r m)]
                       [sp (bvsub (register-ref16 r REG/SP) 4)]
                       [pc (register-ref16 r REG/PC)])
                  (register-set! r REG/SP sp)
                  (memory-set20! m sp pc)
                  (register-set! r REG/PC tmp))]

    [(popm.a n rdst) (popm. 20 n rdst r m)]
    [(popm.w n rdst) (popm. 16 n rdst r m)]

    [(pushm.a n rdst) (pushm. 20 n rdst r m)]
    [(pushm.w n rdst) (pushm. 16 n rdst r m)]

    [(pushx.a src) (push. 20 src r m)]
    [(pushx.w src) (push. 16 src r m)]
    [(pushx.b src) (push. 8  src r m)]

    [(rrcm.a n rdst) (let ([val (rrcm/flags! 20 n rdst r m)]) (store20 val (reg rdst) r m))]
    [(rrcm.w n rdst) (let ([val (rrcm/flags! 16 n rdst r m)]) (store16 val (reg rdst) r m))]

    [(rrum.a n rdst) (let ([val (rrum/flags! 20 n rdst r m)]) (store20 val (reg rdst) r m))]
    [(rrum.w n rdst) (let ([val (rrum/flags! 16 n rdst r m)]) (store16 val (reg rdst) r m))]

    [(rram.a n rdst) (let ([val (rram/flags! 20 n rdst r m)]) (store20 val (reg rdst) r m))]
    [(rram.w n rdst) (let ([val (rram/flags! 16 n rdst r m)]) (store16 val (reg rdst) r m))]

    [(rlam.a n rdst) (let ([val (rlam/flags! 20 n rdst r m)]) (store20 val (reg rdst) r m))]
    [(rlam.w n rdst) (let ([val (rlam/flags! 16 n rdst r m)]) (store16 val (reg rdst) r m))]

    [(rrcx.a dst) (let ([val (rrcx/flags! 20 dst r m)]) (store20 val dst r m))]
    [(rrcx.w dst) (let ([val (rrcx/flags! 16 dst r m)]) (store16 val dst r m))]
    [(rrcx.b dst) (let ([val (rrcx/flags! 8  dst r m)]) (store8  val dst r m))]

    [(rrux.a rdst) (let ([val (rrux/flags! 20 rdst r m)]) (store20 val (reg rdst) r m))]
    [(rrux.w rdst) (let ([val (rrux/flags! 16 rdst r m)]) (store16 val (reg rdst) r m))]
    [(rrux.b rdst) (let ([val (rrux/flags! 8  rdst r m)]) (store8  val (reg rdst) r m))]

    [(rrax.a dst) (let ([val (rrax/flags! 20 dst r m)]) (store20 val dst r m))]
    [(rrax.w dst) (let ([val (rrax/flags! 16 dst r m)]) (store16 val dst r m))]
    [(rrax.b dst) (let ([val (rrax/flags! 8  dst r m)]) (store8  val dst r m))]

    [(swpbx.a dst) (let ([val (load20 dst r m)]) 
                     (store20 (bvor (high8->low val) (low8->high val) (bvand val (mspx-bv #xf000))) dst r m))]
    [(swpbx.w dst) (let ([val (load16 dst r m)]) 
                     (store16 (bvor (high8->low val) (low8->high val)) dst r m))]

    [(sxtx.a dst) (let ([val (sxtx/flags! 20 dst r m)]) (store20 val dst r m))]
    [(sxtx.w dst) (let ([val (sxtx/flags! 16 dst r m)]) (store16 val dst r m))]

    ))

(define (stepn s n)
  ; s is a state, n is the number of steps
  (let ([ip (bitvector->integer (register-ref (state-r s) REG/PC))])
      (begin 
        ; Update instruction ptr
        (register-set! (state-r s) REG/PC (bvadd (mspx-bv (+ ip 1))))
        ; Compute result of instruction at old value of instruction ptr
        (step (vector-ref (state-instrs s) ip) (state-r s) (state-m s))
        ; Do the next step
        (if (> n 1) 
          (stepn s (- n 1))
          (void)))))
    

; to implement:
; semantics for all addressing modes
; step function for mov, sub, and cmp
; working stepn (obviously only supporting what step supports)
; start thinking about tests

; interface things:
; R0 is program counter
; R1 is stack pointer
; R2 is status register
; R3 is zero register
; R4 - R15 are general purpose

; other things to keep in mind:
; want to eventually be able to run on hardware
;   that depends on encoder / decoder / setting up MSP hardware
;   mspdebug from racket???



; Symbolic tests for AI:
;  - Check that AI loads same value, and result register is X + 2
;  - AI should be able to load any number if that number was stored in memory
;  - AI should load a 8 bit value for 8 bit op, 16 bit value for 16 bit op
;  - Should step by +1 for 8 bit op, +2 for 16 bit (does it mask to even
;  addresses here? test!)
;  - 

; To test:
; - load and store (incl. AI -- make a different thing -- one set without AI and
; one with AI)
;    - should respect truncation and width
; - store-load tests
; - test all operands
; - get rest of instructions
; - after that start writing tests for the rest of the instructions
