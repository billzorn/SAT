#lang rosette/safe

(require rosette/lib/match "../lib/bv.rkt" "lang-base.rkt")

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
(struct jump instruction (target) #:transparent)

; actual definitions of instructions
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
  [jl jump]
  )

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

; Flag register bitmasks
(define FLAG/C (mspx-bv #b00000001))
(define FLAG/Z (mspx-bv #b00000010))
(define FLAG/N (mspx-bv #b00000100))
(define FLAG/V (mspx-bv #b10000000))

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
  [store16 register-set16! memory-set16!]
  [store8 register-set8! memory-set8!])

(define-syntax-rule (flag-set? r f)
  (not (bveq (bvand (register-ref r REG/SR) f) (mspx-bv 0))))

; Macro for updating the status (flags) register based on the results of some
; computation
(define-syntax-rule (update-flags c z n v regs)
  (register-set! regs REG/SR
    (bvor (bvand 
            (register-ref regs REG/SR) 
            (bvnot (mspx-bv #b10000111))) 
          (if c FLAG/C (mspx-bv 0))
          (if z FLAG/Z (mspx-bv 0))
          (if n FLAG/N (mspx-bv 0))
          (if v FLAG/V (mspx-bv 0)))))

; Parameterizing these things on width, so that we can dispatch to the right one
; from other macros.
; Widths given in bits

(define-syntax-rule (mspx->. width x)
  (case width
    [(8) (mspx->byte x)]
    [(16) (mspx->word x)]))

(define-syntax-rule (load. width src r m)
  (case width
    [(8) (load8 src r m)]
    [(16) (load16 src r m)]))

(define-syntax-rule (trunc. width x)
  (case width
    [(8) (trunc8 x)]
    [(16) (trunc16 x)]))

(define-syntax-rule (register-ref. width r n)
  (case width
    [(8) (register-ref8 r n)]
    [(16) (register-ref16 r n)]))

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

; Macro for doing an operation and then updating the flags (returns the result
; of the expression for possible use)
; Flag order is N Z C V
(define-syntax op/flags!
  (syntax-rules ()
    ; fmt1 with default N and Z flags
    [(op/flags! width src dst r m [sv sv. dv dv. x x.] expr c-expr v-expr)
     (op/flags! width src dst r m [sv sv. dv dv. x x.] expr (bvslt x (mspx-bv 0)) (bveq x (mspx-bv 0)) c-expr v-expr)]
    ; fmt1
    [(op/flags! width src dst r m [sv sv. dv dv. x x.] expr n-expr z-expr c-expr v-expr)
      (let* ([sv (load. width src r m)]
             [dv (load. width dst r m)]
             [x expr]
             [sv. (mspx->. width sv)]
             [dv. (mspx->. width dv)]
             [x. (mspx->. width x)]
             [n n-expr]
             [z z-expr]
             [c c-expr]
             [v v-expr])
           (begin (update-flags c z n v r) x))]
    ; fmt2 with default N and Z flags
    [(op/flags! width dst r m [dv dv. x x.] expr c-expr v-expr)
     (op/flags! width dst r m [dv dv. x x.] expr (bvslt x (mspx-bv 0)) (bveq x (mspx-bv 0)) c-expr v-expr)]
    ; fmt2
    [(op/flags! width dst r m [dv dv. x x.] expr n-expr z-expr c-expr v-expr)
      (let* ([dv (load. width dst r m)]
             [x expr]
             [dv. (mspx->. width dv)]
             [x. (mspx->. width x)]
             [n n-expr]
             [z z-expr]
             [c c-expr]
             [v v-expr])
           (begin (update-flags c z n v r) x))]
      ))

(define-syntax-rule (add/flags! width src dst r m)
  (op/flags! width src dst r m [srcval srcval. dstval dstval. x x.] 
             (bvadd dstval srcval)
             (bit-set? x width)
             (or (and (bvsgt srcval. (word 0)) (bvsgt dstval. (word 0)) (bvslt x. (word 0)))
                 (and (bvslt srcval. (word 0)) (bvsgt dstval. (word 0)) (bvsgt x. (word 0))))))

(define-syntax-rule (addc/flags! width src dst r m)
  (op/flags! width src dst r m [srcval srcval. dstval dstval. x x.] 
             (bvadd dstval srcval (mspx-bv (if (flag-set? r FLAG/C) 1 0)))
             (bit-set? x width)
             ; TODO double check ↓
             (or (and (bvsgt srcval. (word 0)) (bvsgt dstval. (word 0)) (bvslt x. (word 0)))
                 (and (bvslt srcval. (word 0)) (bvsgt dstval. (word 0)) (bvsgt x. (word 0))))))

(define-syntax-rule (sub/flags! width src dst r m)
  (op/flags! width src dst r m [srcval srcval. dstval dstval. x x.]
             (bvsub dstval srcval) 
             (bvslt x (mspx-bv 0)) 
             (bveq x (mspx-bv 0)) 
             (bit-set? x width)
             (or (and (bvslt srcval. (word 0)) (bvsge (bvsgt dstval. (word 0)) (word 0)) (bvslt x. (word 0)))
                 (and (bvsge srcval. (word 0)) (bvslt (bvsgt dstval. (word 0)) (word 0)) (bvsge x. (word 0))))))

(define-syntax-rule (subc/flags! width src dst r m)
  (op/flags! width src dst r m [srcval srcval. dstval dstval. x x.]
             (bvadd dstval (bvnot srcval) (mspx-bv (if (flag-set? r FLAG/C) 1 0))) 
             (bvslt x (mspx-bv 0)) 
             (bveq x (mspx-bv 0)) 
             (bit-set? x width)
             ; TODO what is hardware behavior for these flags? Manual is unclear
             ; how overflow is calculated, is the carry bit factored in? Is it
             ; logically part of src or dst in the behavior description?
             (or (and (bvslt srcval. (word 0)) (bvsge (bvsgt dstval. (word 0)) (word 0)) (bvslt x. (word 0)))
                 (and (bvsge srcval. (word 0)) (bvslt (bvsgt dstval. (word 0)) (word 0)) (bvsge x. (word 0))))))

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
                 (and (bvslt srcval. (word 0)) (bvslt dstval. (word 0)))))

(define-syntax-rule (rra/flags! width dst r m)
      (op/flags! width dst r m [dstval dstval. x x.]
                 (bvashr dstval (mspx-bv 1)) 
                 (bveq (extract 0 0 dstval) (bv -1 1))
                 #f))

(define-syntax-rule (rrc/flags! width dst r m)
      (op/flags! width dst r m [dstval dstval. x x.]
                 (bvadd (bvashr dstval (mspx-bv 1)) (mspx-bv (if (flag-set? r FLAG/C) (expt 2 (- width 1)) 0)))
                 (bveq (extract 0 0 dstval) (bv -1 1))
                 #f))

(define-syntax-rule (sxt/flags! dst r m)
      (op/flags! 16 dst r m [dstval dstval. x x.]
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

    [(cmp.w src dst) (subc/flags! 16 src dst r m)]
    [(cmp.b src dst) (subc/flags!  8 src dst r m)]

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

    [(push.w src) (let ([sp (bvsub (register-ref r REG/SP) (mspx-bv 2))])
                    (register-set! r REG/SP sp)
                    (memory-set16! m sp (load16 src r m)))]
    [(push.b src) (let ([sp (bvsub (register-ref r REG/SP) (mspx-bv 2))])
                    (register-set! r REG/SP sp)
                    (memory-set8! m sp (load8 src r m)))]

    [(rra.w dst) (let ([val (rra/flags! 16 dst r m)]) (store16 val dst r m))]
    [(rra.b dst) (let ([val (rra/flags!  8 dst r m)]) (store8 val dst r m))]

    [(rrc.w dst) (let ([val (rrc/flags! 16 dst r m)]) (store16 val dst r m))]
    [(rrc.b dst) (let ([val (rrc/flags!  8 dst r m)]) (store8 val dst r m))]

    [(swpb dst) (let ([val (load16 dst r m)]) (store16 (bvor (high8->low val) (low8->high val)) dst r m))]
    
    [(sxt dst) (let ([val (sxt/flags! dst r m)]) (store16 val dst r m))]

    ; call
    ; reti

    [(jmp target) (register-set! r REG/PC target )]

    [(jc target) (if (flag-set? r FLAG/C) (register-set! r REG/PC target) (void))]
    [(jnc target) (if (not (flag-set? r FLAG/C)) (register-set! r REG/PC target) (void))]

    [(jz target) (if (flag-set? r FLAG/Z) (register-set! r REG/PC target) (void))]
    [(jnz target) (if (not (flag-set? r FLAG/Z)) (register-set! r REG/PC target) (void))]

    [(jn target) (if (flag-set? r FLAG/N) (register-set! r REG/PC target) (void))]

    [(jl target) (if (xor (flag-set? r FLAG/N) (flag-set? r FLAG/V)) (register-set! r REG/PC target) (void))]
    [(jge target) (if (not (xor (flag-set? r FLAG/N) (flag-set? r FLAG/V))) (register-set! r REG/PC target) (void))]
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
