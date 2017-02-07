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
  ; [jmp]
  ; [jc]
  ; [jnc]
  ; [jz]
  ; [jnz]
  ; [jn]
  ; [jge]
  ; [jl]
  )

; representation of complete state
; instrs is a vector of instructions representing the program.
(struct state (instrs r m running) #:transparent)

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

; Macro for updating the status (flags) register based on the results of some
; computation
(define-syntax-rule (update-flags c z n v regs)
  (register-set! regs 2 
    (bvor (bvand 
            (register-ref regs 2) 
            (bvnot (mspx-bv #b10000111))) 
          (mspx-bv (if c #b00000001 0))
          (mspx-bv (if z #b00000010 0))
          (mspx-bv (if n #b00000100 0))
          (mspx-bv (if v #b10000000 0)))))

; Parameterizing these things on width, so that we can dispatch to the right one
; from other macros.
; Widths given in bits

(define-syntax-rule (mspx->. width x)
  (case width
    [(8) (mspx->byte x)]
    [(16) (mspx->word x)]
    [(20) x]))

(define-syntax-rule (load. width src r m)
  (case width
    [(8) (load8 src r m)]
    [(16) (load16 src r m)]
    ))

; Macro for doing an operation and then updating the flags (returns the result
; of subtraction for possible use)
; Flag order is N Z C V
(define-syntax do-flags.
  (syntax-rules ()
    [(do-flags. width src dst r m [sv sv. dv dv. x x.] expr n-expr z-expr c-expr v-expr)
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
      [(do-flags. width src dst r m [sv sv. dv dv. x x.] expr c-expr v-expr)
       (do-flags. width src dst r m [sv sv. dv dv. x x.] expr (bvslt x (mspx-bv 0)) (bveq x (mspx-bv 0)) c-expr v-expr)]))

(define-syntax-rule (do-sub-flags. width src dst r m)
  (do-flags. width src dst r m [srcval srcval. dstval dstval. x x.]
             (bvsub dstval srcval) 
             (bvslt x (mspx-bv 0)) 
             (bveq x (mspx-bv 0)) 
             (bit-set? x width)
             (or (and (bvslt srcval. (word 0)) (bvsge (bvsgt dstval. (word 0)) (word 0)) (bvslt x. (word 0)))
                 (and (bvsge srcval. (word 0)) (bvslt (bvsgt dstval. (word 0)) (word 0)) (bvsge x. (word 0))))))

(define-syntax-rule (do-add-flags. width src dst r m)
  (do-flags. width src dst r m [srcval srcval. dstval dstval. x x.] 
             (bvadd dstval srcval)
             (bit-set? x width)
             (or (and (bvsgt srcval. (word 0)) (bvsgt dstval. (word 0)) (bvslt x. (word 0)))
                 (and (bvslt srcval. (word 0)) (bvsgt dstval. (word 0)) (bvsgt x. (word 0))))))

(define-syntax-rule (do-xor-flags. width src dst r m)
      (do-flags. width src dst r m [srcval srcval. dstval dstval. x x.]
                 (bvxor dstval srcval) 
                 (not (bveq x (mspx-bv 0)))
                 (and (bvslt srcval. (word 0)) (bvslt dstval. (word 0)))))

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
    [(add.w src dst) (let ([val (do-add-flags. 16 src dst r m)]) (store16 val dst r m))]
    [(add.b src dst) (let ([val (do-add-flags.  8 src dst r m)]) (store8 val dst r m))]
    [(sub.w src dst) (let ([val (do-sub-flags. 16 src dst r m)]) (store16 val dst r m))]
    [(sub.b src dst) (let ([val (do-sub-flags.  8 src dst r m)]) (store8 val dst r m))]
    [(xor.w src dst) (let ([val (do-xor-flags. 16 src dst r m)]) (store16 val dst r m))]
    [(xor.b src dst) (let ([val (do-xor-flags.  8 src dst r m)]) (store8 val dst r m))]
    [(cmp.w src dst) (do-sub-flags. 16 src dst r m)]
    [(cmp.b src dst) (do-sub-flags.  8 src dst r m)]

    ; r2 is the status register. for simplicity, only bit and cmp affect it indirectly
;    [(bit.w src dst) (let ([x (bvadd (load16 src r m) (load16 dst r m))])
;                       (let ([c (if (bveq x (mspx-bv 0)) (mspx-bv 0) (mspx-bv 1))]
;                             [z (if (bveq x (mspx-bv 0)) (mspx-bv 2) (mspx-bv 0))]
;                             [n (if (bveq (bvand (mspx-bv 32768) x) (mspx-bv 32768)) (mspx-bv 4) (mspx-bv 0))])
;                         (bvand c z n)))]
;    [(bit.b src dst) (let ([x (bvadd (load8 src r m) (load8 dst r m))])
;                       (let ([c (if (bveq x (mspx-bv 0)) (mspx-bv 0) (mspx-bv 1))]
;                             [z (if (bveq x (mspx-bv 0)) (mspx-bv 2) (mspx-bv 0))]
;                             [n (if (bveq (bvand (mspx-bv 128) x) (mspx-bv 128)) (mspx-bv 4) (mspx-bv 0))])
;                         (bvand c z n)))]

;    ; r0 is the stack pointer
;    [(push.w src) (let
;                      ([sp (bvsub (register-ref r 0) (mspx-bv 2))])
;                    (memory-set16! m sp (load16 src r m))
;                    (register-set! r 0 sp))]
    ))

(define (stepn s n)
  ; s is a state, n is the number of steps
  (let ([ip (bitvector->integer (register-ref (state-r s) 0))])
      (begin 
        ; Compute result of instruction at instruction ptr
        (step (vector-ref (state-instrs s) ip) (state-r s) (state-m s))
        ; Update instruction ptr
        (register-set! (state-r s) 0 (bvadd (mspx-bv (+ ip 1))))
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
