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
(struct im1 op () #:transparent)     ; "#1"
(struct ind op (r) #:transparent)    ; "@Rn"
(struct imm2 op (i) #:transparent)   ; "#@N"
(struct ai op (r) #:transparent)     ; "@Rn+"
(struct imm op (i) #:transparent)    ; "#N"

; executable instructions
(struct instruction () #:transparent)
(struct fmt1 instruction (op1 op2) #:transparent)
(struct fmt2 instruction (op1) #:transparent)

; actual definitions of instructions
(define-instruction
  [mov.w fmt1] [mov.b fmt1]
  [add.w fmt1] [add.b fmt1]
  ; no addc yet
  [sub.w fmt1] [sub.b fmt1]
  ; no subc yet
  [cmp.w fmt1] [cmp.b fmt1]
  ; no dadd yet
  [bit.w fmt1] [bit.b fmt1]
  [bic.w fmt1] [bic.b fmt1]
  [bis.w fmt1] [bis.b fmt1]
  [xor.w fmt1] [xor.b fmt1]
  [and.w fmt1] [and.b fmt1]

  [push.w fmt2] [push.b fmt2]
  [pop.w fmt2] [pop.b fmt2] ; normally emulated, explicit due to lack of ai mode
  ; no rrc yet
  [rra.w fmt2] [rra.b fmt2]
  [swpb fmt2]
  [sxt fmt2]
  ; no call yet
  ; no reti yet

  ; need jumps somewhere
  )

; representation of complete state
; instrs is a vector of instructions representing the program.
(struct state (instrs r m running) #:transparent)

; master load macro
(define-syntax (define-load-syntax stx)
  (syntax-case stx ()
    [(_ [id register-refx memory-refx truncx])
     #'(begin (define-syntax-rule (id op registers memory)
                (match op
                  [(reg r)    (register-refx registers r)]
                  [(imm i)    (truncx i)]
                  [(abs addr) (memory-refx memory addr)]
                  [(idx r i)  (memory-refx memory (bvadd (register-ref registers r) i))])))]
    [(_ [id register-refx memory-refx truncx] more ...)
     #'(begin
         (define-load-syntax [id register-refx memory-refx truncx])
         (define-load-syntax more ...))]))

; define 16-bit and 8-bit load macros
(define-load-syntax
  [load16 register-ref16 memory-ref16 trunc16]
  [load8 register-ref8 memory-ref8 trunc8])

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

; interpreter step function
; this macro expands into a huge mess which inlines the entire logic of the step function
(define (step instr r m)
  (match instr
    [(mov.w src dst) (store16 (load16 src r m) dst r m)]
    [(mov.b src dst) (store8 (load8 src r m) dst r m)]
;    [(add.w src dst) (store16 (bvadd (load16 src r m) (load16 dst r m)) dst r m)]
;    [(add.b src dst) (store8 (bvadd (load8 src r m) (load8 dst r m)) dst r m)]
    [(sub.w src dst) (store16 (bvsub (load16 dst r m) (load16 src r m)) dst r m)]
    [(sub.b src dst) (store8 (bvsub (load8 dst r m) (load8 src r m)) dst r m)]
    ; r1 is the status register. for simplicity, only bit and cmp affect it indirectly
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
    ; extraction for comparison ends up being kind of nasty...
    [(cmp.w src dst) (let ([srcval (load16 src r m)]
                           [dstval (load16 dst r m)])
                       (let ([x (bvsub dstval srcval)])
                         (let ([src16 (mspx->word srcval)]
                               [dst16 (mspx->word dstval)]
                               [x16 (mspx->word x)])
                           (let ([c (if (bveq (bvand (mspx-bv 65536) x) (mspx-bv 65536)) (mspx-bv 1) (mspx-bv 0))]
                                 [z (if (bveq x (mspx-bv 0)) (mspx-bv 2) (mspx-bv 0))]
                                 [n (if (bvsgt src16 dst16) (mspx-bv 4) (mspx-bv 0))]
                                 [v (if (or (and (bvslt src16 (word 0)) (bvsge dst16 (word 0)) (bvslt x16 (word 0)))
                                            (and (bvsge src16 (word 0)) (bvslt dst16 (word 0)) (bvsge x16 (word 0))))
                                        (mspx-bv 256) (mspx-bv 0))])
                         (bvand c z n v)))))]
    [(cmp.b src dst) (let ([srcval (load8 src r m)]
                           [dstval (load8 dst r m)])
                       (let ([x (bvsub dstval srcval)])
                         (let ([src8 (mspx->byte srcval)]
                               [dst8 (mspx->byte dstval)]
                               [x8 (mspx->byte x)])
                           (let ([c (if (bveq (bvand (mspx-bv 256) x) (mspx-bv 256)) (mspx-bv 1) (mspx-bv 0))]
                                 [z (if (bveq x (mspx-bv 0)) (mspx-bv 2) (mspx-bv 0))]
                                 [n (if (bvsgt src8 dst8) (mspx-bv 4) (mspx-bv 0))]
                                 [v (if (or (and (bvslt src8 (byte 0)) (bvsge dst8 (byte 0)) (bvslt x8 (byte 0)))
                                            (and (bvsge src8 (byte 0)) (bvslt dst8 (byte 0)) (bvsge x8 (byte 0))))
                                        (mspx-bv 256) (mspx-bv 0))])
                         (bvand c z n v)))))]


;    ; r0 is the stack pointer
;    [(push.w src) (let
;                      ([sp (bvsub (register-ref r 0) (mspx-bv 2))])
;                    (memory-set16! m sp (load16 src r m))
;                    (register-set! r 0 sp))]
    ))

(define (stepn s n)
  ; s is a state, n is the number of steps
  ; placeholder implementation:
  (set-box! (state-running s) #f)
  )

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
