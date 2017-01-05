#lang rosette/safe

(require rosette/lib/match)

(provide (except-out (all-defined-out) define-instruction define-load-syntax define-store-syntax))

(current-bitwidth 20)

; instruction set representation

; operands
(struct op () #:transparent)
(struct reg op (r) #:transparent)
(struct imm op (i) #:transparent)
(struct abs op (addr) #:transparent)
(struct idx op (r i) #:transparent)

; executable instructions
(struct instruction () #:transparent)
(struct fmt1 instruction (op1 op2) #:transparent)
(struct fmt2 instruction (op1) #:transparent)

; macro that allows us to define instructions
(define-syntax (define-instruction stx)
  (syntax-case stx ()
    [(_ [id kind])
     #'(begin (struct id kind () #:transparent))]
    [(_ [id kind] more ...)
     #'(begin
         (define-instruction [id kind])
         (define-instruction more ...))]))

; actual definitions of instructions
(define-instruction
  [mov.w fmt1] [mov.b fmt1]
  [add.w fmt1] [add.b fmt1]
  ; no addc
  [sub.w fmt1] [sub.b fmt1]
  ; no subc
  [cmp.w fmt1] [cmp.b fmt1]
  ; no dadd
  [bit.w fmt1] [bit.b fmt1]
  [bic.w fmt1] [bic.b fmt1]
  [bis.w fmt1] [bis.b fmt1]
  [xor.w fmt1] [xor.b fmt1]
  [and.w fmt1] [and.b fmt1]
  
  [push.w fmt2] [push.b fmt2]
  [pop.w fmt2] [pop.b fmt2] ; normally emulated, explicit due to lack of ai mode
  ; no rrc
  [rra.w fmt2] [rra.b fmt2]
  [swpb fmt2]
  [sxt fmt2]
  ; no call
  ; no reti

  ; jumps are a different kind of structure
  )

; manage control flow with a structures that represent basic blocks
; we can't model call, because return blocks have many successors
(struct bblock () #:transparent)
(struct halt bblock (instrs) #:transparent)
(struct jump bblock (instrs branch-condition taken-block untaken-block) #:transparent)

; representation of branch conditions
(struct br () #:transparent)

; representation of jump instructions
(define-instruction
  [jz br] [jnz br] [jc br] [jnc br] [jn br] [jge br] [jl br] [jmp br])

; representation of complete state
(struct state (entry-block r m running) #:transparent)

; execution helper macros

; truncation
(define-syntax-rule (trunc16 x)
  (bvand x (bv #x0ffff 20)))

(define-syntax-rule (trunc8 x)
  (bvand x (bv #x000ff 20)))

(define-syntax-rule (high8 x)
  (trunc8 (bvlshr x (bv 8 20))))

(define-syntax-rule (trunc1 x)
  (bvand x (bv #x00001 20)))

; address lookup
(define-syntax-rule (addr->integer addr)
  (bitvector->integer (bvlshr addr (bv 1 20))))

; memory dereference
(define-syntax-rule (memory-ref16 memory addr)
  (trunc16 (vector-ref memory (addr->integer addr))))

(define-syntax-rule (memory-ref8 memory addr)
  (if (bveq (trunc1 addr) (bv 0 20))
      (trunc8 (vector-ref memory (addr->integer addr)))
      (high8 (vector-ref memory (addr->integer addr)))))

; memory assignment
(define-syntax-rule (memory-set16! memory addr x)
  (vector-set! memory (addr->integer addr) (trunc16 x)))

; this is horrible
(define-syntax-rule (memory-set8! memory addr x)
  (let ([m (vector-ref memory (addr->integer addr))])
    (if (bveq (trunc1 addr) (bv 0 20))
        (vector-set! memory (addr->integer addr) (bvor (high8 m) (trunc8 x)))
        (vector-set! memory (addr->integer addr) (bvor (bvshl (trunc8 x) (bv 0 20)) (trunc8 m))))))

; register dereference
(define-syntax-rule (register-ref registers r)
  (vector-ref registers r))

(define-syntax-rule (register-ref16 registers r)
  (trunc16 (register-ref registers r)))

(define-syntax-rule (register-ref8 registers r)
  (trunc8 (register-ref registers r)))

; register assignment
(define-syntax-rule (register-set! registers r x)
  (vector-set! registers r x))

(define-syntax-rule (register-set16! registers r x)
  (vector-set! registers r (trunc16 x)))

(define-syntax-rule (register-set8! registers r x)
  (vector-set! registers r (trunc8 x)))

; addressing mode matches as macros

; master load macro
(define-syntax (define-load-syntax stx)
  (syntax-case stx ()
    [(_ [id register-refx memory-refx truncx])
     #'(begin (define-syntax-rule (id op registers memory)
                (match op
                  [(reg r)     (register-refx registers r)]
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
                  [(reg r)     (register-setx! registers r x)]
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
    [(add.w src dst) (store16 (bvadd (load16 src r m) (load16 dst r m)) dst r m)]
    [(add.b src dst) (store8 (bvadd (load8 src r m) (load8 dst r m)) dst r m)]
    [(sub.w src dst) (store16 (bvsub (load16 dst r m) (load16 src r m)) dst r m)]
    [(sub.b src dst) (store8 (bvsub (load8 dst r m) (load8 src r m)) dst r m)]
    ; r1 is the status register. for simplicity, only bit and cmp affect it indirectly
    [(bit.w src dst) (let ([x (bvadd (load16 src r m) (load16 dst r m))])
                       (let ([c (if (bveq x (bv 0 20)) (bv 0 20) (bv 1 20))]
                             [z (if (bveq x (bv 0 20)) (bv 2 20) (bv 0 20))]
                             [n (if (bveq (bvand (bv 32768 20) x) (bv 32768 20)) (bv 4 20) (bv 0 20))])
                         (bvand c z n)))]
    [(bit.b src dst) (let ([x (bvadd (load8 src r m) (load8 dst r m))])
                       (let ([c (if (bveq x (bv 0 20)) (bv 0 20) (bv 1 20))]
                             [z (if (bveq x (bv 0 20)) (bv 2 20) (bv 0 20))]
                             [n (if (bveq (bvand (bv 128 20) x) (bv 128 20)) (bv 4 20) (bv 0 20))])
                         (bvand c z n)))]
    ; extraction for comparison ends up being kind of nasty...
    [(cmp.w src dst) (let ([srcval (load16 src r m)]
                           [dstval (load16 dst r m)])
                       (let ([x (bvsub dstval srcval)])
                         (let ([src16 (extract 15 0 srcval)]
                               [dst16 (extract 15 0 dstval)]
                               [x16 (extract 15 0 x)])
                           (let ([c (if (bveq (bvand (bv 65536 20) x) (bv 65536 20)) (bv 1 20) (bv 0 20))]
                                 [z (if (bveq x (bv 0 20)) (bv 2 20) (bv 0 20))]
                                 [n (if (bvsgt src16 dst16) (bv 4 20) (bv 0 20))]
                                 [v (if (or (and (bvslt src16 (bv 0 16)) (bvsge dst16 (bv 0 16)) (bvslt x16 (bv 0 16)))
                                            (and (bvsge src16 (bv 0 16)) (bvslt dst16 (bv 0 16)) (bvsge x16 (bv 0 16))))
                                        (bv 256 20) (bv 0 20))])
                         (bvand c z n v)))))]
    [(cmp.b src dst) (let ([srcval (load8 src r m)]
                           [dstval (load8 dst r m)])
                       (let ([x (bvsub dstval srcval)])
                         (let ([src8 (extract 7 0 srcval)]
                               [dst8 (extract 7 0 dstval)]
                               [x8 (extract 7 0 x)])
                           (let ([c (if (bveq (bvand (bv 256 20) x) (bv 256 20)) (bv 1 20) (bv 0 20))]
                                 [z (if (bveq x (bv 0 20)) (bv 2 20) (bv 0 20))]
                                 [n (if (bvsgt src8 dst8) (bv 4 20) (bv 0 20))]
                                 [v (if (or (and (bvslt src8 (bv 0 8)) (bvsge dst8 (bv 0 8)) (bvslt x8 (bv 0 8)))
                                            (and (bvsge src8 (bv 0 8)) (bvslt dst8 (bv 0 8)) (bvsge x8 (bv 0 8))))
                                        (bv 256 20) (bv 0 20))])
                         (bvand c z n v)))))]
                           
    
    ; r0 is the stack pointer
    [(push.w src) (let
                      ([sp (bvsub (register-ref r 0) (bv 2 20))])
                    (memory-set16! m sp (load16 src r m))
                    (register-set! r 0 sp))]))

(define (stepn s n)
  (when (> n 0)
    (match s
      [(state block r m running)
       (match block
         [(halt instrs)
          (match instrs
            [(cons instr rest)
             (step instr r m)
             (stepn (state (halt rest) r m running) (- n 1))]
            ['()
             (set-box! running #f)])]
         [(jump instrs branch-condition taken-block untaken-block)
          (match instrs
            [(cons instr rest)
             (step instr r m)
             (stepn (state (jump rest branch-condition taken-block untaken-block) r m running) (- n 1))]
            ['()
             (match branch-condition
               [(jz) (stepn (state (if (bveq (bvand (register-ref r 1) (bv 2 20)) (bv 2 20))
                                       taken-block
                                       untaken-block)
                                   r m running)
                            (- n 1))]
               [(jz) (stepn (state (if (bveq (bvand (register-ref r 1) (bv 2 20)) (bv 2 20))
                                       untaken-block
                                       taken-block)
                                   r m running)
                            (- n 1))]
               [(jmp) (stepn (state taken-block r m running)
                             (- n 1))])])])])))
