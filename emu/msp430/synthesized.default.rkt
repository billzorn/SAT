#lang rosette
(require "../../lib/bv.rkt")
(require "../../lib/bv-operations.rkt")
(provide (all-defined-out))

(define (msp-mov.b sr op1 op2) (pass op1))
(define (msp-sr-mov.b sr op1 op2 dst) 
  (concat
    (pass (sr-carry sr))
    (pass (sr-zero sr))
    (pass (sr-negative sr))
    (bv 0 5)
    (pass (sr-overflow sr))))
(define (msp-add.b sr op1 op2) (bvadd op1 op2))
(define (msp-sr-add.b sr op1 op2 dst) 
  (concat
    (bit9 (bvsub dst op2))
    (eq0 dst)
    (bit8 dst)
    (bv 0 5)
    (bvand (samesign8 op1 op2) (diffsign8 dst op1))))
(define (msp-addc.b sr op1 op2) (bvadd op1 (bvadd (sr-carry20 sr) op2)))
(define (msp-sr-addc.b sr op1 op2 dst) 
  (concat
    (bit9 (bvadd op2 (bvadd (sr-carry sr) op1)))
    (eq0 dst)
    (bit8 dst)
    (bv 0 5)
    (bvand (diffsign8 op1 dst) (samesign8 op2 op1))))
(define (msp-sub.b sr op1 op2) (bvsub op2 op1))
(define (msp-sr-sub.b sr op1 op2 dst) 
  (concat
    (bveq2 dst (bvsub op2 op1))
    (eq0 dst)
    (bit8 dst)
    (bv 0 5)
    (bvand (samesign8 op1 dst) (diffsign8 op2 dst))))
(define (msp-subc.b sr op1 op2) (bvadd op2 (bvnot (bvsub op1 (sr-carry20 sr)))))
(define (msp-sr-subc.b sr op1 op2 dst) 
  (concat
    (bit9 (bvsub (bvsub dst (sr-carry sr)) op2))
    (eq0 dst)
    (bit8 dst)
    (bv 0 5)
    (bvand (samesign8 op1 dst) (diffsign8 dst op2))))
(define (msp-cmp.b sr op1 op2) (pass op2))
(define (msp-sr-cmp.b sr op1 op2 dst) 
  (concat
    (eq0 (bit9 (bvsub dst op1)))
    (bveq2 op1 op2)
    (bit8 (bvsub op2 op1))
    (bv 0 5)
    (bvand (diffsign8 op1 dst) (samesign8 (bvsub dst op1) op1))))
(define (msp-dadd.b sr op1 op2) (mspx-bv 0))
(define (msp-sr-dadd.b sr op1 op2 dst) 
  (concat
    (bv 0 1)
    (eq0 dst)
    (bit8 dst)
    (bv 0 5)
    (bvand (diffsign8 dst op2) (samesign8 op2 op1))))
(define (msp-bit.b sr op1 op2) (pass op2))
(define (msp-sr-bit.b sr op1 op2 dst) 
  (concat
    (eq0 (eq0 (bvand op1 op2)))
    (eq0 (bvand op1 dst))
    (bit8 (bvand op1 dst))
    (bv 0 5)
    (bit9 op1)))
(define (msp-bic.b sr op1 op2) (bvand op2 (bvnot op1)))
(define (msp-sr-bic.b sr op1 op2 dst) 
  (concat
    (bvand (sr-carry sr) (sr-carry sr))
    (bv 0 1)
    (bv 0 1)
    (bv 0 5)
    (bv 0 1)))
(define (msp-bis.b sr op1 op2) (bvor op1 op2))
(define (msp-sr-bis.b sr op1 op2 dst) 
  (concat
    (pass (sr-carry sr))
    (bv 0 1)
    (bv 0 1)
    (bv 0 5)
    (bv 0 1)))
(define (msp-xor.b sr op1 op2) (bvxor op1 op2))
(define (msp-sr-xor.b sr op1 op2 dst) 
  (concat
    (bit9 (bvneg dst))
    (eq0 dst)
    (bit8 dst)
    (bv 0 5)
    (bit8 (bvand op1 op2))))
(define (msp-and.b sr op1 op2) (bvand op1 op2))
(define (msp-sr-and.b sr op1 op2 dst) 
  (concat
    (bit9 (bvneg dst))
    (eq0 dst)
    (bit8 dst)
    (bv 0 5)
    (bit9 op1)))
(define (msp-mov.w sr op1 op2) (pass op1))
(define (msp-sr-mov.w sr op1 op2 dst) 
  (concat
    (pass (sr-carry sr))
    (pass (sr-zero sr))
    (pass (sr-negative sr))
    (bv 0 5)
    (pass (sr-overflow sr))))
(define (msp-add.w sr op1 op2) (bvadd op1 op2))
(define (msp-sr-add.w sr op1 op2 dst) 
  (concat
    (bit17 (bvsub dst op2))
    (eq0 dst)
    (bit16 dst)
    (bv 0 5)
    (bit16 (bvand (bvxor op1 dst) (bvxor op2 dst)))))
(define (msp-addc.w sr op1 op2) (bvadd op1 (bvadd (sr-carry20 sr) op2)))
(define (msp-sr-addc.w sr op1 op2 dst) 
  (concat
    (bit17 (bvadd op1 (bvor (sr-carry20 sr) op2)))
    (eq0 dst)
    (bit16 dst)
    (bv 0 5)
    (bit16 (bvand (bvxor op1 dst) (bvxor op2 dst)))))
(define (msp-sub.w sr op1 op2) (bvsub op2 op1))
(define (msp-sr-sub.w sr op1 op2 dst) 
  (concat
    (bveq2 dst (bvsub op2 op1))
    (eq0 dst)
    (bit16 dst)
    (bv 0 5)
    (bvand (samesign16 dst op1) (diffsign16 op2 dst))))
(define (msp-subc.w sr op1 op2) (bvnot (bvsub (bvsub op1 (sr-carry20 sr)) op2)))
(define (msp-sr-subc.w sr op1 op2 dst) 
  (concat
    (bit17 (bvsub op1 (bvadd (sr-carry20 sr) op2)))
    (eq0 dst)
    (bit16 dst)
    (bv 0 5)
    (bvand (samesign16 op1 dst) (diffsign16 dst op2))))

(define (msp-cmp.w sr op1 op2) (pass op2))
(define (msp-sr-cmp.w sr op1 op2 dst) 
  (concat
    (eq0 (bit17 (bvsub dst op1)))
    (bveq2 op1 dst)
    (bit16 (bvsub dst op1))
    (bv 0 5)
    (bvand (diffsign16 op1 dst) (samesign16 (bvsub dst op1) op1))))
(define (msp-bit.w sr op1 op2) (pass op2))
(define (msp-sr-bit.w sr op1 op2 dst) 
  (concat
    (bit17 (bvneg (bvand op1 op2)))
    (eq0 (bvand op1 dst))
    (bit16 (bvand op1 dst))
    (bv 0 5)
    (bit17 op1)))
(define (msp-bic.w sr op1 op2) (bvand op2 (bvnot op1)))
(define (msp-sr-bic.w sr op1 op2 dst) 
  (concat
    (pass (sr-carry sr))
    (sr-zero sr)
    (sr-negative sr)
    (bv 0 5)
    (sr-overflow sr)))
(define (msp-and.w sr op1 op2) (bvand op1 op2))
(define (msp-sr-and.w sr op1 op2 dst) 
  (concat
    (eq0 (eq0 dst))
    (eq0 dst)
    (bit16 dst)
    (bv 0 5)
    (bit17 op1)))
(define (msp-xor.w sr op1 op2) (bvxor op1 op2))
(define (msp-sr-xor.w sr op1 op2 dst) 
  (concat
    (bit17 (bvneg dst))
    (eq0 dst)
    (bit16 dst)
    (bv 0 5)
    (bit16 (bvand op1 op2))))
(define (msp-mov bw sr op1 op2)
  (case bw
    [(8) (msp-mov.b sr op1 op2)]
    [(16) (msp-mov.w sr op1 op2)]
    [else (mspx-bv 0)]))
(define (msp-sr-mov bw sr op1 op2 dst)
  (case bw
    [(8) (msp-sr-mov.b sr op1 op2 dst)]
    [(16) (msp-sr-mov.w sr op1 op2 dst)]
    [else (mspx-bv 0)]))
(define (msp-add bw sr op1 op2)
  (case bw
    [(8) (msp-add.b sr op1 op2)]
    [(16) (msp-add.w sr op1 op2)]
    [else (mspx-bv 0)]))
(define (msp-sr-add bw sr op1 op2 dst)
  (case bw
    [(8) (msp-sr-add.b sr op1 op2 dst)]
    [(16) (msp-sr-add.w sr op1 op2 dst)]
    [else (mspx-bv 0)]))
(define (msp-addc bw sr op1 op2)
  (case bw
    [(8) (msp-addc.b sr op1 op2)]
    [(16) (msp-addc.w sr op1 op2)]
    [else (mspx-bv 0)]))
(define (msp-sr-addc bw sr op1 op2 dst)
  (case bw
    [(8) (msp-sr-addc.b sr op1 op2 dst)]
    [(16) (msp-sr-addc.w sr op1 op2 dst)]
    [else (mspx-bv 0)]))
(define (msp-sub bw sr op1 op2)
  (case bw
    [(8) (msp-sub.b sr op1 op2)]
    [(16) (msp-sub.w sr op1 op2)]
    [else (mspx-bv 0)]))
(define (msp-sr-sub bw sr op1 op2 dst)
  (case bw
    [(8) (msp-sr-sub.b sr op1 op2 dst)]
    [(16) (msp-sr-sub.w sr op1 op2 dst)]
    [else (mspx-bv 0)]))
(define (msp-subc bw sr op1 op2)
  (case bw
    [(8) (msp-subc.b sr op1 op2)]
    [(16) (msp-subc.w sr op1 op2)]
    [else (mspx-bv 0)]))
(define (msp-sr-subc bw sr op1 op2 dst)
  (case bw
    [(8) (msp-sr-subc.b sr op1 op2 dst)]
    [(16) (msp-sr-subc.w sr op1 op2 dst)]
    [else (mspx-bv 0)]))
(define (msp-cmp bw sr op1 op2)
  (case bw
    [(8) (msp-cmp.b sr op1 op2)]
    [(16) (msp-cmp.w sr op1 op2)]
    [else (mspx-bv 0)]))
(define (msp-sr-cmp bw sr op1 op2 dst)
  (case bw
    [(8) (msp-sr-cmp.b sr op1 op2 dst)]
    [(16) (msp-sr-cmp.w sr op1 op2 dst)]
    [else (mspx-bv 0)]))
(define (msp-dadd bw sr op1 op2)
  (case bw
    [(8) (msp-dadd.b sr op1 op2)]
    ;[(16) (msp-dadd.w sr op1 op2)]
    [else (mspx-bv 0)]))
(define (msp-sr-dadd bw sr op1 op2 dst)
  (case bw
    [(8) (msp-sr-dadd.b sr op1 op2 dst)]
    ;[(16) (msp-sr-dadd.w sr op1 op2 dst)]
    [else (mspx-bv 0)]))
(define (msp-bit bw sr op1 op2)
  (case bw
    [(8) (msp-bit.b sr op1 op2)]
    [(16) (msp-bit.w sr op1 op2)]
    [else (mspx-bv 0)]))
(define (msp-sr-bit bw sr op1 op2 dst)
  (case bw
    [(8) (msp-sr-bit.b sr op1 op2 dst)]
    [(16) (msp-sr-bit.w sr op1 op2 dst)]
    [else (mspx-bv 0)]))
(define (msp-bic bw sr op1 op2)
  (case bw
    [(8) (msp-bic.b sr op1 op2)]
    [(16) (msp-bic.w sr op1 op2)]
    [else (mspx-bv 0)]))
(define (msp-sr-bic bw sr op1 op2 dst)
  (case bw
    [(8) (msp-sr-bic.b sr op1 op2 dst)]
    [(16) (msp-sr-bic.w sr op1 op2 dst)]
    [else (mspx-bv 0)]))
(define (msp-bis bw sr op1 op2)
  (case bw
    [(8) (msp-bis.b sr op1 op2)]
    ;[(16) (msp-bis.w sr op1 op2)]
    [else (mspx-bv 0)]))
(define (msp-sr-bis bw sr op1 op2 dst)
  (case bw
    [(8) (msp-sr-bis.b sr op1 op2 dst)]
    ;[(16) (msp-sr-bis.w sr op1 op2 dst)]
    [else (mspx-bv 0)]))
(define (msp-xor bw sr op1 op2)
  (case bw
    [(8) (msp-xor.b sr op1 op2)]
    [(16) (msp-xor.w sr op1 op2)]
    [else (mspx-bv 0)]))
(define (msp-sr-xor bw sr op1 op2 dst)
  (case bw
    [(8) (msp-sr-xor.b sr op1 op2 dst)]
    [(16) (msp-sr-xor.w sr op1 op2 dst)]
    [else (mspx-bv 0)]))
(define (msp-and bw sr op1 op2)
  (case bw
    [(8) (msp-and.b sr op1 op2)]
    [(16) (msp-and.w sr op1 op2)]
    [else (mspx-bv 0)]))
(define (msp-sr-and bw sr op1 op2 dst)
  (case bw
    [(8) (msp-sr-and.b sr op1 op2 dst)]
    [(16) (msp-sr-and.w sr op1 op2 dst)]
    [else (mspx-bv 0)]))
