#lang rosette
(require "../lib/bv.rkt")

(define (pass x) x)
(define (eq0 x) (if (bveq x (bv 0 20)) (bv 1 1) (bv 0 1)))
(define (sr-carry sr) (if (bit-set? sr 0) (bv 1 1) (bv 0 1)))
(provide (all-defined-out))

(define (msp-mov.b sr op1 op2) (pass op1))
(define (msp-sr-mov.b sr op1 op2 dst) 
  (concat
    (pass (sr-carry sr))
    (bv 0 1)
    (bv 0 1)
    (bv 0 5)
    (bv 0 1)))
(define (msp-add.b sr op1 op2) (bvadd op1 op2))
(define (msp-sr-add.b sr op1 op2 dst) 
  (concat
    (eq0 (bveq op1 (bvsub dst op2)))
    (eq0 dst)
    (bv 0 1)
    (bv 0 5)
    (bv 0 1)))
(define (msp-sub.b sr op1 op2) (bvsub op2 op1))
(define (msp-sr-sub.b sr op1 op2 dst) 
  (concat
    (bveq dst (bvsub op2 op1))
    (eq0 dst)
    (bv 0 1)
    (bv 0 5)
    (bv 0 1)))
