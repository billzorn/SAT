#lang racket

(provide (all-defined-out))

(define (bitwidth-hint opname)
  (printf "~a\n" opname)
  (cond
    [(string-prefix? opname "dadd") '(4 4 8 8 8)]
    [(string-suffix? opname ".b") 8]
    [(string-suffix? opname ".w") 16]
    [(string-suffix? opname ".a") 20]
    [else (error "not an msp430 op name")]))

(define (strategy-hint opname)
  (case opname
    [("dadd.b" "dadd.w" "dadd.a") '(n4-up n4-up full full full)]
    [else "'full"]))

(define (maxlength-hint opname)
  (case opname
    [("mov.b" "mov.w" "movx.a")     '(1 1 1 1 1)]
    [("add.b" "add.w" "addx.a")     '(1 4 4 4 4)]
    [("sub.b" "sub.w" "subx.a")     '(1 4 4 4 4)]
    [("xor.b" "xor.w" "xorx.a")     '(1 4 4 4 4)]
    [("and.b" "and.w" "andx.a")     '(1 4 4 4 4)]
    [("dadd.b" "dadd.w" "daddx.a")  '(8 8 8 8 8)]
    [else                           '(4 4 4 4 4)]))
