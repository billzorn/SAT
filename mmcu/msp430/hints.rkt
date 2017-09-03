#lang racket

(define (bitwidth opname)
  (cond
    [(string-suffix? opname ".b") 8]
    [(string-suffix? opname ".w") 16]
    [(string-suffix? opname ".a") 20]
    [else (error "not an msp430 op name")]))

(define (strategy opname)
  (case opname
    [("dadd.b" "dadd.w" "dadd.a") "n4-up"]
    [else "full"]))

(define (maxlength opname)
  (case opname
    [("mov.b" "mov.w" "movx.a")     '(1 1 1 1 1)]
    [("add.b" "add.w" "addx.a")     '(1 4 4 4 4)]
    [("sub.b" "sub.w" "subx.a")     '(1 4 4 4 4)]
    [("xor.b" "xor.w" "xorx.a")     '(1 4 4 4 4)]
    [("and.b" "and.w" "andx.a")     '(1 4 4 4 4)]
    [("dadd.b" "dadd.w" "daddx.a")  '(8 8 8 8 8)]
    [else                           '(4 4 4 4 4)]
