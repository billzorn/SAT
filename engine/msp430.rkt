#lang racket

(require "framework.rkt" "mmap.rkt" "mmap_vector.rkt")

(define (msp430-state rn hiaddr)
  (state (vector (new mmap-vector% [hiaddr rn] [zero 0]) (new mmap-vector% [hiaddr hiaddr] [zero 0]))))

(define MAP/REG 0)
(define MAP/MEM 1)

; Sample implementations of the addressing modes. These could also be generated
; by synthesizing the results of several tests on the hardware.
(define (imm a) (constant a))
(define (reg r) (ref MAP/REG (constant r)))
(define (ind r) (ref MAP/MEM (reg r)))
(define (abs a) (ref MAP/MEM (constant a)))
(define (idx r i) (ref MAP/MEM (add (constant i) (reg r))))
