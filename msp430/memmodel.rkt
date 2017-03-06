#lang rosette/safe

(require "../lib/bv.rkt" "lang-base.rkt")

; Normally provided by racket but it's not lifted by rosette
(define (make-vector size init-val)
  (letrec ([f (lambda (l n) (if (> n 0) (f (cons init-val l) (- n 1)) l))])
    (list->vector (f '() size))))
    

; Option 1: Memory is a list of (base, (vector ...)) pairs.
; An address a is in memory if it falls between:
;    b and b + (vector-length v) * 2
; for some segment (b, v)
; Requirement: b must be even

(define-syntax-rule (addr->integer addr)
  (bitvector->integer (bvlshr addr (mspx-bv 1))))


(define-syntax-rule (memory-segment b v)
  (cons b v))

(define-syntax-rule (make-memory-segment b len)
  (memory-segment b (make-vector (/ len 2) (mspx-bv 0))))

(define-syntax build-memory 
  (syntax-rules ()
    [(build-memory seg) (list seg)]
    [(build-memory seg1 seg2 ...) (cons seg1 (build-memory seg2 ...))]))

(define-syntax-rule (address-in-range? a base vlen) (and (>= (* a 2) base) (< (* a 2) (+ base (* 2 vlen)))))

; Get the (raw) a^th word of memory
; (That is, a is some address addr / 2)
(define (memory-get m a)
  (begin (assert (not (null? m)))
  (let* ([seg (car m)]
         [base (car seg)]
         [data (cdr seg)])
    (if (address-in-range? a base (vector-length data))
      (vector-ref data (- a (/ base 2)))
      (memory-get (cdr m) a)))))

; Set the (raw) a^th word of memory
; (That is, a is some address addr / 2)
(define (memory-set! m a val)
  (begin (assert (not (null? m)))
  (let* ([seg (car m)]
         [base (car seg)]
         [data (cdr seg)])
    (if (address-in-range? a base (vector-length data))
      (vector-set! data (- a (/ base 2)) val)
      (memory-set! (cdr m) a val)))))

; Set the (raw) a^th word of memory, adding an entry to the segment list if necessary
; NOTE: this function returns a new segment list! Unlike memory-set!, you'll
; need to update your memory variable if you use this function.
; TODO Inefficient implementation, doesn't check if the entry is adjacent to an
; existing segment
(define (memory-insert! m a val)
  (if (null? m)
    (list (memory-segment (* 2 a) (vector val)))
    (let* ([seg (car m)]
           [base (car seg)]
           [data (cdr seg)])
      (if (address-in-range? a base (vector-length data))
        (begin (vector-set! data (- a (/ base 2)) val) m)
        (cons seg (memory-insert! (cdr m) a val))))))

(define-syntax-rule (memory-ref20 m addr)
  (let ([loword (memory-get m (addr->integer addr))]
        [hiword (memory-get m (+ (addr->integer addr) 1))])
    (bvor (trunc16 loword) (bvshl (bvand hiword (mspx-bv #x0000f)) (mspx-bv 16)))))

(define-syntax-rule (memory-ref16 m addr)
  (trunc16 (memory-get m (addr->integer addr))))

(define-syntax-rule (memory-ref8 m addr)
  (if (bveq (trunc1 addr) (mspx-bv 0))
      (trunc8 (memory-get m (addr->integer addr)))
      (high8->low (memory-get m (addr->integer addr)))))


; Option 2: Option 1 but use a segment vector instead of a segment list
; Option 3: Option 1 but use a mutable list so it can grow without needing to be reassigned
; Option 4: Some kind of segment tree structure
