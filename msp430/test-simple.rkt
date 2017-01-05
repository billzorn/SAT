#lang rosette/safe

(require rosette/lib/match rosette/lib/synthax "lang-simple.rkt")

(define (symbolic-int)
  (define-symbolic* i integer?)
  i)

(define (symbolic-bv n)
  (define-symbolic* x (bitvector n))
  x)

;; test MOV

(define s (state (halt (list (add.w (idx 0 (bv 6 20))
                                    (abs (bv 4 20)))))
                 (vector (bv 0 20) (bv 0 20) (bv 0 20) (bv 0 20))
                 (vector (bv 0 20) (bv 0 20) (bv 5119 20) (bv 65279 20))
                 (box #t)))

(stepn s 1)

s