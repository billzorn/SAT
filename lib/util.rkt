#lang rosette

(provide (all-defined-out))

; unnamed streams of symbolic values

(define (tabulate-list f length)
  (if (> length 0)
      (cons (f) (tabulate-list f (- length 1)))
      null))

(define (symbolic-int)
  (define-symbolic* i integer?)
  i)

(define (symbolic-bv n)
  (define-symbolic* x (bitvector n))
  x)

(define (symbolic-bv-vector n length)
  (define (symbolic-bv-fixed) (symbolic-bv n))
  (list->vector (tabulate-list symbolic-bv-fixed length)))

(define-syntax-rule (iff x y)
  (and (implies x y) (implies y x)))

; Get path to this file, useful if you want to "require"
; things that aren't racket files.
; See: http://stackoverflow.com/questions/16842811/racket-how-to-retrieve-the-path-of-the-running-file
(define-syntax-rule (get-here)
  (let*-values ([(ccr) (current-contract-region)]
                [(here-dir here-name here-isdir) (split-path ccr)])
    here-dir))
