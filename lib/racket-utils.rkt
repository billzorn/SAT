#lang racket

(provide (all-defined-out))

; Get path to this file, useful if you want to "require"
; things that aren't racket files.
; See: http://stackoverflow.com/questions/16842811/racket-how-to-retrieve-the-path-of-the-running-file
(define-syntax-rule (get-here)
  (let-values ([(here-dir here-name here-isdir) (split-path (current-contract-region))])
    here-dir))

; endianness things for racket's integer types
(define (le-word bytepair)
  (bitwise-ior (first bytepair) (arithmetic-shift (second bytepair) 8)))
(define (le-dword bytequartet)
  (bitwise-ior
   (first bytequartet)
   (arithmetic-shift (second bytequartet) 8)
   (arithmetic-shift (third bytequartet) 16)
   (arithmetic-shift (fourth bytequartet) 24)))
(define (le-word->bytes x)
  (list (bitwise-and x #xff) (bitwise-and (arithmetic-shift x -8) #xff)))
(define (le-dword->bytes x)
  (list (bitwise-and x #xff)
        (bitwise-and (arithmetic-shift x -8) #xff)
        (bitwise-and (arithmetic-shift x -16) #xff)
        (bitwise-and (arithmetic-shift x -24) #xff)))

; chunking lists
(define (chunks lst n)
  (let-values ([(chunksize extra) (quotient/remainder (length lst) n)])
    (define sublist-head lst)
    (for/list ([i (in-range n)])
      (let-values ([(h t) (split-at sublist-head (if (< i extra) (+ chunksize 1) chunksize))])
        (set! sublist-head t)
        h))))
      

; read/print directly to strings more easily
(define (sread s) (define i (open-input-string s)) (read i))
