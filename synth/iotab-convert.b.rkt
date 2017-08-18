#lang racket
(require "../meas/process-measurements.rkt")
(require "data/iotabs.rkt")
(require racket/cmdline)

(define (sample.b iotab sr a b)
  (cons (list sr a b) 
    (iotab-lookup-fmt1 iotab (list sr a b))))

(define (dump-tab iotab)
  (for* ([a (in-range 256)] 
         [b (in-range 256)] 
         [sr (in-range 16)])
    (let* ([sr-uncompressed (if (> sr 7) (+ (- sr 8) 256) sr)]
           [sample (sample.b iotab sr a b)])
      (printf "(~a . ~a)\n" (cons sr-uncompressed (cdr (car sample))) (cdr sample)))))

(with-output-to-file "data/test/mov.b.rkt"  (thunk (dump-tab mov.b )) #:exists 'truncate)
(with-output-to-file "data/test/add.b.rkt"  (thunk (dump-tab add.b )) #:exists 'truncate)
(with-output-to-file "data/test/addc.b.rkt" (thunk (dump-tab addc.b)) #:exists 'truncate)
(with-output-to-file "data/test/subc.b.rkt" (thunk (dump-tab subc.b)) #:exists 'truncate)
(with-output-to-file "data/test/sub.b.rkt"  (thunk (dump-tab sub.b )) #:exists 'truncate)
(with-output-to-file "data/test/cmp.b.rkt"  (thunk (dump-tab cmp.b )) #:exists 'truncate)
(with-output-to-file "data/test/dadd.b.rkt" (thunk (dump-tab dadd.b)) #:exists 'truncate)
(with-output-to-file "data/test/bit.b.rkt"  (thunk (dump-tab bit.b )) #:exists 'truncate)
(with-output-to-file "data/test/bic.b.rkt"  (thunk (dump-tab bic.b )) #:exists 'truncate)
(with-output-to-file "data/test/bis.b.rkt"  (thunk (dump-tab bis.b )) #:exists 'truncate)
(with-output-to-file "data/test/xor.b.rkt"  (thunk (dump-tab xor.b )) #:exists 'truncate)
(with-output-to-file "data/test/and.b.rkt"  (thunk (dump-tab and.b )) #:exists 'truncate)

;; read, individual vals
;(define (load-iotab file)
;  (with-input-from-file file (λ () 
;    (let ([tab (make-hash)] [pair (read)])
;      (for ([i (in-naturals)] #:break (eof-object? pair))
;        (hash-set! tab (first pair) (second pair))
;        (set! pair (read)))
;      tab))))
;
;(define add.w (load-iotab "data/test/add.w.lines.rkt"))
;(define sub.w (load-iotab "data/test/sub.w.lines.rkt"))
;(printf "add.w: ~a\n" (hash-count add.w))
;(printf "sub.w: ~a\n" (hash-count sub.w))

