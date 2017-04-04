#lang racket

(provide (all-defined-out))

(define (io-diffs data inputs outputs)
  (define exp-regs (list->set outputs))
  (for/list ([regpair data])
    (let ([in-regs (car regpair)]
          [out-regs (cdr regpair)]
          [expected (box #t)])
      (let ([diff (for/list ([rn (in-range 16)]
                             [in-r in-regs]
                             [out-r out-regs]
                             #:unless (= in-r out-r))
                    (unless (set-member? exp-regs rn) (set-box! expected #f))
                    (list rn in-r out-r))])
        (unless (unbox expected)
          (printf "register change not captured by outptus:\n  inputs: ~a, outputs: ~a\n  diff: ~a\n"
                  inputs outputs diff)))
      (cons
       (for/list ([rn inputs]) (vector-ref in-regs rn))
       (for/list ([rn outputs]) (vector-ref out-regs rn))))))

(define-syntax-rule (compress-sr x)
  (bitwise-ior (bitwise-and x 7) (bitwise-and (arithmetic-shift x -5) 8)))

; assumes sr is already compressed, i.e. sr & 0xf = sr
(define (iotab-idx-fmt1 sr a b)
  (bitwise-ior (arithmetic-shift (bitwise-and sr #xf) 16)
               (arithmetic-shift (bitwise-and a #xff) 8)
               (bitwise-and b #xff)))

; assumes sr is already compressed, i.e. sr & 0xf = sr
(define (iotab-idx-fmt1/sr sr a b)
  (bitwise-ior (arithmetic-shift (bitwise-and (compress-sr sr) #xf) 16)
               (arithmetic-shift (bitwise-and a #xff) 8)
               (bitwise-and b #xff)))

; assumes diffs have three inputs (sr src dst) and two outputs (sr dst)
(define (iotab-fmt1/sr diffs)
  (let ([iotab (make-vector (* 16 256 256) (void))])
    (for ([diff diffs])
      (vector-set! iotab (apply iotab-idx-fmt1/sr (car diff)) (cdr diff)))
    (define missing-outputs 0)
    (for ([output iotab])
      (when (void? output) (set! missing-outputs #f)))
    (when (> missing-outputs 0) (printf "iotab-fmt1/sr: missing ~a outputs\n" missing-outputs))
    iotab))

(define (iotab-lookup-fmt1 iotab inputs)
  (vector-ref iotab (apply iotab-idx-fmt1 inputs)))

(define (iotab-lookup-fmt1/sr iotab inputs)
  (vector-ref iotab (apply iotab-idx-fmt1/sr inputs)))


(define (iotab-split iotab n)
  (apply values
         (for/list ([i (in-range n)])
           (vector-map (lambda (x) (vector-ref x i)) iotab))))
