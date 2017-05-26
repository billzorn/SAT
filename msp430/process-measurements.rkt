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

; leaves all inputs and outputs as-is, keeps last instances of duplicate inputs
(define (iotab-hash/sr diffs)
  (make-immutable-hash diffs))

(define-syntax-rule (compress-sr x)
  (bitwise-ior (bitwise-and x 7) (bitwise-and (arithmetic-shift x -5) 8)))

; assumes sr is already compressed, i.e. sr & 0xf = sr
(define (iotab-idx-fmt1 sr a b)
  (bitwise-ior (arithmetic-shift (bitwise-and sr #xf) 16)
               (arithmetic-shift (bitwise-and a #xff) 8)
               (bitwise-and b #xff)))

; assumes sr is not compressed, i.e. 0xv00000nzc
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
      (when (void? output) (set! missing-outputs (+ missing-outputs 0))))
    (when (> missing-outputs 0) (printf "iotab-fmt1/sr: missing ~a outputs\n" missing-outputs))
    iotab))

(define (iotab-lookup-fmt1 iotab inputs)
  (vector-ref iotab (apply iotab-idx-fmt1 inputs)))

(define (iotab-lookup-fmt1/sr iotab inputs)
  (vector-ref iotab (apply iotab-idx-fmt1/sr inputs)))

(define (sr-carry sr)                       (bitwise-and sr #b000000001))
(define (sr-zero sr)      (arithmetic-shift (bitwise-and sr #b000000010) -1))
(define (sr-neg sr)       (arithmetic-shift (bitwise-and sr #b000000100) -2))
(define (sr-overflow sr)  (arithmetic-shift (bitwise-and sr #b100000000) -8))

(define (iotab-entry-separate iotab-entry)
  (let ([sr (first iotab-entry)]
        [val (second iotab-entry)])
    (list val (sr-carry sr) (sr-zero sr) (sr-neg sr) (sr-overflow sr))))

; assumes c is just one bit (the carry)
(define (iotab-idx-n4 c a b)
  (bitwise-ior (arithmetic-shift (bitwise-and c #x1) 8)
               (arithmetic-shift (bitwise-and a #xf) 4)
               (bitwise-and b #xf)))

(define (iotab-fmt1->n4 iotab)
  (let ([ntab (make-vector (* 2 16 16) (void))])
    (for* ([c (in-range 2)]
           [a (in-range 16)]
           [b (in-range 16)])
      (let ([v (second (iotab-lookup-fmt1 iotab (list c a b)))])
        (vector-set! ntab (iotab-idx-n4 c a b) (list (bitwise-and (arithmetic-shift v -4) #x1)
                                                     (bitwise-and v #xf)))))
    (define missing-outputs 0)
    (for ([output ntab])
      (when (void? output) (set! missing-outputs (+ missing-outputs 0))))
    (when (> missing-outputs 0) (printf "iotab-fmt1->n4: missing ~a outputs\n" missing-outputs))
    ntab))

(define (iotab-lookup-n4 ntab inputs)
  (vector-ref ntab (apply iotab-idx-n4 inputs)))


(define (iotab-split iotab n)
  (apply values
         (for/list ([i (in-range n)])
           (vector-map (lambda (x) (list-ref x i)) iotab))))

(define (iotab-sample iotab sr a b)
  (append (list (sr-carry sr) a b) 
    (iotab-entry-separate (iotab-lookup-fmt1 iotab (list sr a b)))))

(define (iotab-fmt1-sample iotab nsamples)
  (vector-append
    ; include a couple sanity check values
    ;(vector (iotab-sample iotab 0 0 0))
    ;(vector (iotab-sample iotab 0 #xff #xff))
    ;(vector (iotab-sample iotab 1 0 0))
    ;(vector (iotab-sample iotab 1 #xff #xff))
    (for/vector #:length nsamples ;(- nsamples 4)
                ([i (in-range nsamples)]) ;(- nsamples 4))])
      (let ([a (random 256)] 
            [b (random 256)] 
            [sr (random 16)])
        (iotab-sample iotab sr a b)))))
