#lang racket

(provide io-diffs io-table/sr io-lookup/sr)

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
       (for/vector ([rn inputs]) (vector-ref in-regs rn))
       (for/vector ([rn outputs]) (vector-ref out-regs rn))))))

(define (empty-table sizes)
  (if (null? sizes)
      '#()
      (for/vector ([x (in-range (car sizes))])
        (empty-table (cdr sizes)))))

(define-syntax-rule (compress-sr x)
  (bitwise-ior (bitwise-and x 7) (bitwise-and (arithmetic-shift x -5) 8)))

(define (update-table/sr iotab i inputs outputs)
  (let ([input-idx (if (= i 0)
                       (compress-sr (vector-ref inputs i))
                       (vector-ref inputs i))])
    (if (< i (- (vector-length inputs) 1))
        (update-table/sr (vector-ref iotab input-idx) (+ i 1) inputs outputs)
        (vector-set! iotab input-idx outputs))))

(define (io-table/sr diffs input-sizes)
  (let ([iotab (empty-table input-sizes)])
    (for ([iopair diffs])
      (update-table/sr iotab 0 (car iopair) (cdr iopair)))
    iotab))

(define (io-lookup/sr iotab inputs #:i [i 0])
  (let ([input-idx (if (= i 0)
                       (compress-sr (vector-ref inputs i))
                       (vector-ref inputs i))])
    (if (< i (- (vector-length inputs) 1))
        (io-lookup/sr (vector-ref iotab input-idx) inputs #:i (+ i 1))
        (vector-ref iotab input-idx))))

(define (io-lookup iotab inputs #:i [i 0])
  (let ([input-idx (vector-ref inputs i)])
    (if (< i (- (vector-length inputs) 1))
        (io-lookup/sr (vector-ref iotab input-idx) inputs #:i (+ i 1))
        (vector-ref iotab input-idx))))
