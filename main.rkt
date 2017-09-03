#lang racket
(require racket/cmdline)

(define data-path (make-parameter "data/"))
(define measure? (make-parameter #t))
(define synthesize? (make-parameter #t))
(define emulate? (make-parameter #t))

(if (measure?)
  (subprocess #f #f #f 
    (find-executable-path "racket"
      "meas/measure.rkt"
      "-d" (data-path))))

(if (synthesize?)
  (subprocess #f #f #f 
    (find-executable-path "racket"
      "synth/synthesize.rkt"
      "-d" (data-path)
      "-o" "emu/msp430/synthesized.rkt")))

(if (emulate?)
  (subprocess #f #f #f 
    (find-executable-path "racket"
      "emu/emulate.rkt"
      "-i")))
