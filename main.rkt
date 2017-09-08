#lang racket
(require racket/cmdline)

(define measure? (make-parameter #f))
(define synthesize? (make-parameter #f))
(define emulate? (make-parameter #f))

(define data-path (make-parameter "data/"))
(define output-file (make-parameter "emu/msp430/synthesized.rkt"))

(command-line
  #:once-each
  [("-m" "--measure") "If present, perform measurement using hardware connected over a debug interface."
		      (measure? #t)]
  [("-s" "--synthesize") "If present, perform synthesis using data files generated during measurement."
		      (synthesize? #t)]
  [("-e" "--emulate") "If present, launch the emulator at the end of synthesis."
		      (emulate? #t)]
  [("-d" "--data-path") d "Alternate path for data files generated during measurement (default: data/)"
		   (data-path d)]
  [("-o" "--output-file") o "Alternate path for synthesis results (default: emu/msp430/synthesized.rkt)"
		   (output-file o)])

(when (measure?)
  (let-values ([(sp o i e)
    (subprocess (current-output-port) (current-input-port) (current-error-port) 
      (find-executable-path "racket")
      "meas/measure.rkt"
      "-d" (data-path))])
    (subprocess-wait sp)))

(when (synthesize?)
  (when (file-exists? (output-file)) 
    (rename-file-or-directory 
      (output-file)
      (string-append (output-file) ".old")))
  (let-values ([(sp o i e)
    (subprocess (current-output-port) (current-input-port) (current-error-port) 
      (find-executable-path "racket")
      "synth/synthesize.rkt"
      "-d" (data-path)
      "-o" (output-file))])
    (subprocess-wait sp)))


(when (emulate?)
  (let-values ([(sp o i e)
    (subprocess (current-output-port) (current-input-port) (current-error-port) 
      (find-executable-path "racket")
      "emu/emulate.rkt"
      "-i")])
    (subprocess-wait sp)))
