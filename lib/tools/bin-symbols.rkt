#lang racket

(provide
 dump-msp430-symbols)

(define symtab-re
  #px"(?m:^([0-9A-Fa-f]+)\\s(.......)\\s(\\S+)\\s+([0-9A-Fa-f]+)\\s+(\\S+)$)")

;; i.e.
;; "lib/tools/bin-symbols.rkt"> ms
;; '("0000fffe l    d  __reset_vector 00000000 __reset_vector"
;;   "00001d20 l       .bss   00000000 _results"
;;   "00002320 l       .bss   00000000 _results_end")
;; "lib/tools/bin-symbols.rkt"> (regexp-match symtab-re (first ms))
;; '("0000fffe l    d  __reset_vector 00000000 __reset_vector"
;;   "0000fffe"
;;   "l    d "
;;   "__reset_vector"
;;   "00000000"
;;   "__reset_vector")
;; "lib/tools/bin-symbols.rkt"> (regexp-match symtab-re (last ms))
;; '("00002320 l       .bss   00000000 _results_end"
;;   "00002320"
;;   "l      "
;;   ".bss"
;;   "00000000"
;;   "_results_end")
(define (extract-msp430-symbol s)
  (let ([fields (regexp-match symtab-re s)])
    (values (string->symbol (last fields))
            (string->number (second fields) 16))))

(define (dump-msp430-symbols p)
  (unless (file-exists? p)
    (raise-argument-error 'dump-msp430-symbols "path to executable file" p))
  (define-values (sp sp-stdout sp-stdin sp-stderr)
    (subprocess #f #f #f (find-executable-path "msp430-elf-objdump") "-t" p))
  (close-output-port sp-stdin)
  (subprocess-wait sp)
  (define stdout-data (port->string sp-stdout))
  (define stderr-data (port->string sp-stderr))
  (close-input-port sp-stdout)
  (close-input-port sp-stderr)
  (for/hasheq ([s (regexp-match* symtab-re stdout-data)])
    (extract-msp430-symbol s)))
