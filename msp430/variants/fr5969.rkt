#lang racket

(provide (all-defined-out))

(define ram-start #x1c00)
(define ram-size 2048)
(define fram-start #x4400)
(define fram-size 48000)
