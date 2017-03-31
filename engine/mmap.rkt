#lang racket

(provide mmap)

(define mmap 
  (interface () mmap-ref mmap-set!))
