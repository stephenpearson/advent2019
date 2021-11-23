#!/usr/bin/env racket

#lang racket

(define (digits n)
  (if (> n 0)
    (let-values ([(q r) (quotient/remainder n 10)])
      (append (digits q) (list r)))
    null))

(define (adjacent? digits prev)
  (if (> (length digits) 0)
    (or (= (car digits) prev) (adjacent? (cdr digits) (car digits)))
    #f))

(define (never-decreases? digits prev)
  (if (> (length digits) 0)
    (and (<= prev (car digits)) (never-decreases? (cdr digits) (car digits)))
    #t))

(define (valid? n)
  (let ([d (digits n)])
    (and
      (adjacent? (cdr d) (car d))
      (never-decreases? (cdr d) (car d)))))

(stream-length (stream-filter valid? (in-range 357253 892942)))
