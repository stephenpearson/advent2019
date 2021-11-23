#!/usr/bin/env racket

#lang racket

(define (digits n)
  (if (> n 0)
    (let-values ([(q r) (quotient/remainder n 10)])
      (append (digits q) (list r)))
    null))

(define (run-length digits prev count)
  (if (> (length digits) 0)
    (let ([count (if (= (car digits) prev) (+ count 1) 0)])
      (cons count (run-length (cdr digits) (car digits) count)))
    '(0)))

(define (adjacent? digits)
  (letrec ([contains-1-0? (lambda (l)
    (if (> (length l) 1)
      (or (and (= (car l) 1) (= (cadr l) 0)) (contains-1-0? (cdr l)))
      #f))])
    (contains-1-0? (run-length (cdr digits) (car digits) 0))))

(define (never-decreases? digits prev)
  (if (> (length digits) 0)
    (and (<= prev (car digits)) (never-decreases? (cdr digits) (car digits)))
    #t))

(define (valid? n)
  (let ([d (digits n)])
    (and (adjacent? d) (never-decreases? (cdr d) (car d)))))

(stream-length (stream-filter valid? (in-range 357253 892942)))
