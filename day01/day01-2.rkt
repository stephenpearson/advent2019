#!/usr/bin/env racket

#lang racket

(require "../common/read_files.rkt")

(define (fuel n)
  (let ([qty (- (quotient n 3) 2)])
    (if (< qty 0) 0 (+ qty (fuel qty)))))

(apply + (map fuel (read-file-of-numbers "input")))
