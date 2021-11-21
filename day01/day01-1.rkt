#!/usr/bin/env racket

#lang racket

(require "../common/read_files.rkt")

(apply + (map (lambda (n) (- (quotient n 3) 2))
  (read-file-of-numbers "input")))
