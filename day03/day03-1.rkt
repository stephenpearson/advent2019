#!/usr/bin/env racket

#lang racket

(require "../common/read_files.rkt")

(define data
  (map (lambda (l) (string-split l #rx"[,\n]")) (read-file-lines "input")))

(define (trace-points x y wire cmpfn result)
  (if (> (length wire) 0)
    (let* ([mv (car wire)]
           [dir (string-ref mv 0)]
           [len (string->number (car (string-split mv #rx"^[UDLR]")))])
      (let-values ([(nx ny)
        (cond
          [(char=? dir #\U) (values x (+ y len))]
          [(char=? dir #\D) (values x (- y len))]
          [(char=? dir #\R) (values (+ x len) y)]
          [(char=? dir #\L) (values (- x len) y)])])
        (trace-points nx ny (cdr wire) cmpfn (cons (cmpfn x y nx ny) result))))
    result))

(define (between x a b)
  (and (> x (min a b)) (< x (max a b))))

(define (intersect x1 y1 nx1 ny1 x2 y2 nx2 ny2 vert1 vert2)
  (if (and vert1 (not vert2) (between x1 x2 nx2) (between y2 y1 ny1))
    (+ (abs x1) (abs y2))
    (if (and vert2 (not vert1) (between x2 x1 nx1) (between y1 y2 ny2))
      (+ (abs x2) (abs y1))
      #f)))

(apply min (filter identity (flatten (trace-points 0 0 (car data)
  (lambda (x y nx ny)
    (let ([vert1 (= x nx)])
      (trace-points 0 0 (cadr data)
        (lambda (x2 y2 nx2 ny2)
          (let ([vert2 (= x2 nx2)])
            (intersect x y nx ny x2 y2 nx2 ny2 vert1 vert2))) null))) null))))
