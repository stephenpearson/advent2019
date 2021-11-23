#!/usr/bin/env racket

#lang racket

(require "../common/read_files.rkt")

(define data
  (map (lambda (l) (string-split l #rx"[,\n]")) (read-file-lines "input")))

(define (trace-points x y wire cmpfn result dst)
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
        (trace-points nx ny (cdr wire) cmpfn (cons (cmpfn x y nx ny dst) result) (+ dst len))))
    result))

(define (between x a b)
  (and (> x (min a b)) (< x (max a b))))

(define (intersect x1 y1 nx1 ny1 x2 y2 nx2 ny2 vert1 vert2 dst)
  ;(printf "x1=~a, y1=~a, nx1=~a, ny1=~a, x2=~a, y2=~a, nx2=~a, ny2=~a, vert1=~a, vert2=~a\n" x1 y1 nx1 ny1 x2 y2 nx2 ny2 vert1 vert2)
  (if (and vert1 (not vert2) (between x1 x2 nx2) (between y2 y1 ny1))
    (+ (abs (- y1 y2)) (abs (- x1 x2)) dst)
    (if (and vert2 (not vert1) (between x2 x1 nx1) (between y1 y2 ny2))
      (+ (abs (- y1 y2)) (abs (- x1 x2)) dst)
      #f)))

(apply min
  (filter identity
    (flatten
      (trace-points 0 0 (car data)
        (lambda (x y nx ny dst)
          (let ([vert1 (= x nx)])
            (trace-points 0 0 (cadr data)
              (lambda (x2 y2 nx2 ny2 dst)
                (let ([vert2 (= x2 nx2)])
                  (intersect x y nx ny x2 y2 nx2 ny2 vert1 vert2 dst)))
               null dst))) null 0))))
