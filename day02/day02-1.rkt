#!/usr/bin/env racket

#lang racket

(require "../common/read_files.rkt")

(define (run mem pc)
  (let ([v1 (vector-ref mem pc)])
    (if (= v1 99)
      (printf "~a\n" (vector-ref mem 0))
      (begin
        (let ([v2 (vector-ref mem (vector-ref mem (+ pc 1)))]
              [v3 (vector-ref mem (vector-ref mem (+ pc 2)))]
              [v4 (vector-ref mem (+ pc 3))])
          ;(printf "v1=~a\nv2=~a\nv3=~a\nv4=~a\n\n" v1 v2 v3 v4)
          (vector-set! mem v4 (cond
            [(= v1 1) (+ v2 v3)]
            [(= v1 2) (* v2 v3)]
            [else v4]))
          (run mem (+ pc 4)))))))

(define prg (list->vector (read-csv-of-numbers "input")))
(vector-set! prg 1 12)
(vector-set! prg 2 2)
(run prg 0)
